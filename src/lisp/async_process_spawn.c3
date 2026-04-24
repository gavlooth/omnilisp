module lisp;

import std::core::mem;
import std::io;
import main;

// (process-spawn cmd args env) -> dict {'handle process-handle 'pid n 'stdin fs-handle 'stdout fs-handle 'stderr fs-handle}
const int PROCESS_SPAWN_TRACE_STAGE_NONE = 0;
const int PROCESS_SPAWN_TRACE_STAGE_PROCESS_HANDLE = 1;
const int PROCESS_SPAWN_TRACE_STAGE_STDIN = 2;
const int PROCESS_SPAWN_TRACE_STAGE_STDOUT = 3;
const int PROCESS_SPAWN_TRACE_STAGE_STDERR = 4;

struct ProcessSpawnCleanupTrace {
    bool spawn_succeeded;
    bool process_handle_created;
    int failed_stage;
    bool stdin_wrapper_created;
    bool stdout_wrapper_created;
    bool stderr_wrapper_created;
    bool stdin_cleanup_called;
    bool stdout_cleanup_called;
    bool stderr_cleanup_called;
    bool process_closed;
}

fn void process_spawn_close_fd(int fd, bool* trace_flag) {
    if (fd < 0) return;
    c_close_fd(fd);
    if (trace_flag != null) *trace_flag = true;
}

fn void process_spawn_close_process(ProcessHandle* ph, ProcessSpawnCleanupTrace* trace) {
    if (ph == null) return;
    close_process_handle(ph);
    if (trace != null) {
        trace.process_closed = !ph.open && ph.uv_process == null;
    }
}

fn Value* process_spawn_impl(Value*[] args, Env* env, Interp* interp, ProcessSpawnCleanupTrace* trace = null) {
    if (trace != null) {
        *trace = {};
    }
    Value* cmd_v = null;
    Value* argv_v = make_nil(interp);
    Value* env_v = make_nil(interp);

    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        cmd_v = car(packed);
        packed = cdr(packed);
        if (is_cons(packed)) {
            argv_v = car(packed);
            packed = cdr(packed);
        }
        if (is_cons(packed)) {
            env_v = car(packed);
            packed = cdr(packed);
        }
        if (!is_nil(packed)) {
            return io_raise(interp, "io/process-spawn-expected-cmd-args-env", "process-spawn: expected (process-spawn cmd args env)");
        }
    } else {
        if (args.len < 1 || args.len > 3) {
            return io_raise(interp, "io/process-spawn-expected-cmd-args-env", "process-spawn: expected (process-spawn cmd args env)");
        }
        cmd_v = args[0];
        if (args.len >= 2) argv_v = args[1];
        if (args.len >= 3) env_v = args[2];
    }

    if (!is_string(cmd_v)) {
        return io_raise(interp, "io/process-spawn-cmd-not-string", "process-spawn: cmd must be a string");
    }
    if (c_api_string_contains_nul(cmd_v.str_chars[:cmd_v.str_len])) {
        return io_raise(interp, "io/process-spawn-invalid-cmd", "process-spawn: cmd contains embedded NUL byte");
    }

    char* cmd_heap = copy_to_heap_c_string(cmd_v.str_chars[:cmd_v.str_len]);
    if (cmd_heap == null) {
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    defer mem::free(cmd_heap);

    char** argv_items = null;
    usz argv_item_count = 0;
    Value* argv_err = collect_string_sequence(
        argv_v,
        &argv_items,
        &argv_item_count,
        interp,
        "io/process-spawn-args-must-be-string-sequence",
        "process-spawn: args must be nil, list, or array of strings/symbols"
    );
    if (argv_err != null) return argv_err;
    defer free_heap_c_string_items(argv_items, argv_item_count);

    bool has_env_override = env_v != null && !is_nil(env_v);
    char** env_items = null;
    usz env_item_count = 0;
    if (has_env_override) {
        Value* env_err = collect_string_sequence(
            env_v,
            &env_items,
            &env_item_count,
            interp,
            "io/process-spawn-env-must-be-string-sequence",
            "process-spawn: env must be nil, list, or array of strings/symbols"
        );
        if (env_err != null) return env_err;
    }
    defer free_heap_c_string_items(env_items, env_item_count);

    if (argv_item_count > usz.max - (usz)2) {
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    usz argv_ptr_count = argv_item_count + (usz)2;
    if (argv_ptr_count > usz.max / char*.sizeof) {
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    char** argv_ptrs = (char**)mem::malloc(char*.sizeof * argv_ptr_count);
    if (argv_ptrs == null) {
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    defer mem::free(argv_ptrs);
    argv_ptrs[0] = cmd_heap;
    for (usz i = 0; i < argv_item_count; i++) {
        argv_ptrs[i + 1] = argv_items[i];
    }
    argv_ptrs[argv_item_count + 1] = null;

    char** env_ptrs = null;
    if (has_env_override) {
        if (env_item_count == usz.max) {
            return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
        }
        usz env_ptr_count = env_item_count + (usz)1;
        if (env_ptr_count > usz.max / char*.sizeof) {
            return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
        }
        env_ptrs = (char**)mem::malloc(char*.sizeof * env_ptr_count);
        if (env_ptrs == null) {
            return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
        }
        for (usz i = 0; i < env_item_count; i++) {
            env_ptrs[i] = env_items[i];
        }
        env_ptrs[env_item_count] = null;
    }
    defer if (env_ptrs != null) mem::free(env_ptrs);

    void* uv_process = null;
    int stdin_fd = -1;
    int stdout_fd = -1;
    int stderr_fd = -1;
    int spawn_status = omni_uv_process_spawn(
        cmd_heap,
        (void*)argv_ptrs,
        (void*)env_ptrs,
        &uv_process,
        &stdin_fd,
        &stdout_fd,
        &stderr_fd
    );
    if (spawn_status < 0 || uv_process == null) {
        process_spawn_close_fd(stdin_fd, trace != null ? &trace.stdin_cleanup_called : null);
        process_spawn_close_fd(stdout_fd, trace != null ? &trace.stdout_cleanup_called : null);
        process_spawn_close_fd(stderr_fd, trace != null ? &trace.stderr_cleanup_called : null);
        char[96] err_buf;
        char[] err_msg;
        if (try char[] formatted = io::bprintf(&err_buf, "process-spawn: spawn failed (%d)", spawn_status)) {
            err_msg = formatted;
        } else {
            err_msg = "process-spawn: spawn failed";
        }
        return io_raise(interp, "io/process-spawn-failed", err_msg);
    }
    if (trace != null) {
        trace.spawn_succeeded = true;
    }

    Value* proc_handle_v = make_process_handle(interp, uv_process);
    if (proc_handle_v == null || is_error(proc_handle_v)) {
        process_spawn_close_fd(stdin_fd, trace != null ? &trace.stdin_cleanup_called : null);
        process_spawn_close_fd(stdout_fd, trace != null ? &trace.stdout_cleanup_called : null);
        process_spawn_close_fd(stderr_fd, trace != null ? &trace.stderr_cleanup_called : null);
        if (trace != null) {
            trace.failed_stage = PROCESS_SPAWN_TRACE_STAGE_PROCESS_HANDLE;
        }
        return proc_handle_v != null ? proc_handle_v : io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    if (trace != null) {
        trace.process_handle_created = true;
    }

    Value* stdin_h = make_fs_handle(interp, stdin_fd, trace != null ? &trace.stdin_cleanup_called : null);
    if (stdin_h == null || is_error(stdin_h)) {
        ProcessHandle* ph = get_process_handle(proc_handle_v);
        process_spawn_close_process(ph, trace);
        process_spawn_close_fd(stdout_fd, trace != null ? &trace.stdout_cleanup_called : null);
        process_spawn_close_fd(stderr_fd, trace != null ? &trace.stderr_cleanup_called : null);
        if (trace != null) {
            trace.failed_stage = PROCESS_SPAWN_TRACE_STAGE_STDIN;
        }
        return stdin_h != null ? stdin_h : io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    if (trace != null) {
        trace.stdin_wrapper_created = true;
    }
    Value* stdout_h = make_fs_handle(interp, stdout_fd, trace != null ? &trace.stdout_cleanup_called : null);
    if (stdout_h == null || is_error(stdout_h)) {
        close_fs_like_handle(stdin_h, interp);
        ProcessHandle* ph = get_process_handle(proc_handle_v);
        process_spawn_close_process(ph, trace);
        process_spawn_close_fd(stderr_fd, trace != null ? &trace.stderr_cleanup_called : null);
        if (trace != null) {
            trace.failed_stage = PROCESS_SPAWN_TRACE_STAGE_STDOUT;
            trace.stdin_cleanup_called = true;
        }
        return stdout_h != null ? stdout_h : io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    if (trace != null) {
        trace.stdout_wrapper_created = true;
    }
    Value* stderr_h = make_fs_handle(interp, stderr_fd, trace != null ? &trace.stderr_cleanup_called : null);
    if (stderr_h == null || is_error(stderr_h)) {
        close_fs_like_handle(stdin_h, interp);
        close_fs_like_handle(stdout_h, interp);
        ProcessHandle* ph = get_process_handle(proc_handle_v);
        process_spawn_close_process(ph, trace);
        if (trace != null) {
            trace.failed_stage = PROCESS_SPAWN_TRACE_STAGE_STDERR;
            trace.stdin_cleanup_called = true;
            trace.stdout_cleanup_called = true;
        }
        return stderr_h != null ? stderr_h : io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    if (trace != null) {
        trace.stderr_wrapper_created = true;
    }

    ProcessHandle* ph = get_process_handle(proc_handle_v);
    int pid = (ph != null && ph.open && ph.uv_process != null) ? omni_uv_process_pid(ph.uv_process) : -1;

    Value* out = make_hashmap_checked(interp, 16, "process-spawn: out of memory");
    if (out == null || out.tag != HASHMAP) {
        close_fs_like_handle(stdin_h, interp);
        close_fs_like_handle(stdout_h, interp);
        close_fs_like_handle(stderr_h, interp);
        if (ph != null) close_process_handle(ph);
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    SymbolId handle_sym = interp.symbols.intern("handle");
    SymbolId pid_sym = interp.symbols.intern("pid");
    SymbolId stdin_sym = interp.symbols.intern("stdin");
    SymbolId stdout_sym = interp.symbols.intern("stdout");
    SymbolId stderr_sym = interp.symbols.intern("stderr");
    if (handle_sym == INVALID_SYMBOL_ID ||
        pid_sym == INVALID_SYMBOL_ID ||
        stdin_sym == INVALID_SYMBOL_ID ||
        stdout_sym == INVALID_SYMBOL_ID ||
        stderr_sym == INVALID_SYMBOL_ID) {
        close_fs_like_handle(stdin_h, interp);
        close_fs_like_handle(stdout_h, interp);
        close_fs_like_handle(stderr_h, interp);
        if (ph != null) close_process_handle(ph);
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: failed to intern result keys");
    }
    Value* handle_key = make_symbol(interp, handle_sym);
    Value* pid_key = make_symbol(interp, pid_sym);
    Value* stdin_key = make_symbol(interp, stdin_sym);
    Value* stdout_key = make_symbol(interp, stdout_sym);
    Value* stderr_key = make_symbol(interp, stderr_sym);
    if (!hashmap_set_checked(out.hashmap_val, handle_key, proc_handle_v, interp) ||
        !hashmap_set_checked(out.hashmap_val, pid_key, make_int(interp, pid), interp) ||
        !hashmap_set_checked(out.hashmap_val, stdin_key, stdin_h, interp) ||
        !hashmap_set_checked(out.hashmap_val, stdout_key, stdout_h, interp) ||
        !hashmap_set_checked(out.hashmap_val, stderr_key, stderr_h, interp)) {
        close_fs_like_handle(stdin_h, interp);
        close_fs_like_handle(stdout_h, interp);
        close_fs_like_handle(stderr_h, interp);
        if (ph != null) close_process_handle(ph);
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    return out;
}

<*
@require interp != null
*>
fn Value* prim_process_spawn(Value*[] args, Env* env, Interp* interp) {
    return process_spawn_impl(args, env, interp);
}
