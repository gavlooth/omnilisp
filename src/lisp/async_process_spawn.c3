module lisp;

import std::core::mem;
import std::io;
import main;

// (process-spawn cmd args env) -> dict {'handle process-handle 'pid n 'stdin fs-handle 'stdout fs-handle 'stderr fs-handle}
fn Value* prim_process_spawn(Value*[] args, Env* env, Interp* interp) {
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

    char** argv_ptrs = (char**)mem::malloc(char*.sizeof * (argv_item_count + 2));
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
        env_ptrs = (char**)mem::malloc(char*.sizeof * (env_item_count + 1));
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
        if (stdin_fd >= 0) c_close_fd(stdin_fd);
        if (stdout_fd >= 0) c_close_fd(stdout_fd);
        if (stderr_fd >= 0) c_close_fd(stderr_fd);
        char[96] err_buf;
        char[] err_msg;
        if (try char[] formatted = io::bprintf(&err_buf, "process-spawn: spawn failed (%d)", spawn_status)) {
            err_msg = formatted;
        } else {
            err_msg = "process-spawn: spawn failed";
        }
        return io_raise(interp, "io/process-spawn-failed", err_msg);
    }

    Value* proc_handle_v = make_process_handle(interp, uv_process);
    if (proc_handle_v == null || is_error(proc_handle_v)) {
        if (stdin_fd >= 0) c_close_fd(stdin_fd);
        if (stdout_fd >= 0) c_close_fd(stdout_fd);
        if (stderr_fd >= 0) c_close_fd(stderr_fd);
        return proc_handle_v != null ? proc_handle_v : io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }

    Value* stdin_h = make_fs_handle(interp, stdin_fd);
    if (stdin_h == null || is_error(stdin_h)) {
        ProcessHandle* ph = get_process_handle(proc_handle_v);
        close_process_handle(ph);
        if (stdout_fd >= 0) c_close_fd(stdout_fd);
        if (stderr_fd >= 0) c_close_fd(stderr_fd);
        return stdin_h != null ? stdin_h : io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    Value* stdout_h = make_fs_handle(interp, stdout_fd);
    if (stdout_h == null || is_error(stdout_h)) {
        close_fs_like_handle(stdin_h, interp);
        ProcessHandle* ph = get_process_handle(proc_handle_v);
        close_process_handle(ph);
        if (stderr_fd >= 0) c_close_fd(stderr_fd);
        return stdout_h != null ? stdout_h : io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    Value* stderr_h = make_fs_handle(interp, stderr_fd);
    if (stderr_h == null || is_error(stderr_h)) {
        close_fs_like_handle(stdin_h, interp);
        close_fs_like_handle(stdout_h, interp);
        ProcessHandle* ph = get_process_handle(proc_handle_v);
        close_process_handle(ph);
        return stderr_h != null ? stderr_h : io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
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
    Value* handle_key = make_symbol(interp, interp.symbols.intern("handle"));
    Value* pid_key = make_symbol(interp, interp.symbols.intern("pid"));
    Value* stdin_key = make_symbol(interp, interp.symbols.intern("stdin"));
    Value* stdout_key = make_symbol(interp, interp.symbols.intern("stdout"));
    Value* stderr_key = make_symbol(interp, interp.symbols.intern("stderr"));
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
