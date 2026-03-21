module lisp;

import std::core::mem;
import std::io;
import main;

fn void close_process_handle(ProcessHandle* ph) {
    if (ph == null || !ph.open || ph.uv_process == null) return;
    omni_uv_process_close(ph.uv_process);
    ph.uv_process = null;
    ph.open = false;
}

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
        char[] err_msg = io::bprintf(&err_buf, "process-spawn: spawn failed (%d)", spawn_status)!!;
        return io_raise(interp, "io/process-spawn-failed", err_msg);
    }

    Value* proc_handle_v = make_process_handle(interp, uv_process);
    if (proc_handle_v == null) {
        if (stdin_fd >= 0) c_close_fd(stdin_fd);
        if (stdout_fd >= 0) c_close_fd(stdout_fd);
        if (stderr_fd >= 0) c_close_fd(stderr_fd);
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }

    Value* stdin_h = make_fs_handle(interp, stdin_fd);
    if (stdin_h == null) {
        ProcessHandle* ph = get_process_handle(proc_handle_v);
        close_process_handle(ph);
        if (stdout_fd >= 0) c_close_fd(stdout_fd);
        if (stderr_fd >= 0) c_close_fd(stderr_fd);
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    Value* stdout_h = make_fs_handle(interp, stdout_fd);
    if (stdout_h == null) {
        close_fs_like_handle(stdin_h, interp);
        ProcessHandle* ph = get_process_handle(proc_handle_v);
        close_process_handle(ph);
        if (stderr_fd >= 0) c_close_fd(stderr_fd);
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    Value* stderr_h = make_fs_handle(interp, stderr_fd);
    if (stderr_h == null) {
        close_fs_like_handle(stdin_h, interp);
        close_fs_like_handle(stdout_h, interp);
        ProcessHandle* ph = get_process_handle(proc_handle_v);
        close_process_handle(ph);
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }

    ProcessHandle* ph = get_process_handle(proc_handle_v);
    int pid = (ph != null && ph.open && ph.uv_process != null) ? omni_uv_process_pid(ph.uv_process) : -1;

    Value* out = make_hashmap(interp, 16);
    Value* handle_key = make_symbol(interp, interp.symbols.intern("handle"));
    Value* pid_key = make_symbol(interp, interp.symbols.intern("pid"));
    Value* stdin_key = make_symbol(interp, interp.symbols.intern("stdin"));
    Value* stdout_key = make_symbol(interp, interp.symbols.intern("stdout"));
    Value* stderr_key = make_symbol(interp, interp.symbols.intern("stderr"));
    hashmap_set(out.hashmap_val, handle_key, proc_handle_v, interp);
    hashmap_set(out.hashmap_val, pid_key, make_int(interp, pid), interp);
    hashmap_set(out.hashmap_val, stdin_key, stdin_h, interp);
    hashmap_set(out.hashmap_val, stdout_key, stdout_h, interp);
    hashmap_set(out.hashmap_val, stderr_key, stderr_h, interp);
    return out;
}

// (process-wait process-handle|process-dict) -> dict {'pid n 'exit-code n 'term-signal n}
fn Value* prim_process_wait(Value*[] args, Env* env, Interp* interp) {
    Value* target_v = null;
    if (args.len == 1 && is_cons(args[0])) {
        target_v = car(args[0]);
    } else if (args.len >= 1) {
        target_v = args[0];
    } else {
        return io_raise(interp, "io/process-wait-expected-handle", "process-wait: expected (process-wait handle)");
    }

    Value* handle_v = value_to_process_handle_arg(target_v, interp);
    ProcessHandle* ph = get_process_handle(handle_v);
    if (ph == null || !ph.open || ph.uv_process == null) {
        return io_raise(interp, "io/process-wait-invalid-handle", "process-wait: invalid or closed process handle");
    }
    if (!scheduler_is_running_fiber_context() || !g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        return io_raise(interp, "io/process-wait-fiber-required", "process-wait: requires running fiber context");
    }

    int pid = omni_uv_process_pid(ph.uv_process);
    int exit_code = 0;
    int term_signal = 0;
    while (true) {
        int completed = 0;
        int poll_status = omni_uv_process_poll(
            ph.uv_process,
            &completed,
            &exit_code,
            &term_signal
        );
        if (poll_status < 0) {
            return io_raise(interp, "io/process-wait-failed", "process-wait: wait failed");
        }
        if (completed != 0) break;

        Value*[1] sleep_args;
        sleep_args[0] = make_int(interp, 1);
        Value* slept = prim_async_sleep(sleep_args[..], null, interp);
        if (slept != null && slept.tag == ERROR) return slept;
    }

    close_process_handle(ph);

    Value* out = make_hashmap(interp, 8);
    Value* pid_key = make_symbol(interp, interp.symbols.intern("pid"));
    Value* exit_key = make_symbol(interp, interp.symbols.intern("exit-code"));
    Value* sig_key = make_symbol(interp, interp.symbols.intern("term-signal"));
    hashmap_set(out.hashmap_val, pid_key, make_int(interp, pid), interp);
    hashmap_set(out.hashmap_val, exit_key, make_int(interp, exit_code), interp);
    hashmap_set(out.hashmap_val, sig_key, make_int(interp, term_signal), interp);
    return out;
}

// (process-kill process-handle|process-dict sig) -> void
fn Value* prim_process_kill(Value*[] args, Env* env, Interp* interp) {
    Value* target_v = null;
    Value* sig_v = null;
    if (args.len == 1 && is_cons(args[0])) {
        Value* packed = args[0];
        target_v = car(packed);
        packed = cdr(packed);
        if (!is_cons(packed)) {
            return io_raise(interp, "io/process-kill-expected-handle-sig", "process-kill: expected (process-kill handle sig)");
        }
        sig_v = car(packed);
    } else if (args.len >= 2) {
        target_v = args[0];
        sig_v = args[1];
    } else {
        return io_raise(interp, "io/process-kill-expected-handle-sig", "process-kill: expected (process-kill handle sig)");
    }

    if (!is_int(sig_v)) {
        return io_raise(interp, "io/process-kill-sig-not-integer", "process-kill: sig must be an integer");
    }

    Value* handle_v = value_to_process_handle_arg(target_v, interp);
    ProcessHandle* ph = get_process_handle(handle_v);
    if (ph == null || !ph.open || ph.uv_process == null) {
        return io_raise(interp, "io/process-kill-invalid-handle", "process-kill: invalid or closed process handle");
    }

    int sig = (int)sig_v.int_val;
    if (sig <= 0) sig = SIGKILL_VAL;
    int kill_status = omni_uv_process_kill(ph.uv_process, sig);
    if (kill_status < 0) {
        return io_raise(interp, "io/process-kill-failed", "process-kill: kill failed");
    }
    return make_void(interp);
}
