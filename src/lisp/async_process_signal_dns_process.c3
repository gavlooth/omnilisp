module lisp;

import std::core::mem;
import std::io;
import main;

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
    if (!process_handle_try_begin_use(ph)) {
        return process_handle_busy_error(interp);
    }
    defer process_handle_end_use(ph);

    int pid = omni_uv_process_pid(ph.uv_process);

    ProcessWaitOffloadCtx wait_ctx = {
        .uv_process = ph.uv_process
    };
    Value* wait_result = scheduler_execute_custom_offload_job(
        &process_wait_offload_callback,
        &wait_ctx,
        interp
    );
    if (wait_result != null && wait_result.tag == ERROR) {
        return io_raise(interp, "io/process-wait-failed", "process-wait: wait failed");
    }
    if (wait_ctx.wait_status < 0) {
        return io_raise(interp, "io/process-wait-failed", "process-wait: wait failed");
    }

    close_process_handle(ph);

    Value* out = make_hashmap(interp, 8);
    Value* pid_key = make_symbol(interp, interp.symbols.intern("pid"));
    Value* exit_key = make_symbol(interp, interp.symbols.intern("exit-code"));
    Value* sig_key = make_symbol(interp, interp.symbols.intern("term-signal"));
    hashmap_set(out.hashmap_val, pid_key, make_int(interp, pid), interp);
    hashmap_set(out.hashmap_val, exit_key, make_int(interp, wait_ctx.exit_code), interp);
    hashmap_set(out.hashmap_val, sig_key, make_int(interp, wait_ctx.term_signal), interp);
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
    if (!process_handle_try_begin_use(ph)) {
        return process_handle_busy_error(interp);
    }
    defer process_handle_end_use(ph);

    int sig = (int)sig_v.int_val;
    if (sig <= 0) sig = SIGKILL_VAL;
    int kill_status = omni_uv_process_kill(ph.uv_process, sig);
    if (kill_status < 0) {
        return io_raise(interp, "io/process-kill-failed", "process-kill: kill failed");
    }
    return make_void(interp);
}
