module lisp;

import std::core::mem;
import std::io;
import main;

fn Value* make_process_wait_result(Interp* interp, int pid, int exit_code, int term_signal) {
    Value* out = make_hashmap_checked(interp, 8, "process-wait: out of memory");
    if (out == null || out.tag != HASHMAP) {
        return io_raise(interp, "io/process-wait-out-of-memory", "process-wait: out of memory");
    }
    SymbolId pid_sym = interp.symbols.intern("pid");
    SymbolId exit_sym = interp.symbols.intern("exit-code");
    SymbolId sig_sym = interp.symbols.intern("term-signal");
    if (pid_sym == INVALID_SYMBOL_ID || exit_sym == INVALID_SYMBOL_ID || sig_sym == INVALID_SYMBOL_ID) {
        return io_raise(interp, "io/process-wait-out-of-memory", "process-wait: failed to intern result keys");
    }
    Value* pid_key = make_symbol(interp, pid_sym);
    Value* exit_key = make_symbol(interp, exit_sym);
    Value* sig_key = make_symbol(interp, sig_sym);
    if (!hashmap_set_checked(out.hashmap_val, pid_key, make_int(interp, pid), interp) ||
        !hashmap_set_checked(out.hashmap_val, exit_key, make_int(interp, exit_code), interp) ||
        !hashmap_set_checked(out.hashmap_val, sig_key, make_int(interp, term_signal), interp)) {
        return io_raise(interp, "io/process-wait-out-of-memory", "process-wait: out of memory");
    }
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

    return make_process_wait_result(interp, pid, wait_ctx.exit_code, wait_ctx.term_signal);
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

    int sig = SIGKILL_VAL;
    if (sig_v.int_val > 0) {
        if (sig_v.int_val > (long)int.max) {
            return io_raise(interp, "io/process-kill-invalid-sig", "process-kill: sig is out of range");
        }
        sig = (int)sig_v.int_val;
    }
    int kill_status = omni_uv_process_kill(ph.uv_process, sig);
    if (kill_status < 0) {
        return io_raise(interp, "io/process-kill-failed", "process-kill: kill failed");
    }
    return make_void(interp);
}
