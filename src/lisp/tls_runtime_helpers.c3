module lisp;

import std::io;
import main;

fn Value* tls_raise_async(Interp* interp, char[] code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "io", code, message, null);
}

fn Value* tls_raise_fiber_required(Interp* interp, char[] op_name, char[] code) {
    char[112] ebuf;
    if (try char[] msg = io::bprintf(&ebuf, "%s: requires running fiber context", (ZString)op_name)) {
        return tls_raise_async(interp, code, msg);
    }
    return tls_raise_async(interp, code, "tls op: requires running fiber context");
}

fn Value* tls_raise_loop_unavailable(Interp* interp, char[] op_name, char[] code) {
    char[112] ebuf;
    if (try char[] msg = io::bprintf(&ebuf, "%s: libuv loop unavailable", (ZString)op_name)) {
        return tls_raise_async(interp, code, msg);
    }
    return tls_raise_async(interp, code, "tls op: libuv loop unavailable");
}

fn Value* tls_require_async_runtime(Interp* interp, char[] op_name, char[] fiber_code, char[] loop_code) {
    if (!scheduler_is_running_fiber_context()) {
        return tls_raise_fiber_required(interp, op_name, fiber_code);
    }
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        return tls_raise_loop_unavailable(interp, op_name, loop_code);
    }
    return null;
}

fn Value* tls_run_fiber_offload(OffloadWork* work, Interp* interp) {
    usz fiber_id = g_scheduler.current_fiber_id;
    Value* start_err = scheduler_begin_fiber_offload(work, fiber_id, interp);
    if (start_err != null) return start_err;

    Value*[1] yargs;
    yargs[0] = make_nil(interp);
    Value* y = prim_yield(yargs[..], null, interp);
    if (y != null && y.tag == ERROR) {
        scheduler_close_pending_offload(fiber_id);
        return y;
    }

    return scheduler_consume_pending_offload(fiber_id, interp);
}

fn bool tls_is_resume_policy_value(Value* v, Interp* interp) {
    return is_symbol(v) && (v.sym_val == interp.sym_true || v.sym_val == interp.sym_false);
}
