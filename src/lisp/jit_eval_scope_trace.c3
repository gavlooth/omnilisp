module lisp;

import std::io;
import main;

fn int jit_trace_scope_generation(main::ScopeRegion* scope) @inline {
    return scope != null ? (int)scope.generation : -1;
}

fn int jit_trace_value_tag(Value* value) @inline {
    return value != null ? (int)value.tag : -1;
}

fn void jit_trace_single_after_eval(
    main::ScopeRegion* current,
    main::ScopeRegion* recycle,
    main::ScopeRegion* saved,
    main::ScopeRegion* call_scope,
    Value* result
) {
    io::printfn(
        "OMNI_TCO_SCOPE_TRACE stage=after-eval current_gen=%d recycle_gen=%d saved_gen=%d call_gen=%d result_tag=%d",
        jit_trace_scope_generation(current),
        jit_trace_scope_generation(recycle),
        jit_trace_scope_generation(saved),
        jit_trace_scope_generation(call_scope),
        jit_trace_value_tag(result)
    );
}

fn void jit_trace_after_pop_defer(String stage, main::ScopeRegion* active, main::ScopeRegion* saved, main::ScopeRegion* call_scope) {
    io::printfn(
        "OMNI_TCO_SCOPE_TRACE stage=%s active_gen=%d saved_gen=%d call_gen=%d",
        (ZString)stage.zstr_tcopy(),
        jit_trace_scope_generation(active),
        jit_trace_scope_generation(saved),
        jit_trace_scope_generation(call_scope)
    );
}

fn void jit_trace_after_leave(main::ScopeRegion* current, main::ScopeRegion* active, main::ScopeRegion* saved) {
    io::printfn(
        "OMNI_TCO_SCOPE_TRACE stage=after-leave current_gen=%d active_gen=%d saved_gen=%d",
        jit_trace_scope_generation(current),
        jit_trace_scope_generation(active),
        jit_trace_scope_generation(saved)
    );
}

fn void jit_trace_after_restore(main::ScopeRegion* current, main::ScopeRegion* active, main::ScopeRegion* saved) {
    io::printfn(
        "OMNI_TCO_SCOPE_TRACE stage=call-after-restore current_gen=%d active_gen=%d saved_gen=%d",
        jit_trace_scope_generation(current),
        jit_trace_scope_generation(active),
        jit_trace_scope_generation(saved)
    );
}

fn void jit_trace_after_finalize(String stage, main::ScopeRegion* current, Value* final) {
    io::printfn(
        "OMNI_TCO_SCOPE_TRACE stage=%s current_gen=%d final_tag=%d",
        (ZString)stage.zstr_tcopy(),
        jit_trace_scope_generation(current),
        jit_trace_value_tag(final)
    );
}

fn void jit_trace_call_after_eval(
    main::ScopeRegion* current,
    main::ScopeRegion* recycle,
    main::ScopeRegion* saved,
    main::ScopeRegion* call_scope,
    Value* result
) {
    if (result != null && result.tag == ERROR) {
        io::printfn(
            "OMNI_TCO_SCOPE_TRACE stage=call-after-eval current_gen=%d recycle_gen=%d saved_gen=%d call_gen=%d result_tag=%d error=%s",
            jit_trace_scope_generation(current),
            jit_trace_scope_generation(recycle),
            jit_trace_scope_generation(saved),
            jit_trace_scope_generation(call_scope),
            (int)result.tag,
            (ZString)result.str_chars
        );
        return;
    }
    io::printfn(
        "OMNI_TCO_SCOPE_TRACE stage=call-after-eval current_gen=%d recycle_gen=%d saved_gen=%d call_gen=%d result_tag=%d",
        jit_trace_scope_generation(current),
        jit_trace_scope_generation(recycle),
        jit_trace_scope_generation(saved),
        jit_trace_scope_generation(call_scope),
        jit_trace_value_tag(result)
    );
}
