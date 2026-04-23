module lisp;

fn Value* prim_explain(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) {
        return runtime_raise(interp, "type/arg-mismatch", "explain: expected (explain 'dispatch <form>) or (explain 'effect <form>)");
    }

    Value* selector = args[0];
    if (selector == null || selector.tag != SYMBOL) {
        return runtime_raise(interp, "type/arg-mismatch", "explain: selector must be symbol ('dispatch or 'effect)");
    }

    SymbolId dispatch_sym = interp.symbols.intern("dispatch");
    SymbolId effect_sym = interp.sym_effect;
    if ((uint)selector.sym_val != (uint)dispatch_sym && (uint)selector.sym_val != (uint)effect_sym) {
        return runtime_raise(interp, "type/arg-mismatch", "explain: unknown selector; expected 'dispatch or 'effect");
    }

    if ((uint)selector.sym_val == (uint)dispatch_sym) {
        return explain_dispatch_result_from_thunk(interp, args[1]);
    }

    return explain_effect_result_from_thunk(interp, args[1]);
}
