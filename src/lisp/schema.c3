module lisp;

fn Value* prim_explain(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) {
        return raise_error(interp, "explain: expected (explain 'dispatch <form>) or (explain 'effect <form>)");
    }

    Value* selector = args[0];
    if (selector == null || selector.tag != SYMBOL) {
        return raise_error(interp, "explain: selector must be symbol ('dispatch or 'effect)");
    }

    SymbolId dispatch_sym = interp.symbols.intern("dispatch");
    SymbolId effect_sym = interp.sym_effect;
    if ((uint)selector.sym_val != (uint)dispatch_sym && (uint)selector.sym_val != (uint)effect_sym) {
        return raise_error(interp, "explain: unknown selector; expected 'dispatch or 'effect");
    }

    if ((uint)selector.sym_val == (uint)dispatch_sym) {
        return explain_dispatch_result_from_thunk(interp, args[1]);
    }

    return explain_effect_result_from_thunk(interp, args[1]);
}

fn Value* prim_schema_explain(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) {
        return raise_error_with_payload_names(
            interp,
            "schema",
            "schema/schema-explain-expected-schema-value",
            "schema-explain: expected (schema-explain schema value)"
        );
    }

    Value* schema = schema_resolve_named_symbol(args[0], interp);
    Value* val = args[1];

    if (schema_check(schema, val, interp)) {
        return make_nil(interp);
    }

    Value* msg = make_string(interp, "validation failed");
    return make_cons(interp, msg, make_nil(interp));
}
