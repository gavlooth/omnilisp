module lisp;

// ============================================================
// Schema Validation — data-driven contracts via multiple dispatch
//
// Schemas are plain Omni data (lists + symbols). validate/explain
// walk the schema and check values. Dispatch on the schema tag symbol.
//
// Schema types:
//   'string, 'int, 'double, 'bool, 'any, 'nil
//   '(and s1 s2), '(or s1 s2), '(not s), '(maybe s)
//   '(> n), '(< n), '(>= n), '(<= n), '(= v)
//   '(re pattern)
//   '(map (name schema) ...), '(array-of schema), '(tuple s1 s2 ...)
//   '(enum v1 v2 ...)
// ============================================================

// ============================================================
// (validate schema-name value) → true or nil
// ============================================================

fn Value* prim_validate(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return runtime_raise(interp, "type/arg-mismatch", "validate: expected (validate schema value)");

    Value* schema = args[0];
    // If schema is a symbol, try looking up as a named schema first
    if (schema.tag == SYMBOL) {
        Value* named = interp.global_env.lookup(schema.sym_val);
        if (named != null && named.tag != NIL) {
            schema = named;  // Found named schema
        }
        // Otherwise use the symbol itself as the schema (bare type: 'int, 'string, etc.)
    }

    Value* val = args[1];

    if (schema_check(schema, val, interp)) {
        return interp.global_env.lookup(interp.symbols.intern("true"));
    }
    return make_nil(interp);
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
    if (msg == null || msg.tag == ERROR) return msg;
    return make_cons(interp, msg, make_nil(interp));
}
