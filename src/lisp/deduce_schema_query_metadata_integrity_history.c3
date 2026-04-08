module lisp;

fn Value* deduce_integrity_violation_class_value(
    DeduceIntegrityViolationClass violation_class,
    Interp* interp
) {
    if (interp == null) return null;
    switch (violation_class) {
        case DEDUCE_INTEGRITY_VIOLATION_KEY:
            return make_symbol(interp, interp.symbols.intern("key"));
        case DEDUCE_INTEGRITY_VIOLATION_UNIQUE:
            return make_symbol(interp, interp.symbols.intern("unique"));
        case DEDUCE_INTEGRITY_VIOLATION_REFERENCE:
            return make_symbol(interp, interp.symbols.intern("reference"));
        case DEDUCE_INTEGRITY_VIOLATION_CHECK:
            return make_symbol(interp, interp.symbols.intern("check"));
    }
}

fn Value* deduce_integrity_violation_history_payload(
    DeduceIntegrityViolationEntry[] history,
    usz count,
    Interp* interp
) {
    if (interp == null || history.ptr == null || count == 0) return make_nil(interp);

    Value* list = make_nil(interp);
    for (usz i = 0; i < count; i++) {
        DeduceIntegrityViolationEntry entry_data = history[i];
        Value* entry = make_hashmap(interp, 8);
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/integrity-history-out-of-memory",
                "deduce: integrity history payload out of memory");
        }
        explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("deduce-integrity-violation"));
        explain_dict_set(entry, interp, "sequence", make_int(interp, (long)entry_data.sequence));
        explain_dict_set(entry, interp, "relation",
            entry_data.relation != (SymbolId)0
                ? make_symbol(interp, entry_data.relation)
                : make_nil(interp));
        explain_dict_set(entry, interp, "code",
            entry_data.code != (SymbolId)0
                ? make_symbol(interp, entry_data.code)
                : make_nil(interp));
        explain_dict_set(entry, interp, "violation-class",
            deduce_integrity_violation_class_value(entry_data.violation_class, interp));
        list = make_cons(interp, entry, list);
    }
    return list;
}
