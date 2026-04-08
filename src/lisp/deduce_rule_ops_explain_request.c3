module lisp;

struct DeduceExplainRequest {
    isz      selected_rule_index;
    bool     use_seminaive_recursive;
    SymbolId requested_execution_engine_sym;
}

fn Value* deduce_explain_parse_request(
    DeduceDb* db,
    Value*[] args,
    DeduceExplainRequest* request,
    Interp* interp
) {
    if (db == null || request == null || interp == null) {
        return deduce_raise(interp, "deduce/explain-invalid-state",
            "deduce/explain: invalid request state");
    }

    request.selected_rule_index = (isz)db.rule_signature_count - 1;
    request.use_seminaive_recursive = true;
    request.requested_execution_engine_sym = interp.symbols.intern("semi-naive-scc");

    if (args.len >= 2) {
        Value* selector = args[1];
        if (selector != null && selector.tag == INT) {
            if (selector.int_val < 0 || (usz)selector.int_val >= db.rule_signature_count) {
                return deduce_raise(interp, "deduce/explain-rule-index-out-of-range",
                    "deduce/explain: rule index is out of range");
            }
            request.selected_rule_index = (isz)selector.int_val;
        } else if (selector != null && selector.tag == SYMBOL) {
            bool found = false;
            for (usz i = 0; i < db.rule_signature_count; i++) {
                if (db.rule_signatures[i].head_predicate == selector.sym_val) {
                    request.selected_rule_index = (isz)i;
                    found = true;
                    break;
                }
            }
            if (!found) {
                return deduce_raise(interp, "deduce/explain-rule-not-found",
                    "deduce/explain: no rule found for requested head predicate");
            }
        } else {
            return deduce_raise(interp, "deduce/explain-selector-not-symbol-or-int",
                "deduce/explain: selector must be a head symbol or rule index");
        }
    }

    if (args.len == 3) {
        Value* engine_arg = args[2];
        if (engine_arg == null || engine_arg.tag != SYMBOL) {
            return deduce_raise(interp, "deduce/explain-invalid-engine",
                "deduce/explain: engine must be symbol 'naive or 'semi-naive");
        }
        char[] engine_name = interp.symbols.get_name(engine_arg.sym_val);
        if (deduce_name_equals(engine_name, "naive") || deduce_name_equals(engine_name, "naive-scc")) {
            request.use_seminaive_recursive = false;
            request.requested_execution_engine_sym = interp.symbols.intern("naive-scc");
        } else if (deduce_name_equals(engine_name, "semi-naive") || deduce_name_equals(engine_name, "semi-naive-scc")) {
            request.use_seminaive_recursive = true;
            request.requested_execution_engine_sym = interp.symbols.intern("semi-naive-scc");
        } else {
            return deduce_raise(interp, "deduce/explain-invalid-engine",
                "deduce/explain: engine must be 'naive or 'semi-naive");
        }
    }
    return null;
}
