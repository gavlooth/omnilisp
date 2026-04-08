module lisp;

struct DeduceAnalyzeRequestedExecution {
    bool     use_seminaive_recursive;
    SymbolId execution_engine_sym;
    long     selector_rule_index;
}

fn void deduce_analyze_requested_execution_init(
    DeduceAnalyzeRequestedExecution* requested,
    Interp* interp
) {
    if (requested == null) return;
    *requested = {
        .use_seminaive_recursive = true,
        .execution_engine_sym = interp.symbols.intern("semi-naive-scc"),
        .selector_rule_index = -1,
    };
}

fn Value* deduce_analyze_parse_requested_execution(
    Value*[] args,
    DeduceAnalyzeRequestedExecution* requested,
    Interp* interp
) {
    if (requested == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state",
            "deduce/analyze: invalid requested execution state");
    }

    deduce_analyze_requested_execution_init(requested, interp);
    if (args.len < 2) return null;

    Value* selector_or_engine = args[1];
    if (selector_or_engine != null && selector_or_engine.tag == INT) {
        requested.selector_rule_index = selector_or_engine.int_val;
        if (args.len == 3) {
            Value* engine_arg = args[2];
            if (engine_arg == null || engine_arg.tag != SYMBOL) {
                return deduce_raise(interp, "deduce/analyze-invalid-engine",
                    "deduce/analyze: engine must be symbol 'naive or 'semi-naive");
            }
            char[] engine_name = interp.symbols.get_name(engine_arg.sym_val);
            if (deduce_name_equals(engine_name, "naive") || deduce_name_equals(engine_name, "naive-scc")) {
                requested.use_seminaive_recursive = false;
                requested.execution_engine_sym = interp.symbols.intern("naive-scc");
            } else if (deduce_name_equals(engine_name, "semi-naive") || deduce_name_equals(engine_name, "semi-naive-scc")) {
                requested.use_seminaive_recursive = true;
                requested.execution_engine_sym = interp.symbols.intern("semi-naive-scc");
            } else {
                return deduce_raise(interp, "deduce/analyze-invalid-engine",
                    "deduce/analyze: engine must be 'naive or 'semi-naive");
            }
        }
        return null;
    }

    if (args.len == 2 && selector_or_engine != null && selector_or_engine.tag == SYMBOL) {
        char[] engine_name = interp.symbols.get_name(selector_or_engine.sym_val);
        if (deduce_name_equals(engine_name, "naive") || deduce_name_equals(engine_name, "naive-scc")) {
            requested.use_seminaive_recursive = false;
            requested.execution_engine_sym = interp.symbols.intern("naive-scc");
        } else if (deduce_name_equals(engine_name, "semi-naive") || deduce_name_equals(engine_name, "semi-naive-scc")) {
            requested.use_seminaive_recursive = true;
            requested.execution_engine_sym = interp.symbols.intern("semi-naive-scc");
        } else {
            return deduce_raise(interp, "deduce/analyze-invalid-engine",
                "deduce/analyze: engine must be 'naive or 'semi-naive");
        }
        return null;
    }

    if (args.len == 3) {
        return deduce_raise(interp, "deduce/analyze-invalid-selector",
            "deduce/analyze: rule-index selector must be an integer");
    }

    return deduce_raise(interp, "deduce/analyze-invalid-engine",
        "deduce/analyze: engine must be symbol 'naive or 'semi-naive");
}
