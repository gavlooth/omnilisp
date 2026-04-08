module lisp;

fn SymbolId deduce_explain_operator_symbol(Interp* interp, DeducePhysicalOpKind op_kind) {
    switch (op_kind) {
        case DEDUCE_PHYSICAL_FULL_SCAN:
            return interp.symbols.intern("full-scan");
        case DEDUCE_PHYSICAL_INDEX_SCAN:
            return interp.symbols.intern("index-scan");
        case DEDUCE_PHYSICAL_NEGATED_SCAN:
            return interp.symbols.intern("negated-scan");
        case DEDUCE_PHYSICAL_NEGATED_INDEX_PROBE:
            return interp.symbols.intern("negated-index-probe");
    }
}

fn SymbolId deduce_explain_aggregate_op_symbol(Interp* interp, DeduceAggregateOpKind op_kind) {
    switch (op_kind) {
        case DEDUCE_AGGREGATE_NONE:
            return interp.symbols.intern("none");
        case DEDUCE_AGGREGATE_COUNT:
            return interp.symbols.intern("count");
        case DEDUCE_AGGREGATE_SUM:
            return interp.symbols.intern("sum");
        case DEDUCE_AGGREGATE_MIN:
            return interp.symbols.intern("min");
        case DEDUCE_AGGREGATE_MAX:
            return interp.symbols.intern("max");
    }
}

fn SymbolId deduce_explain_head_projection_symbol(Interp* interp, DeduceRuleTermKind kind) {
    switch (kind) {
        case DEDUCE_RULE_TERM_LITERAL:
            return interp.symbols.intern("literal");
        case DEDUCE_RULE_TERM_VARIABLE:
            return interp.symbols.intern("group-key");
        case DEDUCE_RULE_TERM_AGGREGATE:
            return interp.symbols.intern("aggregate");
    }
}
