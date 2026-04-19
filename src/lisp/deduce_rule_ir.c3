module lisp;

import std::core::mem;
import std::collections::list;

enum DeduceRuleTermKind {
    DEDUCE_RULE_TERM_LITERAL,
    DEDUCE_RULE_TERM_VARIABLE,
    DEDUCE_RULE_TERM_AGGREGATE,
}

enum DeduceAggregateOpKind {
    DEDUCE_AGGREGATE_NONE,
    DEDUCE_AGGREGATE_COUNT,
    DEDUCE_AGGREGATE_SUM,
    DEDUCE_AGGREGATE_MIN,
    DEDUCE_AGGREGATE_MAX,
}

struct DeduceRuleTerm {
    DeduceRuleTermKind   kind;
    DeduceAggregateOpKind aggregate_op;
    uint                 normalized_var_id;
    Value*               literal_value;
}

struct DeduceRuleAtom {
    SymbolId        predicate;
    bool            negated;
    DeduceRuleTerm* terms;
    usz             term_count;
}

struct DeduceRuleIr {
    DeduceRuleAtom  head;
    DeduceRuleAtom* body;
    usz             body_count;
    uint            max_var_id;
}

struct DeduceRuleVarBinding {
    SymbolId name;
    uint     id;
}

struct DeduceRuleGraphEdge {
    int  from;
    int  to;
    bool negated;
}

enum DeducePhysicalOpKind {
    DEDUCE_PHYSICAL_FULL_SCAN,
    DEDUCE_PHYSICAL_INDEX_SCAN,
    DEDUCE_PHYSICAL_NEGATED_SCAN,
    DEDUCE_PHYSICAL_NEGATED_INDEX_PROBE,
}

struct DeduceBodyPlanStep {
    usz                logical_body_index;
    ulong              bound_mask;
    int                selected_index;
    DeducePhysicalOpKind op_kind;
}

fn Value* deduce_rule_parse_ir(
    Value*[] args,
    DeduceRuleIr* out,
    Interp* interp
) {
    out.head = {};
    out.body = null;
    out.body_count = 0;
    out.max_var_id = 0;

    if (args.len < 2) {
        return deduce_raise(interp, "deduce/invalid-rule", "deduce-rule!: expected head atom and at least one body atom");
    }

    List{DeduceRuleVarBinding} vars = {};
    defer vars.free();

    Value* head_err = deduce_rule_parse_atom(args[0], false, true, &vars, &out.max_var_id, &out.head, interp);
    if (head_err != null) return head_err;
    if (out.head.negated) {
        return deduce_raise(interp, "deduce/invalid-rule", "deduce-rule!: head atom cannot be negated");
    }

    out.body_count = args.len - 1;
    if (out.body_count > usz.max / DeduceRuleAtom.sizeof) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    out.body = (DeduceRuleAtom*)mem::malloc(DeduceRuleAtom.sizeof * out.body_count);
    if (out.body == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }

    for (usz i = 0; i < out.body_count; i++) out.body[i] = {};
    for (usz i = 0; i < out.body_count; i++) {
        Value* body_err = deduce_rule_parse_atom(args[i + 1], true, false, &vars, &out.max_var_id, &out.body[i], interp);
        if (body_err != null) return body_err;
    }
    return null;
}
