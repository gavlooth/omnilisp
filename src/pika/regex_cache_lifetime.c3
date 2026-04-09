module pika;

import std::core::mem;

fn bool regex_generated_name_is_heap(String name) {
    if (name.len < 3) return false;
    if (name[0] == '_') return false;

    isz underscore = -1;
    for (usz i = 0; i < name.len; i++) {
        if (name[i] == '_') {
            underscore = (isz)i;
            break;
        }
    }
    if (underscore <= 0 || (usz)(underscore + 1) >= name.len) return false;

    for (usz i = (usz)(underscore + 1); i < name.len; i++) {
        char ch = name[i];
        if (ch < '0' || ch > '9') return false;
    }
    return true;
}

fn void regex_free_char_class_ctx(void* raw_ctx) {
    CharClassData* cc = (CharClassData*)raw_ctx;
    if (cc == null) return;
    if (cc.ranges_cap > 0) mem::free(cc.ranges.ptr);
    if (cc.chars_cap > 0) mem::free(cc.chars.ptr);
    mem::free(cc);
}

fn void regex_release_rule_builder(RuleBuilder* rb, bool transferred_to_grammar) {
    for (int i = 0; i < rb.len; i++) {
        Clause* clause = &rb.rules[i].clause;

        switch (clause.tag) {
            case SEQ:
            case FIRST:
                if (clause.children.len > 0) mem::free(clause.children.ptr);
            default:
                ;
        }

        if (transferred_to_grammar) continue;

        if (regex_generated_name_is_heap(rb.rules[i].name)) {
            mem::free(rb.rules[i].name.ptr);
        }

        if (clause.tag == SCAN_CTX && clause.scan_ctx != null) {
            regex_free_char_class_ctx(clause.scan_ctx);
            clause.scan_ctx = null;
        }
    }

    if (rb.cap > 0) mem::free(rb.rules.ptr);

    RuleEntry[] empty_rules;
    rb.rules = empty_rules;
    rb.len = 0;
    rb.cap = 0;
}

fn void regex_release_grammar_allocations(Grammar* grammar) {
    if (grammar.clauses.len > 0) {
        for (usz i = 0; i < grammar.clauses.len; i++) {
            Clause* clause = &grammar.clauses[i];
            switch (clause.tag) {
                case SEQ:
                case FIRST:
                    if (clause.children.len > 0) mem::free(clause.children.ptr);
                case SCAN_CTX:
                    if (clause.scan_ctx != null) {
                        regex_free_char_class_ctx(clause.scan_ctx);
                        clause.scan_ctx = null;
                    }
                default:
                    ;
            }
        }
    }

    if (grammar.seed_clauses.len > 0) {
        for (usz i = 0; i < grammar.seed_clauses.len; i++) {
            if (grammar.seed_clauses[i].len > 0) mem::free(grammar.seed_clauses[i].ptr);
        }
        mem::free(grammar.seed_clauses.ptr);
    }

    if (grammar.terminals.len > 0) mem::free(grammar.terminals.ptr);
    if (grammar.can_match_epsilon.len > 0) mem::free(grammar.can_match_epsilon.ptr);
    if (grammar.name_to_idx.len > 0) mem::free(grammar.name_to_idx.ptr);

    if (grammar.names.len > 0) {
        for (usz i = 0; i < grammar.names.len; i++) {
            if (regex_generated_name_is_heap(grammar.names[i])) {
                mem::free(grammar.names[i].ptr);
            }
        }
        mem::free(grammar.names.ptr);
    }

    if (grammar.clauses.len > 0) mem::free(grammar.clauses.ptr);

    String[] empty_names;
    int[] empty_name_to_idx;
    Clause[] empty_clauses;
    bool[] empty_eps;
    int[][] empty_seed;
    int[] empty_terminals;
    grammar.names = empty_names;
    grammar.name_to_idx = empty_name_to_idx;
    grammar.clauses = empty_clauses;
    grammar.can_match_epsilon = empty_eps;
    grammar.seed_clauses = empty_seed;
    grammar.terminals = empty_terminals;
    grammar.num_clauses = 0;
}
