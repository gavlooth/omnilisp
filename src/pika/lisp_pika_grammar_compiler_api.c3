module pika;

import std::core::mem;
import lisp;

fn bool parse_rule_definition(GrammarCompiler* gc, lisp::Value* rule_def, lisp::Interp* interp,
    lisp::SymbolId* rule_name, lisp::Value** clause_out) {
    if (!lisp::is_cons(rule_def)) {
        gc_set_error(gc, "parser/grammar-rule-not-list", "pika/grammar: rule must be a list");
        return false;
    }
    if (!lisp::is_list(rule_def)) {
        gc_set_error(gc, "parser/grammar-rule-not-list", "pika/grammar: rule definition must be a proper list");
        return false;
    }

    lisp::Value* head = rule_def.cons_val.car;
    if (!lisp::is_symbol(head)) {
        gc_set_error(gc, "parser/grammar-rule-head-not-symbol", "pika/grammar: rule definition must start with 'rule");
        return false;
    }

    char[] head_name = interp.symbols.get_name(head.sym_val);
    if (!str_eq_c(head_name, "rule")) {
        gc_set_error(gc, "parser/grammar-rule-head-not-rule", "pika/grammar: expected rule definition head 'rule");
        return false;
    }

    lisp::Value* rest = rule_def.cons_val.cdr;
    if (!lisp::is_cons(rest)) {
        gc_set_error(gc, "parser/grammar-rule-missing-name-clause", "pika/grammar: rule requires name and clause");
        return false;
    }

    lisp::Value* rule_name_val = rest.cons_val.car;
    if (!lisp::is_symbol(rule_name_val)) {
        gc_set_error(gc, "parser/grammar-rule-name-not-symbol", "pika/grammar: rule name must be a symbol");
        return false;
    }
    *rule_name = rule_name_val.sym_val;

    lisp::Value* clause_rest = rest.cons_val.cdr;
    if (!lisp::is_cons(clause_rest)) {
        gc_set_error(gc, "parser/grammar-rule-missing-clause", "pika/grammar: rule requires a clause");
        return false;
    }
    if (!lisp::is_nil(clause_rest.cons_val.cdr)) {
        gc_set_error(gc, "parser/grammar-rule-arity", "pika/grammar: rule expects exactly name and clause");
        return false;
    }
    *clause_out = clause_rest.cons_val.car;
    return true;
}

fn void gc_register_rule(GrammarCompiler* gc, lisp::Interp* interp, lisp::SymbolId rule_name, int clause_idx) {
    int existing_slot = gc.find_rule_slot(rule_name);
    if (existing_slot >= 0) {
        if (gc.rule_declared[existing_slot]) {
            gc_set_error(gc, "parser/grammar-duplicate-rule", "pika/grammar: duplicate rule name");
            return;
        }

        int existing = gc.rule_indices[existing_slot];
        gc.rb.rules[existing].clause = gc.rb.rules[clause_idx].clause;
        gc.rb.rules[clause_idx].clause = mk_fail();
        gc.rule_declared[existing_slot] = true;
        if (gc.start_rule_index < 0) gc.start_rule_index = existing;
        return;
    }
    char[] name_str = interp.symbols.get_name(rule_name);
    gc.rb.rules[clause_idx].name = (String)name_str;
    if (!gc_ensure_rule_capacity(gc)) return;
    gc.rule_names[gc.rule_count] = rule_name;
    gc.rule_indices[gc.rule_count] = clause_idx;
    gc.rule_declared[gc.rule_count] = true;
    gc.rule_count++;
    if (gc.start_rule_index < 0) gc.start_rule_index = clause_idx;
}

fn bool gc_has_unresolved_rule(GrammarCompiler* gc) {
    for (int i = 0; i < gc.rule_count; i++) {
        if (!gc.rule_declared[i]) return true;
    }
    return false;
}

fn lisp::Value* finalize_named_grammar(lisp::Interp* interp, lisp::SymbolId grammar_name, GrammarCompiler* gc, bool* builder_payloads_moved) {
    if (gc.rb.len == 0) {
        return parser_raise(interp, "parser/grammar-no-rules", "pika/grammar: grammar has no rules");
    }
    if (gc.start_rule_index < 0) {
        return parser_raise(interp, "parser/grammar-no-rules", "pika/grammar: grammar has no rules");
    }
    if (gc_has_unresolved_rule(gc)) {
        return parser_raise(interp, "parser/grammar-undefined-rule", "pika/grammar: unresolved rule reference");
    }
    if (named_grammar_exists(grammar_name)) {
        return parser_raise(interp, "parser/grammar-duplicate-name", "pika/grammar: grammar name is already defined");
    }

    String[] starts = mem::new_array(String, 1);
    defer mem::free(starts.ptr);
    starts[0] = gc.rb.rules[gc.start_rule_index].name;
    Grammar grammar = make_grammar(starts, gc.rb.to_array());
    *builder_payloads_moved = true;
    int start_clause = find_name_index(&grammar, starts[0]);
    if (start_clause < 0) {
        regex_release_grammar_allocations(&grammar);
        return parser_raise(interp, "parser/grammar-missing-start-rule", "pika/grammar: failed to resolve start rule");
    }

    bool duplicate_name = false;
    if (!store_named_grammar(grammar_name, grammar, start_clause, &duplicate_name)) {
        regex_release_grammar_allocations(&grammar);
        if (duplicate_name) {
            return parser_raise(interp, "parser/grammar-duplicate-name", "pika/grammar: grammar name is already defined");
        }
        return parser_raise(interp, "parser/grammar-oom", "pika/grammar: failed to grow grammar table");
    }
    return lisp::make_symbol(interp, grammar_name);
}

// (pika/grammar name rules...) -> grammar object
// Example: (pika/grammar expr
//            (rule number (scan "[0-9]+"))
//            (rule op (first "+" "-" "*" "/"))
//            (rule expr (first number (seq "(" expr op expr ")"))))
fn lisp::Value* prim_pika_grammar(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return parser_raise(interp, "parser/grammar-expected-name-rules", "pika/grammar: expected name and at least one rule");
    }

    // First arg is grammar name
    if (args[0].tag != lisp::ValueTag.SYMBOL) {
        return parser_raise(interp, "parser/grammar-name-not-symbol", "pika/grammar: name must be a symbol");
    }

    lisp::SymbolId grammar_name = args[0].sym_val;

    // Initialize compiler
    GrammarCompiler gc;
    gc.init(interp);
    bool grammar_transferred = false;
    defer regex_release_rule_builder(&gc.rb, grammar_transferred);
    defer if (gc.rule_names != null) mem::free(gc.rule_names);
    defer if (gc.rule_indices != null) mem::free(gc.rule_indices);
    defer if (gc.rule_declared != null) mem::free(gc.rule_declared);
    if (gc.has_error) {
        return parser_raise(interp, gc.error_code, gc.error_message);
    }

    // Process each (rule name clause) definition
    for (usz i = 1; i < args.len; i++) {
        lisp::Value* rule_def = args[i];
        lisp::SymbolId rule_name = (lisp::SymbolId)0;
        lisp::Value* clause = null;
        if (!parse_rule_definition(&gc, rule_def, interp, &rule_name, &clause)) {
            return parser_raise(interp, gc.error_code, gc.error_message);
        }

        // Compile the clause
        int clause_idx = compile_clause(&gc, clause);
        if (gc.has_error) return parser_raise(interp, gc.error_code, gc.error_message);
        gc_register_rule(&gc, interp, rule_name, clause_idx);
        if (gc.has_error) return parser_raise(interp, gc.error_code, gc.error_message);
    }
    return finalize_named_grammar(interp, grammar_name, &gc, &grammar_transferred);
}
