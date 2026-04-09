module pika;

import std::core::mem;
import lisp;

fn int count_clause_elements(lisp::Value* rest) {
    int elem_count = 0;
    lisp::Value* curr = rest;
    while (lisp::is_cons(curr)) {
        elem_count++;
        curr = curr.cons_val.cdr;
    }
    return elem_count;
}

fn int[] compile_clause_children_from_list(GrammarCompiler* gc, lisp::Value* rest, int elem_count) {
    int[] children = mem::new_array(int, (usz)elem_count);
    lisp::Value* curr = rest;
    for (int i = 0; i < elem_count && lisp::is_cons(curr); i++) {
        children[i] = compile_clause(gc, curr.cons_val.car);
        curr = curr.cons_val.cdr;
    }
    return children;
}

fn int compile_clause_string(GrammarCompiler* gc, lisp::Value* clause) {
    char[] str = clause.str_chars[:clause.str_len];
    if (str.len == 1) return gc.rb.add("_tok", mk_token(str[0]));
    return gc.rb.add("_str", mk_tokens(str));
}

fn int compile_clause_symbol_ref(GrammarCompiler* gc, lisp::Value* clause) {
    lisp::SymbolId sym = clause.sym_val;
    int existing = gc.find_rule(sym);
    if (existing >= 0) return existing;

    char[] sym_name = gc.interp.symbols.get_name(sym);
    int placeholder = gc.rb.add((String)sym_name, mk_fail());
    if (!gc_ensure_rule_capacity(gc)) return placeholder;
    gc.rule_names[gc.rule_count] = sym;
    gc.rule_indices[gc.rule_count] = placeholder;
    gc.rule_count++;
    return placeholder;
}

fn int compile_clause_seq_op(GrammarCompiler* gc, lisp::Value* rest) {
    int elem_count = count_clause_elements(rest);
    if (elem_count == 0) return gc.rb.add("_eps", mk_epsilon());
    int[] children = compile_clause_children_from_list(gc, rest, elem_count);
    return gc.rb.add("_seq", mk_seq(children));
}

fn int compile_clause_first_op(GrammarCompiler* gc, lisp::Value* rest) {
    int elem_count = count_clause_elements(rest);
    if (elem_count == 0) return gc.rb.add("_fail", mk_fail());
    int[] children = compile_clause_children_from_list(gc, rest, elem_count);
    return gc.rb.add("_first", mk_first(children));
}

fn int compile_clause_named_many(GrammarCompiler* gc, int child) {
    return gc.rb.add("_many", mk_many(child));
}

fn int compile_clause_named_some(GrammarCompiler* gc, int child) {
    return gc.rb.add("_some", mk_some(child));
}

fn int compile_clause_named_opt(GrammarCompiler* gc, int child) {
    int eps = gc.rb.add("_eps", mk_epsilon());
    int[] children = make_children_2(child, eps);
    return gc.rb.add("_opt", mk_first(children));
}

fn int compile_clause_named_not(GrammarCompiler* gc, int child) {
    return gc.rb.add("_not", mk_not_followed_by(child));
}

fn int compile_clause_named_and(GrammarCompiler* gc, int child) {
    return gc.rb.add("_and", mk_followed_by(child));
}
