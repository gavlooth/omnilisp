module pika;

import lisp;

fn bool compile_clause_many_op(GrammarCompiler* gc, char[] op_name, lisp::Value* rest, int* out_idx) {
    if (!str_eq_c(op_name, "many") && !str_eq_c(op_name, "*")) return false;
    if (!lisp::is_cons(rest)) {
        *out_idx = gc.rb.add("_eps", mk_epsilon());
        return true;
    }
    int child = compile_clause(gc, rest.cons_val.car);
    *out_idx = compile_clause_named_many(gc, child);
    return true;
}

fn bool compile_clause_some_op(GrammarCompiler* gc, char[] op_name, lisp::Value* rest, int* out_idx) {
    if (!str_eq_c(op_name, "some") && !str_eq_c(op_name, "+")) return false;
    if (!lisp::is_cons(rest)) {
        *out_idx = gc.rb.add("_fail", mk_fail());
        return true;
    }
    int child = compile_clause(gc, rest.cons_val.car);
    *out_idx = compile_clause_named_some(gc, child);
    return true;
}

fn bool compile_clause_opt_op(GrammarCompiler* gc, char[] op_name, lisp::Value* rest, int* out_idx) {
    if (!str_eq_c(op_name, "opt") && !str_eq_c(op_name, "?")) return false;
    if (!lisp::is_cons(rest)) {
        *out_idx = gc.rb.add("_eps", mk_epsilon());
        return true;
    }
    int child = compile_clause(gc, rest.cons_val.car);
    *out_idx = compile_clause_named_opt(gc, child);
    return true;
}

fn bool compile_clause_scan_op(GrammarCompiler* gc, char[] op_name, lisp::Value* rest, int* out_idx) {
    if (!str_eq_c(op_name, "scan")) return false;
    if (!lisp::is_cons(rest) || !lisp::is_string(rest.cons_val.car)) {
        *out_idx = gc.rb.add("_fail", mk_fail());
        return true;
    }
    lisp::Value* pattern_val = rest.cons_val.car;
    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    if (pattern.len > 0 && pattern[0] == '[') {
        *out_idx = make_char_class_scanner(gc, pattern);
    } else {
        *out_idx = gc.rb.add("_scan_lit", mk_tokens(pattern));
    }
    return true;
}

fn bool compile_clause_not_op(GrammarCompiler* gc, char[] op_name, lisp::Value* rest, int* out_idx) {
    if (!str_eq_c(op_name, "not") && !str_eq_c(op_name, "!")) return false;
    if (!lisp::is_cons(rest)) {
        *out_idx = gc.rb.add("_eps", mk_epsilon());
        return true;
    }
    int child = compile_clause(gc, rest.cons_val.car);
    *out_idx = compile_clause_named_not(gc, child);
    return true;
}

fn bool compile_clause_and_op(GrammarCompiler* gc, char[] op_name, lisp::Value* rest, int* out_idx) {
    if (!str_eq_c(op_name, "and") && !str_eq_c(op_name, "&")) return false;
    if (!lisp::is_cons(rest)) {
        *out_idx = gc.rb.add("_eps", mk_epsilon());
        return true;
    }
    int child = compile_clause(gc, rest.cons_val.car);
    *out_idx = compile_clause_named_and(gc, child);
    return true;
}

fn bool compile_clause_end_op(GrammarCompiler* gc, char[] op_name, int* out_idx) {
    if (!str_eq_c(op_name, "end") && !str_eq_c(op_name, "$")) return false;
    *out_idx = gc.rb.add("_end", mk_end_of_input());
    return true;
}

fn bool compile_clause_unary_op(GrammarCompiler* gc, char[] op_name, lisp::Value* rest, int* out_idx) {
    if (compile_clause_many_op(gc, op_name, rest, out_idx)) return true;
    if (compile_clause_some_op(gc, op_name, rest, out_idx)) return true;
    if (compile_clause_opt_op(gc, op_name, rest, out_idx)) return true;
    if (compile_clause_scan_op(gc, op_name, rest, out_idx)) return true;
    if (compile_clause_not_op(gc, op_name, rest, out_idx)) return true;
    if (compile_clause_and_op(gc, op_name, rest, out_idx)) return true;
    return compile_clause_end_op(gc, op_name, out_idx);
}

fn int compile_clause_list_operator(GrammarCompiler* gc, char[] op_name, lisp::Value* rest) {
    if (str_eq_c(op_name, "seq")) return compile_clause_seq_op(gc, rest);
    if (str_eq_c(op_name, "first") || str_eq_c(op_name, "or") || str_eq_c(op_name, "|")) {
        return compile_clause_first_op(gc, rest);
    }

    int unary_idx = 0;
    if (compile_clause_unary_op(gc, op_name, rest, &unary_idx)) return unary_idx;

    gc_set_error(gc, "parser/grammar-unknown-operator", "pika/grammar: unknown grammar operator");
    return gc.rb.add("_err", mk_fail());
}
