module pika;

import std::core::mem;

// ============================================================
// Grammar Construction
// ============================================================

// Rule indices (assigned during build)
struct LispRuleIndices {
    int ws;
    int integer;
    int float_num;
    int string;
    int symbol;
    int path;
    int dotdot;
    int dot;
    int lparen;
    int rparen;
    int lbracket;
    int rbracket;
    int lbrace;
    int rbrace;
    int quote;
    int caret;
    int atom;
    int expr;
    int expr_list;
    int list_form;
    int array_lit;
    int dict_entry;
    int dict_lit;
    int quote_expr;
    int type_annot;
    int postfix_index;
    int indexed_expr;
    int top;
}

fn void lisp_grammar_add_terminals(RuleBuilder* rb, LispRuleIndices* idx) {
    int ws_char = rb.add("ws_char", mk_scan(&scan_ws_char));
    idx.ws = rb.add("ws", mk_many(ws_char));

    idx.integer = rb.add("integer", mk_scan(&scan_int));
    idx.float_num = rb.add("float", mk_scan(&scan_float));
    idx.string = rb.add("string", mk_scan(&scan_string));
    idx.symbol = rb.add("symbol", mk_scan(&scan_symbol));
    idx.path = rb.add("path", mk_scan(&scan_path));
    idx.dotdot = rb.add("dotdot", mk_scan(&scan_dotdot));
    idx.dot = rb.add("dot", mk_token('.'));

    idx.lparen = rb.add("lparen", mk_token('('));
    idx.rparen = rb.add("rparen", mk_token(')'));
    idx.lbracket = rb.add("lbracket", mk_token('['));
    idx.rbracket = rb.add("rbracket", mk_token(']'));
    idx.lbrace = rb.add("lbrace", mk_token('{'));
    idx.rbrace = rb.add("rbrace", mk_token('}'));
    idx.quote = rb.add("quote_tok", mk_token('\''));
    idx.caret = rb.add("caret", mk_token('^'));

    idx.atom = rb.add("atom", mk_first(make_children_6(
        idx.float_num, idx.integer, idx.string, idx.path, idx.symbol, idx.dotdot
    )));
}

fn int lisp_grammar_add_recursive_rules(RuleBuilder* rb, LispRuleIndices* idx,
    int expr_placeholder, int expr_list_placeholder) {
    idx.quote_expr = rb.add("quote_expr", mk_seq(make_children_3(
        idx.quote, idx.ws, expr_placeholder
    )));

    idx.type_annot = rb.add("type_annot", mk_seq(make_children_5(
        idx.caret, idx.ws, idx.symbol, idx.ws, expr_placeholder
    )));

    idx.array_lit = rb.add("array_lit", mk_seq(make_children_5(
        idx.lbracket, idx.ws, expr_list_placeholder, idx.ws, idx.rbracket
    )));

    idx.dict_entry = rb.add("dict_entry", mk_seq(make_children_4(
        idx.ws, expr_placeholder, idx.ws, expr_placeholder
    )));
    int dict_entries = rb.add("dict_entries", mk_many(idx.dict_entry));
    idx.dict_lit = rb.add("dict_lit", mk_seq(make_children_5(
        idx.lbrace, idx.ws, dict_entries, idx.ws, idx.rbrace
    )));

    idx.list_form = rb.add("list_form", mk_seq(make_children_5(
        idx.lparen, idx.ws, expr_list_placeholder, idx.ws, idx.rparen
    )));

    idx.postfix_index = rb.add("postfix_index", mk_seq(make_children_6(
        idx.dot, idx.lbracket, idx.ws, expr_placeholder, idx.ws, idx.rbracket
    )));
    int indexed_suffix = rb.add("indexed_suffix", mk_many(idx.postfix_index));

    int base_expr = rb.add("base_expr", mk_first(make_children_6(
        idx.quote_expr, idx.type_annot, idx.list_form,
        idx.array_lit, idx.dict_lit, idx.atom
    )));

    idx.indexed_expr = rb.add("indexed_expr", mk_seq(make_children_2(
        base_expr, indexed_suffix
    )));
    idx.expr = rb.add("expr_full", mk_seq(make_children_2(idx.ws, idx.indexed_expr)));

    int expr_elem = rb.add("expr_elem", mk_seq(make_children_2(idx.ws, idx.indexed_expr)));
    idx.expr_list = rb.add("expr_list_full", mk_many(expr_elem));

    rb.rules[expr_placeholder].clause = mk_tie(idx.expr);
    rb.rules[expr_list_placeholder].clause = mk_tie(idx.expr_list);
    return idx.indexed_expr;
}

fn void lisp_grammar_add_top_rule(RuleBuilder* rb, LispRuleIndices* idx, int indexed_expr) {
    int end = rb.add("end", mk_end_of_input());
    idx.top = rb.add("top", mk_seq(make_children_4(idx.ws, indexed_expr, idx.ws, end)));
}

fn LispRuleIndices build_lisp_grammar(RuleBuilder* rb) {
    LispRuleIndices idx;
    lisp_grammar_add_terminals(rb, &idx);

    int expr_placeholder = rb.add("expr", mk_fail());
    int expr_list_placeholder = rb.add("expr_list", mk_fail());
    int indexed_expr = lisp_grammar_add_recursive_rules(
        rb, &idx, expr_placeholder, expr_list_placeholder
    );
    lisp_grammar_add_top_rule(rb, &idx, indexed_expr);
    return idx;
}

// Helper for 5 children
fn int[] make_children_5(int a, int b, int c, int d, int e) {
    int[] ch = mem::new_array(int, 5);
    ch[0] = a;
    ch[1] = b;
    ch[2] = c;
    ch[3] = d;
    ch[4] = e;
    return ch;
}

// Helper for 6 children
fn int[] make_children_6(int a, int b, int c, int d, int e, int f) {
    int[] ch = mem::new_array(int, 6);
    ch[0] = a;
    ch[1] = b;
    ch[2] = c;
    ch[3] = d;
    ch[4] = e;
    ch[5] = f;
    return ch;
}
