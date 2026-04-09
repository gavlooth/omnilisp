module pika;

import std::core::mem;
import lisp;

// (pika/parse grammar-name input) -> parse result as nested list
fn lisp::Value* prim_pika_parse(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return parser_raise(interp, "parser/parse-expected-grammar-input", "pika/parse: expected grammar-name and input");
    }

    if (args[0].tag != lisp::ValueTag.SYMBOL) {
        return parser_raise(interp, "parser/parse-grammar-not-symbol", "pika/parse: first argument must be grammar name");
    }

    Grammar grammar;
    int start_clause = -1;
    if (!lookup_named_grammar(args[0].sym_val, &grammar, &start_clause)) {
        return parser_raise(interp, "parser/parse-grammar-not-found", "pika/parse: grammar not found");
    }

    if (!lisp::is_string(args[1])) {
        return parser_raise(interp, "parser/parse-input-not-string", "pika/parse: second argument must be input string");
    }

    char[] input = args[1].str_chars[:args[1].str_len];
    ParserState st = parse_simple(&grammar, input);
    int top_match = lookup_best_match_id(0, start_clause, &st);
    if (top_match < 0) return lisp::make_nil(interp);
    return parse_tree_to_lisp(&st, &grammar, top_match, input, interp);
}

fn lisp::Value* parse_tree_to_lisp(ParserState* st, Grammar* g, int match_idx, char[] input, lisp::Interp* interp) {
    if (match_idx < 0 || match_idx >= st.matches_len) {
        return lisp::make_nil(interp);
    }

    Match* m = &st.matches[match_idx];
    char[] rule_name = g.names[m.clause];
    lisp::SymbolId name_sym = interp.symbols.intern(rule_name);
    lisp::Value* name_val = lisp::make_symbol(interp, name_sym);

    char[] matched_text = "";
    if (m.last >= m.first && m.first >= 0 && m.last < (int)input.len) {
        matched_text = input[m.first..m.last];
    }
    lisp::Value* text_val = lisp::make_string(interp, matched_text);

    lisp::Value* children = lisp::make_nil(interp);
    UserMatch um = get_user_view(m.clause, match_idx, st);
    if (um.submatches.len > 0) {
        for (int i = (int)um.submatches.len - 1; i >= 0; i--) {
            lisp::Value* child = parse_tree_to_lisp(st, g, um.submatches[i], input, interp);
            children = lisp::make_cons(interp, child, children);
        }
        mem::free(um.submatches.ptr);
    }

    lisp::Value* result = lisp::make_cons(interp, children, lisp::make_nil(interp));
    result = lisp::make_cons(interp, text_val, result);
    result = lisp::make_cons(interp, name_val, result);
    return result;
}

// (pika/fold grammar-name input fold-fn) -> folded result
// fold-fn is called with (rule-name matched-text child-results...)
fn lisp::Value* prim_pika_fold(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 3) {
        return parser_raise(interp, "parser/fold-expected-grammar-input-fn", "pika/fold: expected grammar-name, input, and fold-fn");
    }

    if (args[0].tag != lisp::ValueTag.SYMBOL) {
        return parser_raise(interp, "parser/fold-grammar-not-symbol", "pika/fold: first argument must be grammar name");
    }

    Grammar grammar;
    int start_clause = -1;
    if (!lookup_named_grammar(args[0].sym_val, &grammar, &start_clause)) {
        return parser_raise(interp, "parser/fold-grammar-not-found", "pika/fold: grammar not found");
    }

    if (!lisp::is_string(args[1])) {
        return parser_raise(interp, "parser/fold-input-not-string", "pika/fold: second argument must be input string");
    }

    char[] input = args[1].str_chars[:args[1].str_len];
    lisp::Value* fold_fn = args[2];
    ParserState st = parse_simple(&grammar, input);
    int top_match = lookup_best_match_id(0, start_clause, &st);
    if (top_match < 0) return lisp::make_nil(interp);
    return fold_parse_tree(&st, &grammar, top_match, input, fold_fn, interp);
}

fn lisp::Value* fold_parse_tree(ParserState* st, Grammar* g, int match_idx, char[] input,
                                 lisp::Value* fold_fn, lisp::Interp* interp) {
    if (match_idx < 0 || match_idx >= st.matches_len) {
        return lisp::make_nil(interp);
    }

    Match* m = &st.matches[match_idx];
    char[] rule_name = g.names[m.clause];
    lisp::SymbolId name_sym = interp.symbols.intern(rule_name);
    lisp::Value* name_val = lisp::make_symbol(interp, name_sym);

    char[] matched_text = "";
    if (m.last >= m.first && m.first >= 0 && m.last < (int)input.len) {
        matched_text = input[m.first..m.last];
    }
    lisp::Value* text_val = lisp::make_string(interp, matched_text);

    lisp::Value* child_results = lisp::make_nil(interp);
    UserMatch um = get_user_view(m.clause, match_idx, st);
    if (um.submatches.len > 0) {
        for (int i = (int)um.submatches.len - 1; i >= 0; i--) {
            lisp::Value* child_result = fold_parse_tree(st, g, um.submatches[i], input, fold_fn, interp);
            child_results = lisp::make_cons(interp, child_result, child_results);
        }
        mem::free(um.submatches.ptr);
    }

    lisp::Value* r1 = lisp::jit_apply_value(fold_fn, name_val, interp);
    if (r1 != null && r1.tag == lisp::ValueTag.ERROR) return r1;

    lisp::Value* r2 = lisp::jit_apply_value(r1, text_val, interp);
    if (r2 != null && r2.tag == lisp::ValueTag.ERROR) return r2;

    lisp::Value* r3 = lisp::jit_apply_value(r2, child_results, interp);
    if (r3 != null && r3.tag == lisp::ValueTag.ERROR) return r3;
    return r3;
}
