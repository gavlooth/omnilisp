module pika;

import std::thread;
import lisp;

// Global Lisp parser (lazy-initialized)
LispPikaParser g_lisp_parser;
bool g_lisp_parser_initialized = false;
thread::OnceFlag g_lisp_parser_once;

fn void init_lisp_parser_once() {
    g_lisp_parser = create_lisp_parser();
    g_lisp_parser_initialized = true;
}

fn void ensure_pika_parser() {
    g_lisp_parser_once.call(&init_lisp_parser_once);
}

fn bool pika_parse_lisp_source(char[] input, lisp::Interp* interp, ParserState* st, int* top_match) {
    ensure_pika_parser();
    *st = pika_parse_lisp(&g_lisp_parser, input);
    *top_match = pika_find_top_match(&g_lisp_parser, st);
    if (*top_match >= 0) return true;
    return lisp::parse(input, interp) == null;
}

fn lisp::Value* pika_eval_lisp_type_error(lisp::Interp* interp, char[] code, char[] message) @inline {
    return parser_raise(interp, code, message);
}

fn lisp::Value* pika_eval_lisp_grammar_drift(lisp::Interp* interp) @inline {
    return parser_raise(
        interp,
        "parser/eval-lisp-grammar-drift",
        "pika/eval-lisp: built-in grammar is ahead of Pika reader grammar"
    );
}

fn lisp::Value* prim_pika_parse_lisp(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 1) {
        return parser_raise(interp, "parser/parse-lisp-expected-input", "pika/parse-lisp: expected input string");
    }

    if (!lisp::is_string(args[0])) {
        return parser_raise(interp, "parser/parse-lisp-input-not-string", "pika/parse-lisp: argument must be string");
    }

    char[] input = args[0].str_chars[:args[0].str_len];

    ParserState st;
    int top_match = -1;

    if (!pika_parse_lisp_source(input, interp, &st, &top_match)) {
        if (top_match < 0) {
            return parser_raise(
                interp,
                "parser/parse-lisp-grammar-drift",
                "pika/parse-lisp: built-in grammar is behind reader syntax"
            );
        }
    }

    if (top_match < 0) return lisp::make_nil(interp);

    // Convert parse tree to Lisp value representation
    return parse_tree_to_lisp_value(&st, &g_lisp_parser.grammar, top_match, input, interp);
}

// Helper: convert parse tree to Lisp value (name text children...)
fn lisp::Value* parse_tree_to_lisp_value(
    ParserState* st,
    Grammar* g,
    int match_idx,
    char[] input,
    lisp::Interp* interp
) {
    if (match_idx < 0 || match_idx >= st.matches_len) {
        return lisp::make_nil(interp);
    }

    Match* m = &st.matches[match_idx];

    // Get rule name as symbol
    char[] rule_name = g.names[m.clause];
    lisp::SymbolId name_sym = interp.symbols.intern(rule_name);
    lisp::Value* name_val = lisp::make_symbol(interp, name_sym);

    // Get matched text
    char[] matched_text = "";
    if (m.last >= m.first && m.first >= 0 && m.last < (int)input.len) {
        matched_text = input[m.first..m.last];
    }
    lisp::Value* text_val = lisp::make_string(interp, matched_text);

    // Get children
    int[] subs = get_submatches(st, match_idx);
    lisp::Value* children = lisp::make_nil(interp);

    // Build children list (reverse order for cons)
    for (int i = (int)subs.len - 1; i >= 0; i--) {
        lisp::Value* child = parse_tree_to_lisp_value(st, g, subs[i], input, interp);
        children = lisp::make_cons(interp, child, children);
    }

    // Build (name text children...)
    lisp::Value* result = lisp::make_cons(interp, children, lisp::make_nil(interp));
    result = lisp::make_cons(interp, text_val, result);
    result = lisp::make_cons(interp, name_val, result);

    return result;
}

// (pika/grammar-rules grammar-name) -> list of rule names
fn lisp::Value* prim_pika_grammar_rules(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 1) {
        return parser_raise(interp, "parser/grammar-rules-expected-name", "pika/grammar-rules: expected grammar name");
    }

    if (args[0].tag != lisp::ValueTag.SYMBOL) {
        return parser_raise(interp, "parser/grammar-rules-name-not-symbol", "pika/grammar-rules: argument must be grammar name symbol");
    }

    Grammar grammar;
    int start_clause = -1;
    if (!lookup_named_grammar(args[0].sym_val, &grammar, &start_clause)) {
        return parser_raise(interp, "parser/grammar-rules-grammar-not-found", "pika/grammar-rules: grammar not found");
    }

    // Build list of rule names
    lisp::Value* result = lisp::make_nil(interp);
    for (int i = (int)grammar.names.len - 1; i >= 0; i--) {
        char[] name = grammar.names[i];
        lisp::SymbolId sym = interp.symbols.intern(name);
        lisp::Value* name_val = lisp::make_symbol(interp, sym);
        result = lisp::make_cons(interp, name_val, result);
    }

    return result;
}

// (pika/match-span grammar-name input rule-name) -> (start end) or nil
// Get the span (start/end positions) of the first match for a rule
fn lisp::Value* prim_pika_match_span(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 3) {
        return parser_raise(interp, "parser/match-span-expected-grammar-input-rule", "pika/match-span: expected grammar-name, input, and rule-name");
    }

    if (args[0].tag != lisp::ValueTag.SYMBOL) {
        return parser_raise(interp, "parser/match-span-grammar-not-symbol", "pika/match-span: first argument must be grammar name");
    }

    Grammar grammar;
    int start_clause = -1;
    if (!lookup_named_grammar(args[0].sym_val, &grammar, &start_clause)) {
        return parser_raise(interp, "parser/match-span-grammar-not-found", "pika/match-span: grammar not found");
    }

    if (!lisp::is_string(args[1])) {
        return parser_raise(interp, "parser/match-span-input-not-string", "pika/match-span: second argument must be input string");
    }

    if (args[2].tag != lisp::ValueTag.SYMBOL) {
        return parser_raise(interp, "parser/match-span-rule-not-symbol", "pika/match-span: third argument must be rule name symbol");
    }

    char[] input = args[1].str_chars[:args[1].str_len];
    char[] rule_name = interp.symbols.get_name(args[2].sym_val);

    // Parse
    ParserState st = parse_simple(&grammar, input);

    // Find match for the specified rule at position 0
    int match_idx = find_match_at(&st, (String)rule_name, 0);

    if (match_idx < 0) {
        return lisp::make_nil(interp);
    }

    Match* m = &st.matches[match_idx];
    lisp::Value* start = lisp::make_int(interp, m.first);
    lisp::Value* end = lisp::make_int(interp, m.last);
    lisp::Value* end_list = lisp::make_cons(interp, end, lisp::make_nil(interp));
    return lisp::make_cons(interp, start, end_list);
}

// (pika/eval-lisp input) -> evaluated result
// Parse with the Pika reader first, then evaluate the original source through
// the core runtime pipeline once grammar parity is confirmed.
fn lisp::Value* prim_pika_eval_lisp(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 1) {
        return pika_eval_lisp_type_error(interp, "parser/eval-lisp-expected-input", "pika/eval-lisp: expected input string");
    }

    if (!lisp::is_string(args[0])) {
        return pika_eval_lisp_type_error(interp, "parser/eval-lisp-input-not-string", "pika/eval-lisp: argument must be string");
    }

    char[] input = args[0].str_chars[:args[0].str_len];
    ParserState st;
    int top_match = -1;
    if (!pika_parse_lisp_source(input, interp, &st, &top_match)) {
        return pika_eval_lisp_grammar_drift(interp);
    }

    if (top_match < 0) {
        return parser_raise(interp, "parser/eval-lisp-invalid-input", "pika/eval-lisp: source did not parse");
    }

    lisp::EvalResult r = lisp::run(input, interp);
    if (r.error.has_error) {
        return parser_raise(
            interp,
            "parser/eval-lisp-failed",
            "pika/eval-lisp: parse/eval error"
        );
    }

    return r.value;
}
