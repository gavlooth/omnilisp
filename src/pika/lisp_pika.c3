module pika;

import std::core::mem;
import lisp;

// ============================================================
// Pika Lisp Primitives
//
// Exposes PikaParser functionality to Lisp:
//   - re-match, re-find-all, re-split, re-replace, re-fullmatch
//   - pika/grammar, pika/parse, pika/fold
// ============================================================

// ============================================================
// Regex Primitives
// ============================================================

struct NamedGrammar {
    lisp::SymbolId name;
    Grammar grammar;
    int start_clause;
    bool valid;
}

NamedGrammar* g_named_grammars = null;
int g_grammar_count = 0;
int g_grammar_capacity = 0;

fn bool ensure_named_grammar_capacity() {
    if (g_grammar_count < g_grammar_capacity) return true;
    int new_capacity = g_grammar_capacity < 16 ? 16 : g_grammar_capacity * 2;
    NamedGrammar* next = (NamedGrammar*)mem::malloc(NamedGrammar.sizeof * new_capacity);
    if (next == null) return false;
    for (int i = 0; i < g_grammar_count; i++) {
        next[i] = g_named_grammars[i];
    }
    if (g_named_grammars != null) mem::free(g_named_grammars);
    g_named_grammars = next;
    g_grammar_capacity = new_capacity;
    return true;
}

// Global Lisp parser (lazy-initialized)
LispPikaParser g_lisp_parser;
bool g_lisp_parser_initialized = false;

fn void ensure_pika_parser() {
    if (!g_lisp_parser_initialized) {
        g_lisp_parser = create_lisp_parser();
        g_lisp_parser_initialized = true;
    }
}

// Find a stored grammar by name
fn NamedGrammar* find_grammar(lisp::SymbolId name) {
    for (int i = 0; i < g_grammar_count; i++) {
        if ((uint)g_named_grammars[i].name == (uint)name && g_named_grammars[i].valid) {
            return &g_named_grammars[i];
        }
    }
    return null;
}

// (pika/parse grammar-name input) -> parse result as nested list
fn lisp::Value* prim_pika_parse(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return parser_raise(interp, "parser/parse-expected-grammar-input", "pika/parse: expected grammar-name and input");
    }

    // Get grammar
    if (args[0].tag != lisp::ValueTag.SYMBOL) {
        return parser_raise(interp, "parser/parse-grammar-not-symbol", "pika/parse: first argument must be grammar name");
    }

    NamedGrammar* ng = find_grammar(args[0].sym_val);
    if (ng == null) {
        return parser_raise(interp, "parser/parse-grammar-not-found", "pika/parse: grammar not found");
    }

    // Get input string
    if (!lisp::is_string(args[1])) {
        return parser_raise(interp, "parser/parse-input-not-string", "pika/parse: second argument must be input string");
    }

    char[] input = args[1].str_chars[:args[1].str_len];

    // Parse
    ParserState st = parse_simple(&ng.grammar, input);

    // Find top-level match
    int top_match = lookup_best_match_id(0, ng.start_clause, &st);

    if (top_match < 0) {
        return lisp::make_nil(interp);
    }

    // Convert parse tree to Lisp list
    return parse_tree_to_lisp(&st, &ng.grammar, top_match, input, interp);
}

// Convert a parse tree match to a Lisp value
fn lisp::Value* parse_tree_to_lisp(ParserState* st, Grammar* g, int match_idx, char[] input, lisp::Interp* interp) {
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
    lisp::Value* children = lisp::make_nil(interp);
    UserMatch um = get_user_view(m.clause, match_idx, st);
    if (um.submatches.len > 0) {
        // Build children list in natural order.
        for (int i = (int)um.submatches.len - 1; i >= 0; i--) {
            lisp::Value* child = parse_tree_to_lisp(st, g, um.submatches[i], input, interp);
            children = lisp::make_cons(interp, child, children);
        }
        mem::free(um.submatches.ptr);
    }

    // Build (name text children...)
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

    // Get grammar
    if (args[0].tag != lisp::ValueTag.SYMBOL) {
        return parser_raise(interp, "parser/fold-grammar-not-symbol", "pika/fold: first argument must be grammar name");
    }

    NamedGrammar* ng = find_grammar(args[0].sym_val);
    if (ng == null) {
        return parser_raise(interp, "parser/fold-grammar-not-found", "pika/fold: grammar not found");
    }

    // Get input string
    if (!lisp::is_string(args[1])) {
        return parser_raise(interp, "parser/fold-input-not-string", "pika/fold: second argument must be input string");
    }

    char[] input = args[1].str_chars[:args[1].str_len];

    // Get fold function
    lisp::Value* fold_fn = args[2];

    // Parse
    ParserState st = parse_simple(&ng.grammar, input);

    // Find top-level match
    int top_match = lookup_best_match_id(0, ng.start_clause, &st);

    if (top_match < 0) {
        return lisp::make_nil(interp);
    }

    // Fold over parse tree
    return fold_parse_tree(&st, &ng.grammar, top_match, input, fold_fn, interp);
}

// Fold over parse tree, calling fold_fn for each node
fn lisp::Value* fold_parse_tree(ParserState* st, Grammar* g, int match_idx, char[] input,
                                 lisp::Value* fold_fn, lisp::Interp* interp) {
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

    // Recursively fold children
    lisp::Value* child_results = lisp::make_nil(interp);
    UserMatch um = get_user_view(m.clause, match_idx, st);
    if (um.submatches.len > 0) {
        // Fold children and build results list in natural order.
        for (int i = (int)um.submatches.len - 1; i >= 0; i--) {
            lisp::Value* child_result = fold_parse_tree(st, g, um.submatches[i], input, fold_fn, interp);
            child_results = lisp::make_cons(interp, child_result, child_results);
        }
        mem::free(um.submatches.ptr);
    }

    // Call fold_fn with (rule-name text child-results)
    // Build argument list: (fold-fn rule-name text child-results)
    lisp::Value* call_args = lisp::make_cons(interp, child_results, lisp::make_nil(interp));
    call_args = lisp::make_cons(interp, text_val, call_args);
    call_args = lisp::make_cons(interp, name_val, call_args);

    // Apply fold_fn - need to curry since our apply takes one arg at a time
    lisp::Value* r1 = lisp::jit_apply_value(fold_fn, name_val, interp);
    if (r1 != null && r1.tag == lisp::ValueTag.ERROR) return r1;

    lisp::Value* r2 = lisp::jit_apply_value(r1, text_val, interp);
    if (r2 != null && r2.tag == lisp::ValueTag.ERROR) return r2;

    lisp::Value* r3 = lisp::jit_apply_value(r2, child_results, interp);
    if (r3 != null && r3.tag == lisp::ValueTag.ERROR) return r3;

    return r3;
}

// ============================================================
// Additional Regex Primitives
// ============================================================

// (re-match-pos pattern input) -> (start end) or nil
// Returns the start and end positions of the first match
fn lisp::Value* prim_re_match_pos(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return regex_raise(interp, "regex/re-match-pos-expected-pattern-input", "re-match-pos: expected (re-match-pos pattern input)");
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-match-pos-pattern-not-string", "re-match-pos: pattern must be a string");
    if (input_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-match-pos-input-not-string", "re-match-pos: input must be a string");

    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    char[] input = input_val.str_chars[:input_val.str_len];

    RegexMatch m = regex_search(pattern, input);
    if (lisp::Value* compile_err = regex_raise_invalid_pattern_if_any(
            interp,
            "regex/re-match-pos-invalid-pattern",
            "re-match-pos: invalid regex pattern")) {
        return compile_err;
    }

    if (m.matched) {
        // Return (start end) pair
        lisp::Value* start = lisp::make_int(interp, m.start);
        lisp::Value* end = lisp::make_int(interp, m.end);
        lisp::Value* end_list = lisp::make_cons(interp, end, lisp::make_nil(interp));
        return lisp::make_cons(interp, start, end_list);
    }

    return lisp::make_nil(interp);
}

// (re-find-all-pos pattern input) -> ((start1 end1) (start2 end2) ...)
// Returns list of (start end) pairs for all matches
fn lisp::Value* prim_re_find_all_pos(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 2) {
        return regex_raise(interp, "regex/re-find-all-pos-expected-pattern-input", "re-find-all-pos: expected (re-find-all-pos pattern input)");
    }

    lisp::Value* pattern_val = args[0];
    lisp::Value* input_val = args[1];

    if (pattern_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-find-all-pos-pattern-not-string", "re-find-all-pos: pattern must be a string");
    if (input_val.tag != lisp::ValueTag.STRING) return regex_raise(interp, "regex/re-find-all-pos-input-not-string", "re-find-all-pos: input must be a string");

    char[] pattern = pattern_val.str_chars[:pattern_val.str_len];
    char[] input = input_val.str_chars[:input_val.str_len];

    int count = 0;
    RegexMatch[] matches = regex_find_all(pattern, input, &count);
    defer if (matches.len > 0) mem::free(matches.ptr);
    if (lisp::Value* compile_err = regex_raise_invalid_pattern_if_any(
            interp,
            "regex/re-find-all-pos-invalid-pattern",
            "re-find-all-pos: invalid regex pattern")) {
        return compile_err;
    }

    // Build list of (start end) pairs
    lisp::Value* result = lisp::make_nil(interp);
    for (int i = count - 1; i >= 0; i--) {
        lisp::Value* start = lisp::make_int(interp, matches[i].start);
        lisp::Value* end = lisp::make_int(interp, matches[i].end);
        lisp::Value* end_list = lisp::make_cons(interp, end, lisp::make_nil(interp));
        lisp::Value* pair = lisp::make_cons(interp, start, end_list);
        result = lisp::make_cons(interp, pair, result);
    }

    return result;
}

// ============================================================
// Lisp Parsing Primitives
// ============================================================

// (pika/parse-lisp input) -> parsed AST as nested list
// Uses the built-in Lisp grammar to parse Lisp code
fn lisp::Value* prim_pika_parse_lisp(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    if (args.len < 1) {
        return parser_raise(interp, "parser/parse-lisp-expected-input", "pika/parse-lisp: expected input string");
    }

    if (!lisp::is_string(args[0])) {
        return parser_raise(interp, "parser/parse-lisp-input-not-string", "pika/parse-lisp: argument must be string");
    }

    char[] input = args[0].str_chars[:args[0].str_len];

    // Use the global Lisp parser
    ensure_pika_parser();
    ParserState st = pika_parse_lisp(&g_lisp_parser, input);
    int top_match = pika_find_top_match(&g_lisp_parser, &st);

    if (top_match < 0) {
        return lisp::make_nil(interp);
    }

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

    NamedGrammar* ng = find_grammar(args[0].sym_val);
    if (ng == null) {
        return parser_raise(interp, "parser/grammar-rules-grammar-not-found", "pika/grammar-rules: grammar not found");
    }

    // Build list of rule names
    lisp::Value* result = lisp::make_nil(interp);
    for (int i = (int)ng.grammar.names.len - 1; i >= 0; i--) {
        char[] name = ng.grammar.names[i];
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

    NamedGrammar* ng = find_grammar(args[0].sym_val);
    if (ng == null) {
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
    ParserState st = parse_simple(&ng.grammar, input);

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
// Parse and evaluate Lisp code (using Pika parser)
fn lisp::Value* prim_pika_eval_lisp(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    // Pika-based eval-lisp is deferred — lisp_semantics.c3 needs AST adaptation.
    // Use (eval (read-string s)) instead.
    return parser_raise(interp, "parser/eval-lisp-unavailable", "pika/eval-lisp not yet available — use (eval (read-string s)) instead");
}

// ============================================================
// Registration
// ============================================================

fn void register_pika_primitives(lisp::Interp* interp) {
    // Register regex primitives
    lisp::register_prim(interp, "re-match", &prim_re_match, 2);
    lisp::register_prim(interp, "re-fullmatch", &prim_re_fullmatch, 2);
    lisp::register_prim(interp, "re-find-all", &prim_re_find_all, 2);
    lisp::register_prim(interp, "re-split", &prim_re_split, 2);
    lisp::register_prim(interp, "re-replace", &prim_re_replace, -1);  // 3-4 args
    lisp::register_prim(interp, "re-match-pos", &prim_re_match_pos, 2);
    lisp::register_prim(interp, "re-find-all-pos", &prim_re_find_all_pos, 2);

    // Register grammar primitives
    lisp::register_prim(interp, "pika/grammar", &prim_pika_grammar, -1);  // variadic
    lisp::register_prim(interp, "pika/parse", &prim_pika_parse, 2);
    lisp::register_prim(interp, "pika/fold", &prim_pika_fold, 3);
    lisp::register_prim(interp, "pika/grammar-rules", &prim_pika_grammar_rules, 1);
    lisp::register_prim(interp, "pika/match-span", &prim_pika_match_span, 3);

    // Register Lisp parsing primitives
    lisp::register_prim(interp, "pika/parse-lisp", &prim_pika_parse_lisp, 1);
    lisp::register_prim(interp, "pika/eval-lisp", &prim_pika_eval_lisp, 1);
}
