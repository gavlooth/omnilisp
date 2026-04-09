module pika;

import std::core::mem;

// ============================================================
// Create and parse with Lisp grammar
// ============================================================

struct LispPikaParser {
    Grammar grammar;
    LispRuleIndices rules;
}

fn LispPikaParser create_lisp_parser() {
    RuleBuilder rb = rb_new();
    LispRuleIndices rules = build_lisp_grammar(&rb);

    String[] starts = mem::new_array(String, 1);
    starts[0] = "top";

    Grammar g = make_grammar(starts, rb.to_array());

    LispPikaParser parser;
    parser.grammar = g;
    parser.rules = rules;
    return parser;
}

fn ParserState pika_parse_lisp(LispPikaParser* parser, char[] input) {
    return parse_simple(&parser.grammar, input);
}

// Find the top-level match
fn int pika_find_top_match(LispPikaParser* parser, ParserState* st) {
    return find_match_at(st, "top", 0);
}
