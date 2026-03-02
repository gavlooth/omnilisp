module pika;

import std::core::mem;

// ============================================================
// Lisp Grammar for PikaParser
// Matches current syntax: parentheses for lists, ^Type annotations,
// [x y z] for arrays, {:key val} for dicts, 'x for quote
// ============================================================

// ============================================================
// SCAN Functions for Token Matching
// Each returns 0 if no match, else the number of characters matched
// ============================================================

// Skip whitespace and comments - returns number of characters consumed
// Note: returns 0 if no whitespace, which the parser treats as "no match"
fn int scan_ws(char[] view) {
    int i = 0;
    while (i < (int)view.len) {
        char c = view[i];
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
            i++;
        } else if (c == ';') {
            // Skip comment to end of line
            while (i < (int)view.len && view[i] != '\n') {
                i++;
            }
        } else {
            break;
        }
    }
    return i;
}

// Scan exactly one whitespace character (for MANY wrapper)
fn int scan_ws_char(char[] view) {
    if (view.len == 0) return 0;
    char c = view[0];
    if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
        return 1;
    }
    if (c == ';') {
        // Skip entire comment as one "character"
        int i = 1;
        while (i < (int)view.len && view[i] != '\n') {
            i++;
        }
        return i;
    }
    return 0;
}

// Match integer: optional minus followed by digits
fn int scan_int(char[] view) {
    int i = 0;

    // Optional minus
    if (i < (int)view.len && view[i] == '-') {
        if (i + 1 >= (int)view.len || view[i + 1] < '0' || view[i + 1] > '9') {
            return 0; // Just '-' is not an integer
        }
        i++;
    }

    // Require at least one digit
    if (i >= (int)view.len || view[i] < '0' || view[i] > '9') {
        return 0;
    }

    while (i < (int)view.len && view[i] >= '0' && view[i] <= '9') {
        i++;
    }

    // Check not followed by decimal point with digit (that's a float)
    if (i < (int)view.len && view[i] == '.') {
        if (i + 1 < (int)view.len && view[i + 1] >= '0' && view[i + 1] <= '9') {
            return 0; // This is a float, not an int
        }
    }

    // Check not followed by 'e' or 'E' (that's a float)
    if (i < (int)view.len && (view[i] == 'e' || view[i] == 'E')) {
        return 0;
    }

    return i;
}

// Match float: int part, decimal point, fractional part, optional exponent
fn int scan_float(char[] view) {
    int i = 0;

    // Optional minus
    if (i < (int)view.len && view[i] == '-') {
        i++;
    }

    // Integer part (required)
    if (i >= (int)view.len || view[i] < '0' || view[i] > '9') {
        return 0;
    }
    while (i < (int)view.len && view[i] >= '0' && view[i] <= '9') {
        i++;
    }

    bool has_decimal = false;
    bool has_exp = false;

    // Decimal point and fractional part
    if (i < (int)view.len && view[i] == '.') {
        if (i + 1 < (int)view.len && view[i + 1] >= '0' && view[i + 1] <= '9') {
            has_decimal = true;
            i++; // consume '.'
            while (i < (int)view.len && view[i] >= '0' && view[i] <= '9') {
                i++;
            }
        }
    }

    // Exponent part (e.g., e-5, E10)
    if (i < (int)view.len && (view[i] == 'e' || view[i] == 'E')) {
        int exp_start = i;
        i++;
        if (i < (int)view.len && (view[i] == '+' || view[i] == '-')) {
            i++;
        }
        if (i < (int)view.len && view[i] >= '0' && view[i] <= '9') {
            has_exp = true;
            while (i < (int)view.len && view[i] >= '0' && view[i] <= '9') {
                i++;
            }
        } else {
            // Invalid exponent, rollback
            i = exp_start;
        }
    }

    // Must have either decimal or exponent to be a float
    if (!has_decimal && !has_exp) {
        return 0;
    }

    return i;
}

// Match string: "..." with escape sequences
fn int scan_string(char[] view) {
    if (view.len == 0 || view[0] != '"') return 0;

    int i = 1;
    while (i < (int)view.len) {
        char c = view[i];
        if (c == '"') {
            return i + 1; // Include closing quote
        }
        if (c == '\\' && i + 1 < (int)view.len) {
            i += 2; // Skip escape sequence
        } else {
            i++;
        }
    }

    return 0; // Unterminated string
}

// Helper: is this a symbol character?
fn bool is_sym_char(char c) {
    if (c >= 'a' && c <= 'z') return true;
    if (c >= 'A' && c <= 'Z') return true;
    if (c >= '0' && c <= '9') return true;
    if (c == '_' || c == '-' || c == '+' || c == '*' || c == '/' ||
        c == '=' || c == '<' || c == '>' || c == '!' || c == '?' ||
        c == ':' || c == '.' || c == '@' || c == '#' || c == '$' ||
        c == '%' || c == '&' || c == '|' || c == '~') {
        return true;
    }
    return false;
}

// Match symbol (not containing dots except as part of path)
fn int scan_symbol(char[] view) {
    if (view.len == 0) return 0;

    char c = view[0];
    // Symbol can't start with a digit, or with just '.'
    if (c >= '0' && c <= '9') return 0;
    if (!is_sym_char(c)) return 0;

    int i = 0;
    bool has_dot = false;
    while (i < (int)view.len && is_sym_char(view[i])) {
        if (view[i] == '.') has_dot = true;
        i++;
    }

    // If it has a dot, reject (that's a path)
    // Unless it's just ".." which is handled separately
    if (has_dot && i > 2) return 0;
    if (has_dot && i <= 2) {
        // Could be "." or ".." - let dotdot handle ".."
        if (i == 2 && view[0] == '.' && view[1] == '.') return 0;
    }

    return i;
}

// Match path: symbol.symbol.symbol
fn int scan_path(char[] view) {
    if (view.len == 0) return 0;

    char c = view[0];
    if (c >= '0' && c <= '9') return 0;
    if (!is_sym_char(c)) return 0;
    if (c == '.') return 0; // Path can't start with dot

    int i = 0;
    bool has_internal_dot = false;
    int last_dot = -1;

    while (i < (int)view.len && is_sym_char(view[i])) {
        if (view[i] == '.') {
            has_internal_dot = true;
            last_dot = i;
        }
        i++;
    }

    // Must have internal dot, not at start or end
    if (!has_internal_dot) return 0;
    if (last_dot == i - 1) return 0; // Ends with dot

    return i;
}

// Match dotdot: .. followed by whitespace/delimiter or EOF
fn int scan_dotdot(char[] view) {
    if (view.len < 2) return 0;
    if (view[0] != '.' || view[1] != '.') return 0;

    // Check what follows
    if (view.len == 2) return 2;

    char next = view[2];
    if (next == ' ' || next == '\t' || next == '\n' || next == '\r' ||
        next == ')' || next == ']' || next == '}') {
        return 2;
    }

    return 0;
}

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

fn LispRuleIndices build_lisp_grammar(RuleBuilder* rb) {
    LispRuleIndices idx;

    // Whitespace: MANY(ws_char) to make it epsilon-matchable
    int ws_char = rb.add("ws_char", mk_scan(&scan_ws_char));
    idx.ws = rb.add("ws", mk_many(ws_char));

    // Terminal tokens using SCAN
    idx.integer = rb.add("integer", mk_scan(&scan_int));
    idx.float_num = rb.add("float", mk_scan(&scan_float));
    idx.string = rb.add("string", mk_scan(&scan_string));
    idx.symbol = rb.add("symbol", mk_scan(&scan_symbol));
    idx.path = rb.add("path", mk_scan(&scan_path));
    idx.dotdot = rb.add("dotdot", mk_scan(&scan_dotdot));

    // Single character tokens
    idx.lparen = rb.add("lparen", mk_token('('));
    idx.rparen = rb.add("rparen", mk_token(')'));
    idx.lbracket = rb.add("lbracket", mk_token('['));
    idx.rbracket = rb.add("rbracket", mk_token(']'));
    idx.lbrace = rb.add("lbrace", mk_token('{'));
    idx.rbrace = rb.add("rbrace", mk_token('}'));
    idx.quote = rb.add("quote_tok", mk_token('\''));
    idx.caret = rb.add("caret", mk_token('^'));

    // atom: float | integer | string | path | symbol | dotdot
    // Note: float before integer (longer match first)
    idx.atom = rb.add("atom", mk_first(make_children_6(
        idx.float_num, idx.integer, idx.string, idx.path, idx.symbol, idx.dotdot
    )));

    // Forward declarations for recursive rules
    // We'll update these after adding all rules
    int expr_placeholder = rb.add("expr", mk_fail());
    int expr_list_placeholder = rb.add("expr_list", mk_fail());

    // quote_expr: quote ws? expr
    idx.quote_expr = rb.add("quote_expr", mk_seq(make_children_3(
        idx.quote, idx.ws, expr_placeholder
    )));

    // type_annot: caret ws? symbol ws? expr
    idx.type_annot = rb.add("type_annot", mk_seq(make_children_5(
        idx.caret, idx.ws, idx.symbol, idx.ws, expr_placeholder
    )));

    // array_lit: lbracket ws? expr_list ws? rbracket
    idx.array_lit = rb.add("array_lit", mk_seq(make_children_5(
        idx.lbracket, idx.ws, expr_list_placeholder, idx.ws, idx.rbracket
    )));

    // dict_entry: ws? expr ws? expr
    idx.dict_entry = rb.add("dict_entry", mk_seq(make_children_4(
        idx.ws, expr_placeholder, idx.ws, expr_placeholder
    )));

    // dict_entries: dict_entry*
    int dict_entries = rb.add("dict_entries", mk_many(idx.dict_entry));

    // dict_lit: lbrace ws? dict_entries ws? rbrace
    idx.dict_lit = rb.add("dict_lit", mk_seq(make_children_5(
        idx.lbrace, idx.ws, dict_entries, idx.ws, idx.rbrace
    )));

    // list_form: lparen ws? expr_list ws? rparen
    idx.list_form = rb.add("list_form", mk_seq(make_children_5(
        idx.lparen, idx.ws, expr_list_placeholder, idx.ws, idx.rparen
    )));

    // postfix_index: lbracket ws? expr ws? rbracket
    idx.postfix_index = rb.add("postfix_index", mk_seq(make_children_5(
        idx.lbracket, idx.ws, expr_placeholder, idx.ws, idx.rbracket
    )));

    // indexed_suffix: postfix_index*
    int indexed_suffix = rb.add("indexed_suffix", mk_many(idx.postfix_index));

    // base_expr: quote_expr | type_annot | list_form | array_lit | dict_lit | atom
    int base_expr = rb.add("base_expr", mk_first(make_children_6(
        idx.quote_expr, idx.type_annot, idx.list_form,
        idx.array_lit, idx.dict_lit, idx.atom
    )));

    // indexed_expr: base_expr indexed_suffix
    idx.indexed_expr = rb.add("indexed_expr", mk_seq(make_children_2(
        base_expr, indexed_suffix
    )));

    // Now update the forward declarations
    // expr: ws? indexed_expr
    idx.expr = rb.add("expr_full", mk_seq(make_children_2(idx.ws, idx.indexed_expr)));

    // expr_elem: ws? indexed_expr
    int expr_elem = rb.add("expr_elem", mk_seq(make_children_2(idx.ws, idx.indexed_expr)));

    // expr_list: expr_elem*
    idx.expr_list = rb.add("expr_list_full", mk_many(expr_elem));

    // Update the placeholder rules to reference the real rules
    rb.rules[expr_placeholder].clause = mk_tie(idx.expr);
    rb.rules[expr_list_placeholder].clause = mk_tie(idx.expr_list);

    // top: ws? expr ws? END
    int end = rb.add("end", mk_end_of_input());
    idx.top = rb.add("top", mk_seq(make_children_4(idx.ws, idx.indexed_expr, idx.ws, end)));

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
