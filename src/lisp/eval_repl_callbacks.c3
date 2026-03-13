module lisp;

// Replxx color constants
const CInt RXCOLOR_BLACK         = 0;
const CInt RXCOLOR_RED           = 1;
const CInt RXCOLOR_GREEN         = 2;
const CInt RXCOLOR_BROWN         = 3;
const CInt RXCOLOR_BLUE          = 4;
const CInt RXCOLOR_MAGENTA       = 5;
const CInt RXCOLOR_CYAN          = 6;
const CInt RXCOLOR_LIGHTGRAY     = 7;
const CInt RXCOLOR_GRAY          = 8;
const CInt RXCOLOR_BRIGHTRED     = 9;
const CInt RXCOLOR_BRIGHTGREEN   = 10;
const CInt RXCOLOR_YELLOW        = 11;
const CInt RXCOLOR_BRIGHTBLUE    = 12;
const CInt RXCOLOR_BRIGHTMAGENTA = 13;
const CInt RXCOLOR_BRIGHTCYAN    = 14;
const CInt RXCOLOR_WHITE         = 15;
const CInt RXCOLOR_DEFAULT       = 1 << 16;
const CInt RXCOLOR_BOLD          = 1 << 17;

// Global replxx handle for use in callbacks
void* g_replxx = null;
// Global interp for completion callback
Interp* g_repl_interp = null;

// =============================================================================
// REPLXX CALLBACKS — Syntax Highlighting + Completion
// =============================================================================

// Check if a symbol name is a keyword (special form)
fn bool is_keyword(char[] name) {
    // Special forms
    char[][] keywords = {
        "lambda", "define", "let", "if", "block", "set!", "quote",
        "and", "or", "match", "checkpoint", "capture", "signal", "handle",
        "resolve", "explain", "module", "import", "export", "with-continuation"
    };
    foreach (kw : keywords) {
        if (kw.len == name.len) {
            bool eq = true;
            for (usz i = 0; i < kw.len; i++) {
                if (kw[i] != name[i]) { eq = false; break; }
            }
            if (eq) return true;
        }
    }
    return false;
}

// Check if a symbol is a builtin constant
fn bool is_builtin_const(char[] name) {
    char[][] builtins = { "true", "false", "nil", "pi", "e" };
    foreach (b : builtins) {
        if (b.len == name.len) {
            bool eq = true;
            for (usz i = 0; i < b.len; i++) {
                if (b[i] != name[i]) { eq = false; break; }
            }
            if (eq) return true;
        }
    }
    return false;
}

// Helper: classify a symbol name and apply color to the range [start, end) in colors array
fn void flush_symbol(char* input, CInt* colors, isz start, CInt end_pos) {
    if (start < 0 || start >= (isz)end_pos) return;
    usz slen = (usz)end_pos - (usz)start;
    char[128] sbuf;
    if (slen > 127) slen = 127;
    for (usz k = 0; k < slen; k++) sbuf[k] = input[(usz)start + k];
    char[] sym = sbuf[:slen];

    CInt col = RXCOLOR_DEFAULT;
    if (is_keyword(sym)) {
        col = RXCOLOR_BRIGHTCYAN | RXCOLOR_BOLD;
    } else if (is_builtin_const(sym)) {
        col = RXCOLOR_BRIGHTMAGENTA;
    }
    for (isz j = start; j < (isz)end_pos; j++) {
        colors[(CInt)j] = col;
    }
}

struct ReplHighlighterState {
    bool in_string;
    bool in_comment;
    bool escape_next;
    int paren_depth;
    isz sym_start;
}

const CInt[6] REPL_PAREN_COLORS = {
    RXCOLOR_BRIGHTCYAN, RXCOLOR_BRIGHTMAGENTA, RXCOLOR_YELLOW,
    RXCOLOR_BRIGHTGREEN, RXCOLOR_BRIGHTBLUE, RXCOLOR_BRIGHTRED
};

fn bool repl_highlighter_is_whitespace(char c) @inline {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

fn bool repl_highlighter_is_quote_prefix(char* input, CInt i) @inline {
    return i == 0 || input[i - 1] == '(' || input[i - 1] == ' ' || input[i - 1] == '\t';
}

fn void repl_highlighter_flush_symbol(char* input, CInt* colors, ReplHighlighterState* state, CInt pos) @inline {
    flush_symbol(input, colors, state.sym_start, pos);
    state.sym_start = -1;
}

fn bool repl_highlighter_step_comment(char c, CInt* colors, CInt i, ReplHighlighterState* state) @inline {
    if (!state.in_comment) return false;
    colors[i] = RXCOLOR_GRAY;
    if (c == '\n') {
        state.in_comment = false;
    }
    return true;
}

fn bool repl_highlighter_step_string(char c, CInt* colors, CInt i, ReplHighlighterState* state) @inline {
    if (!state.in_string) return false;
    colors[i] = RXCOLOR_GREEN;
    if (state.escape_next) {
        state.escape_next = false;
        return true;
    }
    if (c == '\\') {
        state.escape_next = true;
        return true;
    }
    if (c == '"') {
        state.in_string = false;
    }
    return true;
}

fn void repl_highlighter_color_paren(CInt* colors, CInt i, char c, ReplHighlighterState* state) @inline {
    if (c == '(') {
        colors[i] = REPL_PAREN_COLORS[state.paren_depth % 6];
        state.paren_depth++;
        return;
    }
    state.paren_depth--;
    if (state.paren_depth < 0) {
        state.paren_depth = 0;
    }
    colors[i] = REPL_PAREN_COLORS[state.paren_depth % 6];
}

fn void repl_highlighter_step_normal(
    char* input,
    CInt* colors,
    CInt i,
    char c,
    ReplHighlighterState* state
) {
    if (c == ';') {
        repl_highlighter_flush_symbol(input, colors, state, i);
        state.in_comment = true;
        colors[i] = RXCOLOR_GRAY;
        return;
    }
    if (c == '"') {
        repl_highlighter_flush_symbol(input, colors, state, i);
        state.in_string = true;
        colors[i] = RXCOLOR_GREEN;
        return;
    }
    if (c == '(' || c == ')') {
        repl_highlighter_flush_symbol(input, colors, state, i);
        repl_highlighter_color_paren(colors, i, c, state);
        return;
    }
    if (c == '[' || c == ']') {
        repl_highlighter_flush_symbol(input, colors, state, i);
        colors[i] = RXCOLOR_YELLOW;
        return;
    }
    if (c == '{' || c == '}') {
        repl_highlighter_flush_symbol(input, colors, state, i);
        colors[i] = RXCOLOR_BROWN;
        return;
    }
    if (c == '\'' && repl_highlighter_is_quote_prefix(input, i)) {
        colors[i] = RXCOLOR_CYAN;
        return;
    }
    if (c >= '0' && c <= '9' && state.sym_start < 0) {
        colors[i] = RXCOLOR_BRIGHTMAGENTA;
        return;
    }
    if (repl_highlighter_is_whitespace(c)) {
        repl_highlighter_flush_symbol(input, colors, state, i);
        return;
    }
    if (state.sym_start < 0) {
        state.sym_start = (isz)i;
    }
}

// Syntax highlighter callback for replxx — colors individual code points
fn void lisp_highlighter(char* input, CInt* colors, CInt size, void* ud) {
    if (input == null || size <= 0) return;

    ReplHighlighterState state = {
        .in_string = false,
        .in_comment = false,
        .escape_next = false,
        .paren_depth = 0,
        .sym_start = -1,
    };

    for (CInt i = 0; i < size; i++) {
        char c = input[i];
        if (repl_highlighter_step_comment(c, colors, i, &state)) continue;
        if (repl_highlighter_step_string(c, colors, i, &state)) continue;
        repl_highlighter_step_normal(input, colors, i, c, &state);
    }

    // Flush trailing symbol
    flush_symbol(input, colors, state.sym_start, size);
}

fn usz repl_cstr_len_ptr(char* s) @inline {
    usz len = 0;
    while (s[len] != 0) {
        len++;
    }
    return len;
}

fn bool repl_is_completion_word_break(char c) @inline {
    return c == ' ' || c == '\t' || c == '(' || c == ')' ||
        c == '[' || c == ']' || c == '{' || c == '}' ||
        c == '\'' || c == '"';
}

fn isz repl_completion_find_word_start(char* input, usz len) {
    if (len == 0) return 0;
    isz word_start = (isz)len - 1;
    while (word_start >= 0) {
        char c = input[word_start];
        if (repl_is_completion_word_break(c)) {
            return word_start + 1;
        }
        if (word_start == 0) {
            break;
        }
        word_start--;
    }
    if (word_start < 0) return 0;
    return word_start;
}

fn bool repl_completion_matches_prefix(char[] name, char* prefix, usz prefix_len) @inline {
    if (name.len < prefix_len) return false;
    for (usz i = 0; i < prefix_len; i++) {
        if (name[i] != prefix[i]) return false;
    }
    return true;
}

fn void repl_completion_emit(void* completions, char[] name) {
    char[256] nbuf;
    usz copy = name.len;
    if (copy > 255) {
        copy = 255;
    }
    for (usz i = 0; i < copy; i++) {
        nbuf[i] = name[i];
    }
    nbuf[copy] = 0;
    replxx_add_completion(completions, &nbuf[0]);
}

// Completion callback — complete defined symbols
fn void lisp_completion(char* input, void* completions, CInt* context_len, void* ud) {
    if (input == null || g_repl_interp == null) return;

    usz len = repl_cstr_len_ptr(input);
    if (len == 0) return;

    isz word_start = repl_completion_find_word_start(input, len);

    usz prefix_len = len - (usz)word_start;
    if (prefix_len == 0) return;
    *context_len = (CInt)prefix_len;
    char* prefix = &input[word_start];

    // Search global env bindings
    Env* env = g_repl_interp.global_env;
    for (usz i = 0; i < env.binding_count; i++) {
        char[] name = g_repl_interp.symbols.get_name(env.bindings[i].name);
        if (repl_completion_matches_prefix(name, prefix, prefix_len)) {
            repl_completion_emit(completions, name);
        }
    }
}
