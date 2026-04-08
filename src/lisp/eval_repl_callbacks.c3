module lisp;

// Global replxx handle for use in callbacks
void* g_replxx = null;
// Global interp for completion callback
Interp* g_repl_interp = null;

// =============================================================================
// REPLXX CALLBACKS — Completion
// =============================================================================

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
