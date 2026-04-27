module lisp;


import std::io;

// =============================================================================
// REPLXX FFI DECLARATIONS
// =============================================================================
// replxx is a readline replacement with syntax highlighting, hints, and completions.
// replxx_input returns a library-managed pointer (do NOT free it). null on EOF.

// Opaque replxx handle — use void* since C3 requires uppercase for type defs
alias ReplxxHighlighterCb = fn void(char*, CInt*, CInt, void*);
alias ReplxxCompletionCb = fn void(char*, void*, CInt*, void*);

extern fn void* replxx_init() @extern("replxx_init");
extern fn void replxx_end(void* rx) @extern("replxx_end");
extern fn char* replxx_input(void* rx, char* prompt) @extern("replxx_input");
extern fn void replxx_history_add(void* rx, char* line) @extern("replxx_history_add");
extern fn int replxx_history_load(void* rx, char* filename) @extern("replxx_history_load");
extern fn int replxx_history_save(void* rx, char* filename) @extern("replxx_history_save");
extern fn void replxx_set_max_history_size(void* rx, CInt len) @extern("replxx_set_max_history_size");
extern fn void replxx_set_highlighter_callback(void* rx, ReplxxHighlighterCb cb, void* ud) @extern("replxx_set_highlighter_callback");
extern fn void replxx_set_completion_callback(void* rx, ReplxxCompletionCb cb, void* ud) @extern("replxx_set_completion_callback");
extern fn void replxx_add_completion(void* lc, char* str) @extern("replxx_add_completion");
extern fn void replxx_set_word_break_characters(void* rx, char* wb) @extern("replxx_set_word_break_characters");
extern fn void replxx_set_unique_history(void* rx, CInt val) @extern("replxx_set_unique_history");
extern fn int replxx_install_window_change_handler(void* rx) @extern("replxx_install_window_change_handler");
extern fn void replxx_enable_bracketed_paste(void* rx) @extern("replxx_enable_bracketed_paste");
extern fn void replxx_set_indent_multiline(void* rx, CInt val) @extern("replxx_set_indent_multiline");

// =============================================================================
// SECTION 9: REPL
// =============================================================================

fn char* repl_prompt_for(usz buf_len) @inline {
    if (buf_len == 0) return "\x1b[1;34momni>\x1b[0m ";
    return "\x1b[1;34m ....\x1b[0m ";
}

fn bool repl_handle_eof(usz* buf_len) {
    if (*buf_len > 0) {
        io::printn("");
        *buf_len = 0;
        return false;
    }
    io::printn("\nGoodbye!");
    return true;
}

enum ReplLineAction : char {
    REPL_LINE_SKIP,
    REPL_LINE_QUIT,
    REPL_LINE_APPEND,
}

fn ReplLineAction repl_classify_line(char* line, usz len, usz* buf_len) {
    if (*buf_len > 0 && len == 0) {
        io::printn("Input cancelled.");
        *buf_len = 0;
        return REPL_LINE_SKIP;
    }
    if (*buf_len == 0 && len == 0) {
        return REPL_LINE_SKIP;
    }
    if (*buf_len == 0 && repl_is_quit_command(line, len)) {
        io::printn("Goodbye!");
        return REPL_LINE_QUIT;
    }
    return REPL_LINE_APPEND;
}

fn void repl_eval_buffer(void* rx, char[8192]* buf, usz* buf_len, Interp* interp, char[] ansi_red, char[] ansi_green, char[] ansi_reset) {
    if (*buf_len >= (*buf).len) {
        if (rx != null) io::eprintn("Error: REPL input line too long");
        *buf_len = 0;
        return;
    }
    char[] accumulated = (*buf)[:*buf_len];
    int depth = count_paren_depth(accumulated);
    if (depth > 0) return;

    (*buf)[*buf_len] = 0;
    replxx_history_add(rx, &(*buf)[0]);
    runtime_gc_between_top_level_runs();

    char[] input = (*buf)[:*buf_len];
    interp_sigint_bind(interp);
    defer interp_sigint_unbind(interp);
    // Drop stale Ctrl+C presses that happened outside active eval.
    interp_interrupt_clear(interp);
    EvalResult r = repl_eval_line(input, interp);
    repl_print_eval_outcome(r, interp, ansi_red, ansi_green, ansi_reset);
    *buf_len = 0;
}

fn bool repl_init_session(Interp* interp, void** rx_out, char* history_file) {
    void* rx = replxx_init();
    if (rx == null) {
        io::eprintn("Failed to initialize replxx");
        return false;
    }
    g_replxx = rx;
    g_repl_interp = interp;
    repl_configure(rx, history_file);
    *rx_out = rx;
    return true;
}

fn void repl_shutdown_session(void* rx, char* history_file) {
    replxx_history_save(rx, history_file);
    replxx_end(rx);
    g_replxx = null;
    g_repl_interp = null;
}

fn void repl_print_banner() {
    io::printn("Omni Lisp REPL (type 'quit' or 'exit' to leave, Ctrl-D for EOF)");
    io::printn("---");
}

fn bool repl_step(
    void* rx,
    Interp* interp,
    char[8192]* buf,
    usz* buf_len,
    char[] ansi_red,
    char[] ansi_green,
    char[] ansi_reset
) {
    char* line = (char*)replxx_input(rx, repl_prompt_for(*buf_len));
    if (line == null) {
        if (repl_handle_eof(buf_len)) return false;
        return true;
    }

    usz len = repl_cstr_len(line);
    ReplLineAction action = repl_classify_line(line, len, buf_len);
    if (action == REPL_LINE_SKIP) return true;
    if (action == REPL_LINE_QUIT) return false;

    if (!repl_append_line(buf, buf_len, line, len)) {
        io::eprintn("Error: REPL input line too long");
        *buf_len = 0;
        return true;
    }
    repl_eval_buffer(rx, buf, buf_len, interp, ansi_red, ansi_green, ansi_reset);
    return true;
}

fn void repl(Interp* interp) {
    // Install SIGINT handler so Ctrl+C interrupts eval instead of killing process
    interp_sigint_install_handler();

    char* history_file = ".omni_history";
    void* rx = null;
    if (!repl_init_session(interp, &rx, history_file)) {
        return;
    }
    defer {
        repl_shutdown_session(rx, history_file);
    }

    // ANSI color codes for output
    char[] ansi_red     = "\x1b[31m";
    char[] ansi_green   = "\x1b[32m";
    char[] ansi_reset   = "\x1b[0m";

    repl_print_banner();

    // Buffer for accumulating multi-line input
    char[8192] buf;
    usz buf_len = 0;

    while (repl_step(rx, interp, &buf, &buf_len, ansi_red, ansi_green, ansi_reset)) {}
}
