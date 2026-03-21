module lisp;


import std::io;
import main;

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

/**
 * Read a line of input from stdin.
 * Returns the number of characters read (not including null terminator).
 */
struct ReadLineResult {
    usz len;
    bool eof;
    bool truncated;
}

fn ReadLineResult read_line(char[] buffer) {
    ReadLineResult out = { .len = 0, .eof = false, .truncated = false };
    usz max_len = buffer.len - 1;
    while (out.len < max_len) {
        if (try c = io::stdin().read_byte()) {
            if (c == '\n') {
                break;
            }
            buffer[out.len] = c;
            out.len++;
        } else {
            out.eof = true;
            break;  // EOF or error
        }
    }

    if (out.len >= max_len) {
        if (try c = io::stdin().read_byte()) {
            if (c != '\n') {
                out.truncated = true;
            }
            if (c != '\n') {
                while (try c2 = io::stdin().read_byte()) {
                    if (c2 == '\n') {
                        break;
                    }
                }
            }
        } else {
            out.eof = true;
        }
    }

    buffer[out.len] = 0;  // Null terminate
    return out;
}

/**
 * Read-Eval-Print-Loop.
 */
// Count net parenthesis depth in a string, skipping chars inside "..." strings
// and ; line comments. Returns the net depth (opens - closes).
struct ReplParenDepthState {
    int depth;
    bool in_string;
    bool in_comment;
    bool escape_next;
}

fn bool repl_paren_depth_step_string(char c, ReplParenDepthState* state) @inline {
    if (!state.in_string) return false;
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

fn bool repl_paren_depth_step_comment(char c, ReplParenDepthState* state) @inline {
    if (!state.in_comment) return false;
    if (c == '\n') {
        state.in_comment = false;
    }
    return true;
}

fn void repl_paren_depth_step_normal(char c, ReplParenDepthState* state) @inline {
    if (c == ';') {
        state.in_comment = true;
        return;
    }
    if (c == '"') {
        state.in_string = true;
        return;
    }
    if (c == '(') {
        state.depth++;
        return;
    }
    if (c == ')') {
        state.depth--;
    }
}

fn int count_paren_depth(char[] input) {
    ReplParenDepthState state = {
        .depth = 0,
        .in_string = false,
        .in_comment = false,
        .escape_next = false,
    };

    for (usz i = 0; i < input.len; i++) {
        char c = input[i];
        if (repl_paren_depth_step_string(c, &state)) continue;
        if (repl_paren_depth_step_comment(c, &state)) continue;
        repl_paren_depth_step_normal(c, &state);
    }
    return state.depth;
}

fn usz repl_cstr_len(char* line) @inline {
    usz len = 0;
    while (line[len] != 0) len++;
    return len;
}

fn bool repl_is_quit_command(char* line, usz len) @inline {
    if (len >= 4 &&
        line[0] == 'q' && line[1] == 'u' &&
        line[2] == 'i' && line[3] == 't') {
        return true;
    }
    if (len >= 4 &&
        line[0] == 'e' && line[1] == 'x' &&
        line[2] == 'i' && line[3] == 't') {
        return true;
    }
    return false;
}

fn bool repl_append_line(char[8192]* buf, usz* buf_len, char* line, usz len) {
    if (*buf_len > 0 && *buf_len < 8191) {
        (*buf)[*buf_len] = '\n';
        (*buf_len)++;
    }
    bool truncated = false;
    usz copy_len = len;
    if (*buf_len + copy_len > 8191) {
        copy_len = 8191 - *buf_len;
        truncated = true;
    }
    for (usz i = 0; i < copy_len; i++) {
        (*buf)[*buf_len + i] = line[i];
    }
    *buf_len += copy_len;
    return !truncated && copy_len == len;
}

fn EvalResult repl_eval_source(char[] input, Interp* interp, bool program_mode) {
    boundary_assert_interp_scope_chain(interp);
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);

    main::ScopeRegion* repl_child_scope = boundary_push_child_scope(interp);
    if (repl_child_scope == null) {
        return eval_error("repl: failed to create child scope");
    }
    boundary_assert_interp_scope_chain(interp);
    defer boundary_pop_child_scope(interp, session.current_scope, repl_child_scope);

    EvalResult r = program_mode ? run_program(input, interp) : run(input, interp);
    if (!r.error.has_error && r.value != null) {
        BoundaryScopeSwap target_swap = boundary_scope_swap_begin(interp, session.current_scope);
        defer boundary_scope_swap_end(interp, &target_swap);
        BoundaryReleasingScopeSwap releasing_swap =
            boundary_releasing_scope_swap_begin(interp, repl_child_scope);
        defer boundary_releasing_scope_swap_end(interp, &releasing_swap);
        PromotionContext copy_ctx = {};
        boundary_promotion_context_begin(interp, &copy_ctx);
        defer boundary_promotion_context_end(interp, &copy_ctx);
        BoundaryCopyResult copied = boundary_copy_to_parent_site_ctx_checked(
            r.value,
            interp,
            COPY_SITE_REPL_LINE_RESULT,
            &copy_ctx
        );
        if (!boundary_copy_result_ok(copied)) {
            return eval_error(boundary_copy_fault_message(copied.fault_code));
        }
        r.value = copied.value;
    }
    boundary_assert_interp_scope_chain(interp);
    return r;
}

fn EvalResult repl_eval_line(char[] input, Interp* interp) {
    return repl_eval_source(input, interp, false);
}

fn EvalResult repl_eval_program(char[] input, Interp* interp) {
    return repl_eval_source(input, interp, true);
}

fn void repl_print_eval_outcome(EvalResult r, Interp* interp, char[] ansi_red, char[] ansi_green, char[] ansi_reset) {
    if (r.error.has_error) {
        io::printf("%s", (ZString)ansi_red);
        if (r.error.line > 0) {
            io::printf("Error at line %d, column %d: ", (int)r.error.line, (int)r.error.column);
        } else {
            io::print("Error: ");
        }
        io::printf("%s", (ZString)&r.error.message);
        io::printf("%s", (ZString)ansi_reset);
        io::printn("");
        return;
    }

    io::printf("%s", (ZString)ansi_green);
    print_value(r.value, &interp.symbols);
    io::printf("%s", (ZString)ansi_reset);
    io::printn("");
}

fn void repl_configure(void* rx, char* history_file) {
    replxx_set_max_history_size(rx, 1000);
    replxx_set_unique_history(rx, 1);
    replxx_install_window_change_handler(rx);
    replxx_set_word_break_characters(rx, " \t\n()[]{}';\"");
    replxx_set_indent_multiline(rx, 1);
    replxx_set_highlighter_callback(rx, &lisp_highlighter, null);
    replxx_set_completion_callback(rx, &lisp_completion, null);
    replxx_history_load(rx, history_file);
}

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
    char[] accumulated = (*buf)[:*buf_len];
    int depth = count_paren_depth(accumulated);
    if (depth > 0) return;

    (*buf)[*buf_len] = 0;
    replxx_history_add(rx, &(*buf)[0]);
    runtime_gc_between_top_level_runs();

    char[] input = (*buf)[:*buf_len];
    g_interrupted = false;
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
    signal(SIGINT_VAL, &sigint_handler);

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
