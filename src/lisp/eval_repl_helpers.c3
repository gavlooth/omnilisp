module lisp;

import std::io;
import main;

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
    if (buffer.len == 0) return out;
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
            break;
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

    buffer[out.len] = 0;
    return out;
}

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

foreach (item : input) {
        char c = item;
        if (repl_paren_depth_step_string(c, &state)) continue;
        if (repl_paren_depth_step_comment(c, &state)) continue;
        repl_paren_depth_step_normal(c, &state);
}
    return state.depth;
}

fn usz repl_cstr_len(char* line) @inline {
    usz len = 0;
    while (len < 65536 && line[len] != 0) len++;
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
