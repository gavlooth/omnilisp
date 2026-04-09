module main;

import lisp;

struct FmtFrame {
    char close_char;
    int  open_column;
    int  child_column;
    bool let_binding_pending;
    bool binding_context;
    bool higher_order_lambda_context;
    bool coroutine_lambda_context;
}

fn int fmt_vector_child_column(char[] text, usz open_index, int line_indent_column, int indent_width) {
    usz value_start = fmt_skip_horizontal_space(text, open_index + 1);
    if (value_start >= text.len || fmt_is_close_delimiter(text[value_start]) || text[value_start] == ';') {
        return line_indent_column + indent_width;
    }
    if (fmt_is_open_delimiter(text[value_start])) return line_indent_column + (int)open_index + 1;
    return line_indent_column + indent_width;
}

fn int fmt_map_child_column(char[] text, usz open_index, int line_indent_column, int indent_width) {
    usz value_start = fmt_skip_horizontal_space(text, open_index + 1);
    if (value_start >= text.len || fmt_is_close_delimiter(text[value_start]) || text[value_start] == ';') {
        return line_indent_column + indent_width;
    }
    return line_indent_column + (int)open_index + 1;
}

fn int fmt_list_child_column(char[] text, usz open_index, int line_indent_column, int indent_width, bool* let_binding_pending, bool* higher_order_lambda_context, bool* coroutine_lambda_context) {
    *let_binding_pending = false;
    *higher_order_lambda_context = false;
    *coroutine_lambda_context = false;

    usz head_start = fmt_skip_horizontal_space(text, open_index + 1);
    if (head_start >= text.len || fmt_is_close_delimiter(text[head_start]) || text[head_start] == ';') {
        return line_indent_column + indent_width;
    }
    if (fmt_is_open_delimiter(text[head_start])) return line_indent_column + (int)open_index + 1;

    usz head_end = fmt_scan_token_end(text, head_start);
    if (head_end <= head_start) return line_indent_column + indent_width;

    if (fmt_token_equals(text, head_start, head_end, "let")) *let_binding_pending = true;
    if (lisp::parser_head_policy_is_higher_order_call(text, head_start, head_end)) *higher_order_lambda_context = true;
    if (lisp::parser_head_policy_is_coroutine_wrapper(text, head_start, head_end)) *coroutine_lambda_context = true;
    if (lisp::parser_head_policy_is_wide_branch_form(text, head_start, head_end)) {
        return line_indent_column + (int)open_index + indent_width * 2;
    }
    if (lisp::parser_head_policy_is_block_form(text, head_start, head_end)) return line_indent_column + indent_width;

    usz value_start = fmt_skip_horizontal_space(text, head_end);
    if (value_start >= text.len || fmt_is_close_delimiter(text[value_start]) || text[value_start] == ';') {
        return line_indent_column + indent_width;
    }
    if (!lisp::parser_head_policy_aligns_to_second_token(text, head_start, head_end)) return line_indent_column + (int)value_start;
    return line_indent_column + (int)value_start;
}

fn int fmt_line_indent_column(char[] text, FmtFrame[512] stack, int depth) {
    int temp_depth = depth;
    int anchor = -1;
    usz index = 0;

    while (index < text.len && fmt_is_close_delimiter(text[index])) {
        if (temp_depth <= 0) break;
        if (stack[temp_depth - 1].close_char != text[index]) break;
        anchor = stack[temp_depth - 1].open_column;
        temp_depth--;
        index++;
    }

    if (index >= text.len) return anchor >= 0 ? anchor : 0;
    if (anchor >= 0) return anchor;
    return temp_depth > 0 ? stack[temp_depth - 1].child_column : 0;
}

fn bool fmt_push_frame(FmtFrame[512]* stack, int* depth, bool* overflowed, char close_char, int open_column, int child_column, bool let_binding_pending = false, bool binding_context = false, bool higher_order_lambda_context = false, bool coroutine_lambda_context = false) {
    if (*depth >= 512) {
        *overflowed = true;
        return false;
    }
    (*stack)[*depth] = {
        .close_char = close_char,
        .open_column = open_column,
        .child_column = child_column,
        .let_binding_pending = let_binding_pending,
        .binding_context = binding_context,
        .higher_order_lambda_context = higher_order_lambda_context,
        .coroutine_lambda_context = coroutine_lambda_context,
    };
    (*depth)++;
    return true;
}

fn void fmt_update_frames_for_line(char[] text, int line_indent_column, int indent_width, FmtFrame[512]* stack, int* depth, bool* overflowed) {
    usz index = 0;
    while (index < text.len) {
        char c = text[index];
        if (c == ';') break;
        if (c == '"') {
            index = fmt_scan_string_end(text, index);
            continue;
        }
        if (fmt_is_close_delimiter(c)) {
            if (*depth > 0 && (*stack)[*depth - 1].close_char == c) (*depth)--;
            index++;
            continue;
        }
        if (!fmt_is_open_delimiter(c)) {
            index++;
            continue;
        }

        int open_column = line_indent_column + (int)index;
        bool let_binding_pending = false;
        bool higher_order_lambda_context = false;
        bool coroutine_lambda_context = false;
        int child_column;

        bool binding_context = false;

        if (c == '(' && *depth > 0 && (*stack)[*depth - 1].let_binding_pending) {
            (*stack)[*depth - 1].let_binding_pending = false;
            usz binding_start = fmt_skip_horizontal_space(text, index + 1);
            child_column = binding_start < text.len ? line_indent_column + (int)binding_start : line_indent_column + indent_width;
            binding_context = true;
        } else if (c == '(') {
            child_column = fmt_list_child_column(text, index, line_indent_column, indent_width, &let_binding_pending, &higher_order_lambda_context, &coroutine_lambda_context);
            if (*depth > 0 && (*stack)[*depth - 1].binding_context && lisp::parser_head_policy_prefers_binding_context_alignment(text, index)) {
                child_column = open_column + indent_width;
            }
            if (*depth > 0 && (*stack)[*depth - 1].higher_order_lambda_context) {
                usz head_start = fmt_skip_horizontal_space(text, index + 1);
                usz head_end = fmt_scan_token_end(text, head_start);
                if (head_end > head_start && fmt_token_equals(text, head_start, head_end, "lambda")) {
                    child_column = open_column + indent_width;
                }
            }
            if (*depth > 0 && (*stack)[*depth - 1].coroutine_lambda_context) {
                usz head_start = fmt_skip_horizontal_space(text, index + 1);
                usz head_end = fmt_scan_token_end(text, head_start);
                if (head_end > head_start && fmt_token_equals(text, head_start, head_end, "lambda")) {
                    child_column = line_indent_column + indent_width * 2;
                }
            }
        } else if (c == '[') {
            child_column = fmt_vector_child_column(text, index, line_indent_column, indent_width);
        } else {
            child_column = fmt_map_child_column(text, index, line_indent_column, indent_width);
        }

        if (!fmt_push_frame(
                stack,
                depth,
                overflowed,
                fmt_matching_closer(c),
                open_column,
                child_column,
                let_binding_pending,
                binding_context,
                higher_order_lambda_context,
                coroutine_lambda_context
            )) {
            return;
        }
        index++;
    }
}
