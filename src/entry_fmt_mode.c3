module main;

import std::io;
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

fn char* find_fmt_input_file(int argc, char** argv, int fmt_idx) {
    for (int i = fmt_idx + 1; i < argc; i++) {
        if (str_eq(argv[i], "--write")) continue;
        if (str_eq(argv[i], "--check")) continue;
        if (cli_arg_starts_with_dash(argv[i])) return null;
        return argv[i];
    }
    return null;
}

fn char* find_fmt_option_like_arg(int argc, char** argv, int fmt_idx) {
    for (int i = fmt_idx + 1; i < argc; i++) {
        if (str_eq(argv[i], "--write")) continue;
        if (str_eq(argv[i], "--check")) continue;
        if (cli_arg_starts_with_dash(argv[i])) return argv[i];
        return null;
    }
    return null;
}

fn bool fmt_is_horizontal_space(char c) @inline {
    return c == ' ' || c == '\t';
}

fn bool fmt_is_blank_line(char[] line) {
    foreach (c : line) {
        if (c == '\r') continue;
        if (!fmt_is_horizontal_space(c)) return false;
    }
    return true;
}

fn usz fmt_trimmed_line_end(char[] line) {
    usz end = line.len;
    while (end > 0) {
        char c = line[end - 1];
        if (c == '\r' || fmt_is_horizontal_space(c)) {
            end--;
            continue;
        }
        break;
    }
    return end;
}

fn usz fmt_line_content_start(char[] line, usz trimmed_end) {
    usz start = 0;
    while (start < trimmed_end && fmt_is_horizontal_space(line[start])) start++;
    return start;
}

fn bool fmt_is_close_delimiter(char c) @inline {
    return c == ')' || c == ']' || c == '}';
}

fn bool fmt_is_open_delimiter(char c) @inline {
    return c == '(' || c == '[' || c == '{';
}

fn char fmt_matching_closer(char c) @inline {
    switch (c) {
        case '(': return ')';
        case '[': return ']';
        case '{': return '}';
        default: return '\0';
    }
}

fn usz fmt_skip_horizontal_space(char[] text, usz index) {
    while (index < text.len && fmt_is_horizontal_space(text[index])) index++;
    return index;
}

fn usz fmt_scan_token_end(char[] text, usz index) {
    while (index < text.len) {
        char c = text[index];
        if (fmt_is_horizontal_space(c) || fmt_is_open_delimiter(c) || fmt_is_close_delimiter(c) || c == ';' || c == '"') {
            break;
        }
        index++;
    }
    return index;
}

fn bool fmt_token_equals(char[] text, usz start, usz end, char[] token) {
    usz len = end - start;
    if (len != token.len) return false;
    for (usz i = 0; i < len; i++) {
        if (text[start + i] != token[i]) return false;
    }
    return true;
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

fn void fmt_append_space_columns(DString* out, int count) {
    for (int i = 0; i < count; i++) out.append_char(' ');
}

fn int fmt_leading_closer_count(char[] text) {
    int count = 0;
    usz index = 0;
    while (index < text.len) {
        char c = text[index];
        if (fmt_is_horizontal_space(c)) {
            index++;
            continue;
        }
        if (c == ')' || c == ']' || c == '}') {
            count++;
            index++;
            continue;
        }
        break;
    }
    return count;
}

fn usz fmt_scan_string_end(char[] text, usz index) {
    usz cursor = index + 1;
    while (cursor < text.len) {
        if (text[cursor] == '\\') {
            cursor += 2;
            continue;
        }
        if (text[cursor] == '"') return cursor + 1;
        cursor++;
    }
    return text.len;
}

fn bool fmt_text_equal(char[] a, char[] b) {
    if (a.len != b.len) return false;
    for (usz i = 0; i < a.len; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}

fn bool fmt_uses_crlf(char[] text) {
    for (usz i = 1; i < text.len; i++) {
        if (text[i] == '\n' && text[i - 1] == '\r') return true;
    }
    return false;
}

fn void fmt_append_newline(DString* out, bool use_crlf) {
    if (use_crlf) out.append_char('\r');
    out.append_char('\n');
}

fn int fmt_line_indent_delta(char[] text) {
    int delta = 0;
    usz index = 0;
    while (index < text.len) {
        char c = text[index];
        if (c == ';') break;
        if (c == '"') {
            index = fmt_scan_string_end(text, index);
            continue;
        }
        if (c == '(' || c == '[' || c == '{') {
            delta++;
            index++;
            continue;
        }
        if (c == ')' || c == ']' || c == '}') {
            delta--;
            index++;
            continue;
        }
        index++;
    }
    return delta;
}

struct FmtFormatResult {
    bool ok;
    char[] text;
}

fn FmtFormatResult formatted_text(char[] text, bool insert_spaces = true, int tab_size = 2) {
    DString out;
    out.init(mem);
    bool use_crlf = fmt_uses_crlf(text);

    char[] indent_unit = "\t";
    char[8] spaces_buf = {};
    if (insert_spaces) {
        int width = tab_size > 0 ? tab_size : 2;
        if (width > spaces_buf.len) width = spaces_buf.len;
        for (int i = 0; i < width; i++) spaces_buf[i] = ' ';
        indent_unit = spaces_buf[:width];
    }

    if (insert_spaces) {
        int indent_width = tab_size > 0 ? tab_size : 2;
        FmtFrame[512] stack = {};
        int depth = 0;
        bool overflowed = false;
        usz line_start = 0;

        while (line_start < text.len) {
            usz line_end = line_start;
            while (line_end < text.len && text[line_end] != '\n') line_end++;

            char[] line = text[line_start:line_end - line_start];
            usz trimmed_end = fmt_trimmed_line_end(line);

            if (!fmt_is_blank_line(line[:trimmed_end])) {
                usz content_start = fmt_line_content_start(line, trimmed_end);
                char[] stripped = line[content_start:trimmed_end - content_start];
                int line_indent_column = fmt_line_indent_column(stripped, stack, depth);
                fmt_append_space_columns(&out, line_indent_column);
                out.append_string((String)stripped);
                fmt_update_frames_for_line(stripped, line_indent_column, indent_width, &stack, &depth, &overflowed);
                if (overflowed) return { .ok = false, .text = "" };
            }

            if (line_end < text.len) fmt_append_newline(&out, use_crlf);
            line_start = line_end + 1;
        }

        return { .ok = true, .text = out.str_view() };
    }

    int indent_level = 0;
    usz line_start = 0;

    while (line_start < text.len) {
        usz line_end = line_start;
        while (line_end < text.len && text[line_end] != '\n') line_end++;

        char[] line = text[line_start:line_end - line_start];
        usz trimmed_end = fmt_trimmed_line_end(line);

        if (!fmt_is_blank_line(line[:trimmed_end])) {
            usz content_start = fmt_line_content_start(line, trimmed_end);
            char[] stripped = line[content_start:trimmed_end - content_start];
            int leading_closers = fmt_leading_closer_count(stripped);
            int line_indent_level = indent_level - leading_closers;
            if (line_indent_level < 0) line_indent_level = 0;
            for (int i = 0; i < line_indent_level; i++) out.append_string((String)indent_unit);
            out.append_string((String)stripped);
            indent_level += fmt_line_indent_delta(stripped);
            if (indent_level < 0) indent_level = 0;
        }

        if (line_end < text.len) fmt_append_newline(&out, use_crlf);
        line_start = line_end + 1;
    }

    return { .ok = true, .text = out.str_view() };
}

fn int run_fmt_mode(int argc, char** argv, int fmt_idx) {
    bool write = find_flag_index(argc, argv, "--write") >= 0;
    bool check = find_flag_index(argc, argv, "--check") >= 0;
    if (write && check) return print_fmt_usage();

    char* input_file = find_fmt_input_file(argc, argv, fmt_idx);
    if (input_file == null) {
        char* unexpected = find_fmt_option_like_arg(argc, argv, fmt_idx);
        if (unexpected != null) {
            return print_unexpected_mode_arg("--fmt", unexpected);
        }
        return print_fmt_usage();
    }
    char* extra = find_extra_mode_arg(argc, argv, fmt_idx, 1, false, true, true);
    if (extra != null) {
        return print_unexpected_mode_arg("--fmt", extra);
    }

    char[] input_path = input_file[:cstr_len(input_file)];
    if (try source = io::file::load_temp((String)input_path)) {
        FmtFormatResult formatted_result = formatted_text(source);
        if (!formatted_result.ok) return print_fmt_structure_error(input_file);
        char[] formatted = formatted_result.text;
        bool changed = !fmt_text_equal(formatted, source);

        if (check) {
            return changed ? 1 : 0;
        }

        if (write) {
            if (try file = io::file::open((String)input_path, "w")) {
                char[] remaining = formatted;
                bool write_ok = true;
                while (write_ok && remaining.len > 0) {
                    usz? written_opt = file.write(remaining);
                    if (catch err = written_opt) {
                        write_ok = false;
                        break;
                    }
                    if (written_opt == 0) {
                        write_ok = false;
                        break;
                    }
                    remaining = remaining[written_opt..];
                }
                if (catch err = file.close()) write_ok = false;
                if (!write_ok) {
                    return print_fmt_write_error(input_file);
                }
                return 0;
            }
            return print_fmt_write_error(input_file);
        }

        io::print((String)formatted);
        return 0;
    }
    return print_fmt_read_error(input_file);
}
