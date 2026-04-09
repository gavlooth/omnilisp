module main;

fn void fmt_append_space_columns(DString* out, int count) {
    for (int i = 0; i < count; i++) out.append_char(' ');
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
