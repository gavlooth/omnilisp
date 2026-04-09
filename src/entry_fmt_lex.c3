module main;

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
