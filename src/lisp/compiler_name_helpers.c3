module lisp;

fn char[] compiler_format_generated_name_id(char[] buffer, char[] prefix, long id) {
    usz pos = 0;
    for (usz i = 0; i < prefix.len; i++) {
        buffer[pos++] = prefix[i];
    }

    char[32] id_buf;
    char[] digits = int_to_string(id, &id_buf);
    for (usz i = 0; i < digits.len; i++) {
        buffer[pos++] = digits[i];
    }
    return buffer[:pos];
}

fn char[] compiler_format_generated_name_index(char[] buffer, char[] prefix, long idx) {
    usz pos = 0;
    for (usz i = 0; i < prefix.len; i++) {
        buffer[pos++] = prefix[i];
    }
    buffer[pos++] = '[';

    char[32] idx_buf;
    char[] digits = int_to_string(idx, &idx_buf);
    for (usz i = 0; i < digits.len; i++) {
        buffer[pos++] = digits[i];
    }

    buffer[pos++] = ']';
    return buffer[:pos];
}

fn char[] compiler_format_generated_name_suffix(char[] buffer, char[] base, char[] suffix) {
    usz pos = 0;
    for (usz i = 0; i < base.len; i++) {
        buffer[pos++] = base[i];
    }
    for (usz i = 0; i < suffix.len; i++) {
        buffer[pos++] = suffix[i];
    }
    return buffer[:pos];
}

fn char[] compiler_format_generated_name_pair(char[] buffer, char[] prefix, long first, long second) {
    usz pos = 0;
    for (usz i = 0; i < prefix.len; i++) {
        buffer[pos++] = prefix[i];
    }

    char[32] first_buf;
    char[] first_digits = int_to_string(first, &first_buf);
    for (usz i = 0; i < first_digits.len; i++) {
        buffer[pos++] = first_digits[i];
    }

    buffer[pos++] = '_';

    char[32] second_buf;
    char[] second_digits = int_to_string(second, &second_buf);
    for (usz i = 0; i < second_digits.len; i++) {
        buffer[pos++] = second_digits[i];
    }
    return buffer[:pos];
}
