module lisp;

fn char[] compiler_format_generated_name_id(char[] buffer, char[] prefix, long id) {
    usz pos = 0;
foreach (item : prefix) {
        buffer[pos++] = item;
}

    char[32] id_buf;
    char[] digits = int_to_string(id, &id_buf);
foreach (digit : digits) {
        buffer[pos++] = digit;
}
    return buffer[:pos];
}

fn char[] compiler_format_generated_name_index(char[] buffer, char[] prefix, long idx) {
    usz pos = 0;
foreach (item : prefix) {
        buffer[pos++] = item;
}
    buffer[pos++] = '[';

    char[32] idx_buf;
    char[] digits = int_to_string(idx, &idx_buf);
foreach (digit : digits) {
        buffer[pos++] = digit;
}

    buffer[pos++] = ']';
    return buffer[:pos];
}

fn char[] compiler_format_generated_name_suffix(char[] buffer, char[] base, char[] suffix) {
    usz pos = 0;
foreach (item : base) {
        buffer[pos++] = item;
}
foreach (item : suffix) {
        buffer[pos++] = item;
}
    return buffer[:pos];
}

fn char[] compiler_format_generated_name_pair(char[] buffer, char[] prefix, long first, long second) {
    usz pos = 0;
foreach (item : prefix) {
        buffer[pos++] = item;
}

    char[32] first_buf;
    char[] first_digits = int_to_string(first, &first_buf);
foreach (first_digit : first_digits) {
        buffer[pos++] = first_digit;
}

    buffer[pos++] = '_';

    char[32] second_buf;
    char[] second_digits = int_to_string(second, &second_buf);
foreach (second_digit : second_digits) {
        buffer[pos++] = second_digit;
}
    return buffer[:pos];
}
