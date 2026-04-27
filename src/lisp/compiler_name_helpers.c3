module lisp;

const char[] COMPILER_GENERATED_NAME_OVERFLOW_ERROR = "compiler: generated name exceeds buffer capacity";

fn bool compiler_generated_name_append(char[] buffer, usz* pos, char[] text) {
    foreach (item : text) {
        if (*pos >= buffer.len) return false;
        buffer[*pos] = item;
        *pos = *pos + 1;
    }
    return true;
}

fn bool compiler_generated_name_append_char(char[] buffer, usz* pos, char item) {
    if (*pos >= buffer.len) return false;
    buffer[*pos] = item;
    *pos = *pos + 1;
    return true;
}

fn char[] compiler_format_generated_name_id(char[] buffer, char[] prefix, long id) {
    usz pos = 0;
    if (!compiler_generated_name_append(buffer, &pos, prefix)) return "";

    char[32] id_buf;
    char[] digits = int_to_string(id, &id_buf);
    if (!compiler_generated_name_append(buffer, &pos, digits)) return "";
    return buffer[:pos];
}

fn char[] compiler_format_generated_name_index(char[] buffer, char[] prefix, long idx) {
    usz pos = 0;
    if (!compiler_generated_name_append(buffer, &pos, prefix)) return "";
    if (!compiler_generated_name_append_char(buffer, &pos, '[')) return "";

    char[32] idx_buf;
    char[] digits = int_to_string(idx, &idx_buf);
    if (!compiler_generated_name_append(buffer, &pos, digits)) return "";

    if (!compiler_generated_name_append_char(buffer, &pos, ']')) return "";
    return buffer[:pos];
}

fn char[] compiler_format_generated_name_suffix(char[] buffer, char[] base, char[] suffix) {
    usz pos = 0;
    if (!compiler_generated_name_append(buffer, &pos, base)) return "";
    if (!compiler_generated_name_append(buffer, &pos, suffix)) return "";
    return buffer[:pos];
}

fn char[] compiler_format_generated_name_pair(char[] buffer, char[] prefix, long first, long second) {
    usz pos = 0;
    if (!compiler_generated_name_append(buffer, &pos, prefix)) return "";

    char[32] first_buf;
    char[] first_digits = int_to_string(first, &first_buf);
    if (!compiler_generated_name_append(buffer, &pos, first_digits)) return "";

    if (!compiler_generated_name_append_char(buffer, &pos, '_')) return "";

    char[32] second_buf;
    char[] second_digits = int_to_string(second, &second_buf);
    if (!compiler_generated_name_append(buffer, &pos, second_digits)) return "";
    return buffer[:pos];
}

fn char[] Compiler.generated_name_id(Compiler* self, char[] buffer, char[] prefix, long id) {
    char[] name = compiler_format_generated_name_id(buffer, prefix, id);
    if (name.len == 0) self.set_compile_error(COMPILER_GENERATED_NAME_OVERFLOW_ERROR);
    return name;
}

fn char[] Compiler.generated_name_index(Compiler* self, char[] buffer, char[] prefix, long idx) {
    char[] name = compiler_format_generated_name_index(buffer, prefix, idx);
    if (name.len == 0) self.set_compile_error(COMPILER_GENERATED_NAME_OVERFLOW_ERROR);
    return name;
}

fn char[] Compiler.generated_name_suffix(Compiler* self, char[] buffer, char[] base, char[] suffix) {
    char[] name = compiler_format_generated_name_suffix(buffer, base, suffix);
    if (name.len == 0) self.set_compile_error(COMPILER_GENERATED_NAME_OVERFLOW_ERROR);
    return name;
}

fn char[] Compiler.generated_name_pair(Compiler* self, char[] buffer, char[] prefix, long first, long second) {
    char[] name = compiler_format_generated_name_pair(buffer, prefix, first, second);
    if (name.len == 0) self.set_compile_error(COMPILER_GENERATED_NAME_OVERFLOW_ERROR);
    return name;
}
