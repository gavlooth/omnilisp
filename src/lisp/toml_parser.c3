module lisp;

fn bool toml_is_space(char ch) @inline {
    return ch == ' ' || ch == '\t';
}

fn void toml_clear_buf(void* dest_ptr, usz len) {
    char* dest = (char*)dest_ptr;
    for (usz i = 0; i < len; i++) dest[i] = 0;
}

fn void toml_set_default(void* dest_ptr, usz len, char[] value) {
    toml_clear_buf(dest_ptr, len);
    char* dest = (char*)dest_ptr;
    usz copy_len = value.len;
    if (copy_len >= len) copy_len = len - 1;
    for (usz i = 0; i < copy_len; i++) dest[i] = value[i];
}

fn void toml_init_config_defaults(TomlConfig* config) {
    toml_clear_buf(&config.project.name, 128);
    toml_clear_buf(&config.project.version, 32);
    config.ffi_dep_count = 0;
    config.ffi_dep_limit_hit = false;

    toml_set_default(&config.build.output_dir, 128, "build");
    toml_set_default(&config.build.safety, 16, "safe");
    toml_set_default(&config.build.opt, 8, "O0");
    toml_set_default(&config.build.debug_info, 16, "full");
    config.build.single_module = false;
    toml_set_default(&config.build.sanitize, 16, "none");
}

fn void toml_trim_line_bounds(char[] source, usz line_start, usz line_end, usz* out_ls, usz* out_le) {
    usz ls = line_start;
    while (ls < line_end && toml_is_space(source[ls])) ls++;

    usz le = line_end;
    while (le > ls && (toml_is_space(source[le - 1]) || source[le - 1] == '\r')) le--;

    *out_ls = ls;
    *out_le = le;
}

fn isz toml_find_eq_pos(char[] source, usz ls, usz le) {
    for (usz i = ls; i < le; i++) {
        if (source[i] == '=') return (isz)i;
    }
    return -1;
}

fn bool toml_quote_is_escaped(char[] source, usz lower_bound, usz quote_pos) {
    usz slash_count = 0;
    usz pos = quote_pos;
    while (pos > lower_bound && source[pos - 1] == '\\') {
        slash_count++;
        pos--;
    }
    return (slash_count % 2) == 1;
}

fn usz toml_find_closing_quote(char[] source, usz quote_pos, usz end) {
    usz pos = quote_pos + 1;
    while (pos < end) {
        if (source[pos] == '"' && !toml_quote_is_escaped(source, quote_pos + 1, pos)) return pos;
        pos++;
    }
    return end;
}

fn usz toml_trim_value_comment_end(char[] source, usz vs, usz le) {
    bool in_string = false;
    for (usz i = vs; i < le; i++) {
        char ch = source[i];
        if (ch == '"' && !toml_quote_is_escaped(source, vs, i)) {
            in_string = !in_string;
        } else if (ch == '#' && !in_string) {
            le = i;
            break;
        }
    }
    while (le > vs && toml_is_space(source[le - 1])) le--;
    return le;
}

fn bool toml_hex_value(char ch, uint* out) @inline {
    if (out == null) return false;
    if (ch >= '0' && ch <= '9') {
        *out = (uint)(ch - '0');
        return true;
    }
    if (ch >= 'a' && ch <= 'f') {
        *out = (uint)(ch - 'a') + 10;
        return true;
    }
    if (ch >= 'A' && ch <= 'F') {
        *out = (uint)(ch - 'A') + 10;
        return true;
    }
    return false;
}

fn bool toml_read_hex_codepoint(char[] content, usz start, usz digits, uint* out) {
    if (out == null || start + digits > content.len) return false;
    uint codepoint = 0;
    for (usz i = 0; i < digits; i++) {
        uint value = 0;
        if (!toml_hex_value(content[start + i], &value)) return false;
        codepoint = (codepoint << 4) | value;
    }
    if (codepoint > 0x10FFFF) return false;
    if (codepoint >= 0xD800 && codepoint <= 0xDFFF) return false;
    *out = codepoint;
    return true;
}

fn bool toml_append_decoded_byte(char* dest, usz* copy_len, usz max_len, char ch) @inline {
    if (dest == null || copy_len == null) return false;
    if (*copy_len >= max_len) return false;
    dest[*copy_len] = ch;
    (*copy_len)++;
    return true;
}

fn bool toml_append_utf8_codepoint(char* dest, usz* copy_len, usz max_len, uint codepoint) {
    if (codepoint > 0x10FFFF) return false;
    if (codepoint >= 0xD800 && codepoint <= 0xDFFF) return false;
    if (codepoint == 0) return false;
    if (codepoint <= 0x7F) {
        return toml_append_decoded_byte(dest, copy_len, max_len, (char)codepoint);
    }
    if (codepoint <= 0x7FF) {
        return toml_append_decoded_byte(dest, copy_len, max_len, (char)(0xC0 | (codepoint >> 6))) &&
            toml_append_decoded_byte(dest, copy_len, max_len, (char)(0x80 | (codepoint & 0x3F)));
    }
    if (codepoint <= 0xFFFF) {
        return toml_append_decoded_byte(dest, copy_len, max_len, (char)(0xE0 | (codepoint >> 12))) &&
            toml_append_decoded_byte(dest, copy_len, max_len, (char)(0x80 | ((codepoint >> 6) & 0x3F))) &&
            toml_append_decoded_byte(dest, copy_len, max_len, (char)(0x80 | (codepoint & 0x3F)));
    }
    return toml_append_decoded_byte(dest, copy_len, max_len, (char)(0xF0 | (codepoint >> 18))) &&
        toml_append_decoded_byte(dest, copy_len, max_len, (char)(0x80 | ((codepoint >> 12) & 0x3F))) &&
        toml_append_decoded_byte(dest, copy_len, max_len, (char)(0x80 | ((codepoint >> 6) & 0x3F))) &&
        toml_append_decoded_byte(dest, copy_len, max_len, (char)(0x80 | (codepoint & 0x3F)));
}

/**
 * Parse TOML source text into caller-provided TomlConfig.
 */
fn void parse_toml(char[] source, TomlConfig* config) {
    toml_init_config_defaults(config);

    // Track current section
    // 0 = none, 1 = [project], 2 = [dependencies.ffi.X], 3 = [build]
    int section_kind = 0;
    usz current_dep = 0;

    usz pos = 0;
    while (pos < source.len) {
        // Find line end
        usz line_start = pos;
        while (pos < source.len && source[pos] != '\n') pos++;
        usz line_end = pos;
        if (pos < source.len) pos++; // skip \n

        usz ls = 0;
        usz le = 0;
        toml_trim_line_bounds(source, line_start, line_end, &ls, &le);
        if (le == ls) continue;             // blank line
        if (source[ls] == '#') continue;    // comment

        // Section header: [...]
        if (source[ls] == '[') {
            usz section_le = toml_trim_value_comment_end(source, ls, le);
            if (section_le > ls && source[section_le - 1] == ']') {
                char[] header = source[ls + 1 .. section_le - 2]; // inclusive: content between [ ]
                toml_parse_section(header, &section_kind, &current_dep, config);
            } else {
                if (section_kind == 2 && current_dep < config.ffi_dep_count) {
                    config.ffi_deps[current_dep].unknown_key_invalid = true;
                }
                section_kind = 0;
            }
            continue;
        }

        // Key = value — find '='
        isz eq_pos = toml_find_eq_pos(source, ls, le);
        if (eq_pos < 0) continue;

        // Extract key (trimmed)
        usz ks = ls;
        usz ke = (usz)eq_pos;
        while (ke > ks && toml_is_space(source[ke - 1])) ke--;
        char[] key = source[ks .. ke - 1]; // inclusive

        // Extract value (trimmed)
        usz vs = (usz)eq_pos + 1;
        while (vs < le && toml_is_space(source[vs])) vs++;
        usz ve = toml_trim_value_comment_end(source, vs, le);
        char[] val = "";
        if (ve > vs) {
            val = source[vs .. ve - 1]; // inclusive
        }

        toml_assign_value(key, val, section_kind, current_dep, config);
    }
}

/**
 * Parse a section header like "project" or "dependencies.ffi.math"
 */
fn void toml_parse_section(char[] header, int* section_kind, usz* current_dep, TomlConfig* config) {
    if (header.len == 7 && toml_str_match(header, "project")) {
        *section_kind = 1;
        return;
    }

    if (header.len == 5 && toml_str_match(header, "build")) {
        *section_kind = 3;
        return;
    }

    // Check for "dependencies.ffi.NAME"
    char[] prefix = "dependencies.ffi.";
    if (header.len > prefix.len && toml_str_match(header[:prefix.len], prefix)) {
        char[] dep_name = header[prefix.len ..];
        if (config.ffi_dep_count < TOML_MAX_DEPS) {
            bool duplicate_dep_name = false;
            if (dep_name.len <= 63) {
                for (usz prev = 0; prev < config.ffi_dep_count; prev++) {
                    if (toml_cstr_match_slice(&config.ffi_deps[prev].name, dep_name)) {
                        config.ffi_deps[prev].name_invalid = true;
                        duplicate_dep_name = true;
                    }
                }
            }

            usz idx = config.ffi_dep_count;
            config.ffi_dep_count++;

            TomlFfiDep* dep = &config.ffi_deps[idx];
            for (usz i = 0; i < 64; i++) dep.name[i] = 0;
            for (usz i = 0; i < 128; i++) dep.library[i] = 0;
            dep.name_invalid = duplicate_dep_name || dep_name.len > 63;
            dep.library_seen = false;
            dep.library_invalid = false;
            toml_set_default(&dep.raw_syntax, 16, "legacy");
            dep.raw_syntax_seen = false;
            dep.raw_syntax_invalid = false;
            toml_set_default(&dep.mode, 16, "abi");
            dep.mode_seen = false;
            dep.mode_invalid = false;
            toml_set_default(&dep.generator, 32, "clang");
            dep.generator_seen = false;
            dep.generator_invalid = false;
            dep.header_count = 0;
            dep.headers_seen = false;
            dep.headers_invalid = false;
            dep.function_count = 0;
            dep.filter_functions = false;
            dep.functions_seen = false;
            dep.functions_invalid = false;
            dep.exclude_function_count = 0;
            dep.exclude_functions_seen = false;
            dep.exclude_functions_invalid = false;
            dep.strip_prefix_count = 0;
            dep.strip_prefixes_seen = false;
            dep.strip_prefixes_invalid = false;
            dep.unknown_key_invalid = false;

            if (dep_name.len <= 63) {
                for (usz i = 0; i < dep_name.len; i++) dep.name[i] = dep_name[i];
            }

            *current_dep = idx;
            *section_kind = 2;
        } else {
            config.ffi_dep_limit_hit = true;
            *section_kind = 0;
        }
        return;
    }

    *section_kind = 0;
}

/**
 * Assign a key=value pair to the appropriate config field.
 */
fn void toml_assign_value(char[] key, char[] val, int section_kind, usz current_dep, TomlConfig* config) {
    if (section_kind == 1) {
        // [project]
        if (toml_str_match(key, "name")) {
            toml_copy_quoted(val, &config.project.name, 127);
        } else if (toml_str_match(key, "version")) {
            toml_copy_quoted(val, &config.project.version, 31);
        }
    } else if (section_kind == 3) {
        // [build]
        if (toml_str_match(key, "output-dir")) {
            toml_copy_quoted(val, &config.build.output_dir, 127);
        } else if (toml_str_match(key, "safety")) {
            toml_copy_quoted(val, &config.build.safety, 15);
        } else if (toml_str_match(key, "opt")) {
            toml_copy_quoted(val, &config.build.opt, 7);
        } else if (toml_str_match(key, "debug-info")) {
            toml_copy_quoted(val, &config.build.debug_info, 15);
        } else if (toml_str_match(key, "single-module")) {
            config.build.single_module = toml_str_match(val, "true");
        } else if (toml_str_match(key, "sanitize")) {
            toml_copy_quoted(val, &config.build.sanitize, 15);
        }
    } else if (section_kind == 2 && current_dep < config.ffi_dep_count) {
        // [dependencies.ffi.X]
        TomlFfiDep* dep = &config.ffi_deps[current_dep];

        if (toml_str_match(key, "library")) {
            if (dep.library_seen) {
                dep.library_invalid = true;
            } else {
                dep.library_seen = true;
                dep.library_invalid = !toml_copy_quoted_strict(val, &dep.library, 127, false);
            }
        } else if (toml_str_match(key, "raw-syntax")) {
            if (dep.raw_syntax_seen) {
                dep.raw_syntax_invalid = true;
            } else {
                dep.raw_syntax_seen = true;
                dep.raw_syntax_invalid = !toml_copy_quoted_strict(val, &dep.raw_syntax, 15, false);
            }
        } else if (toml_str_match(key, "mode")) {
            if (dep.mode_seen) {
                dep.mode_invalid = true;
            } else {
                dep.mode_seen = true;
                dep.mode_invalid = !toml_copy_quoted_strict(val, &dep.mode, 15, false);
            }
        } else if (toml_str_match(key, "generator")) {
            if (dep.generator_seen) {
                dep.generator_invalid = true;
            } else {
                dep.generator_seen = true;
                dep.generator_invalid = !toml_copy_quoted_strict(val, &dep.generator, 31, false);
            }
        } else if (toml_str_match(key, "headers")) {
            if (dep.headers_seen) {
                dep.headers_invalid = true;
            } else {
                dep.headers_seen = true;
                dep.headers_invalid = !toml_parse_string_array_strict(
                    val,
                    &dep.headers,
                    &dep.header_count,
                    TOML_MAX_HEADERS,
                    TOML_MAX_STR
                ) || dep.header_count == 0;
            }
        } else if (toml_str_match(key, "functions")) {
            if (dep.functions_seen) {
                dep.functions_invalid = true;
            } else {
                dep.functions_seen = true;
                dep.filter_functions = true;
                dep.functions_invalid = !toml_parse_string_array_strict(
                    val,
                    &dep.functions,
                    &dep.function_count,
                    TOML_MAX_FUNCTIONS,
                    64
                ) || dep.function_count == 0;
            }
        } else if (toml_str_match(key, "exclude-functions")) {
            if (dep.exclude_functions_seen) {
                dep.exclude_functions_invalid = true;
            } else {
                dep.exclude_functions_seen = true;
                dep.exclude_functions_invalid = !toml_parse_string_array_strict(
                    val,
                    &dep.exclude_functions,
                    &dep.exclude_function_count,
                    TOML_MAX_FUNCTIONS,
                    64
                ) || dep.exclude_function_count == 0;
            }
        } else if (toml_str_match(key, "strip-prefixes")) {
            if (dep.strip_prefixes_seen) {
                dep.strip_prefixes_invalid = true;
            } else {
                dep.strip_prefixes_seen = true;
                dep.strip_prefixes_invalid = !toml_parse_string_array_strict(
                    val,
                    &dep.strip_prefixes,
                    &dep.strip_prefix_count,
                    TOML_MAX_STRIP_PREFIXES,
                    64
                );
            }
        } else {
            dep.unknown_key_invalid = true;
        }
    }
}

/**
 * Compare a slice against a string literal.
 */
fn bool toml_str_match(char[] slice, char[] target) {
    if (slice.len != target.len) return false;
    for (usz i = 0; i < slice.len; i++) {
        if (slice[i] != target[i]) return false;
    }
    return true;
}

fn bool toml_cstr_match_slice(char* text, char[] target) {
    if (text == null) return false;
    usz i = 0;
    while (i < target.len && text[i] != 0) {
        if (text[i] != target[i]) return false;
        i++;
    }
    return i == target.len && text[i] == 0;
}

/**
 * Copy a quoted string value ("...") into a fixed buffer.
 */
fn void toml_copy_quoted(char[] val, void* dest_ptr, usz max_len) {
    char* dest = (char*)dest_ptr;
    usz start = 0;
    usz end = val.len;

    if (val.len >= 2 && val[0] == '"' && val[val.len - 1] == '"') {
        start = 1;
        end = val.len - 1;
    }

    usz copy_len = end - start;
    if (copy_len > max_len) copy_len = max_len;
    for (usz i = 0; i < copy_len; i++) {
        dest[i] = val[start + i];
    }
    dest[copy_len] = 0;
}

fn bool toml_copy_escaped_string_content(char[] content, void* dest_ptr, usz max_len, bool allow_empty) {
    char* dest = (char*)dest_ptr;
    usz copy_len = 0;
    for (usz i = 0; i < content.len; i++) {
        char ch = content[i];
        if (((uint)ch & 0xFF) < 0x20) return false;
        if (ch == '"') return false;
        if (ch == '\\') {
            if (i + 1 >= content.len) return false;
            i++;
            switch (content[i]) {
                case '"':
                    ch = '"';
                case '\\':
                    ch = '\\';
                case 'n':
                    ch = '\n';
                case 'r':
                    ch = '\r';
                case 't':
                    ch = '\t';
                case 'b':
                    ch = (char)8;
                case 'f':
                    ch = (char)12;
                case 'u':
                    uint codepoint = 0;
                    if (!toml_read_hex_codepoint(content, i + 1, 4, &codepoint)) return false;
                    if (!toml_append_utf8_codepoint(dest, &copy_len, max_len, codepoint)) return false;
                    i += 4;
                    continue;
                case 'U':
                    uint long_codepoint = 0;
                    if (!toml_read_hex_codepoint(content, i + 1, 8, &long_codepoint)) return false;
                    if (!toml_append_utf8_codepoint(dest, &copy_len, max_len, long_codepoint)) return false;
                    i += 8;
                    continue;
                default:
                    return false;
            }
        }
        if (!toml_append_decoded_byte(dest, &copy_len, max_len, ch)) return false;
    }
    if (!allow_empty && copy_len == 0) return false;
    dest[copy_len] = 0;
    return true;
}

fn bool toml_copy_quoted_strict(char[] val, void* dest_ptr, usz max_len, bool allow_empty) {
    toml_clear_buf(dest_ptr, max_len + 1);
    if (val.len < 2 || val[0] != '"' || val[val.len - 1] != '"') return false;
    if (toml_quote_is_escaped(val, 0, val.len - 1)) return false;
    char[] content = "";
    if (val.len > 2) content = val[1 .. val.len - 2];
    return toml_copy_escaped_string_content(content, dest_ptr, max_len, allow_empty);
}

/**
 * Parse a string array value: ["a", "b", "c"]
 */
fn void toml_parse_string_array(char[] val, void* dest_ptr, usz* count, usz max_items, usz item_size) {
    char* dest = (char*)dest_ptr;
    *count = 0;

    usz start = 0;
    while (start < val.len && val[start] != '[') start++;
    if (start >= val.len) return;
    start++;

    usz end = val.len;
    while (end > start && val[end - 1] != ']') end--;
    if (end <= start) return;
    end--;

    usz pos = start;
    while (pos < end && *count < max_items) {
        while (pos < end && (val[pos] == ' ' || val[pos] == ',' || val[pos] == '\t')) pos++;
        if (pos >= end) break;

        if (val[pos] == '"') {
            pos++;
            usz str_start = pos;
            while (pos < end && val[pos] != '"') pos++;
            usz str_len = pos - str_start;
            if (pos < end) pos++;

            if (str_len >= item_size) str_len = item_size - 1;
            usz offset = *count * item_size;
            for (usz i = 0; i < str_len; i++) {
                dest[offset + i] = val[str_start + i];
            }
            dest[offset + str_len] = 0;
            (*count)++;
        } else {
            pos++;
        }
    }
}

fn void toml_clear_string_array(void* dest_ptr, usz max_items, usz item_size) {
    char* dest = (char*)dest_ptr;
    for (usz i = 0; i < max_items * item_size; i++) dest[i] = 0;
}

fn bool toml_parse_string_array_strict(char[] val, void* dest_ptr, usz* count, usz max_items, usz item_size) {
    char* dest = (char*)dest_ptr;
    *count = 0;
    if (item_size == 0) return false;
    toml_clear_string_array(dest_ptr, max_items, item_size);

    usz start = 0;
    while (start < val.len && toml_is_space(val[start])) start++;
    if (start >= val.len || val[start] != '[') return false;
    start++;

    usz end = val.len;
    while (end > start && toml_is_space(val[end - 1])) end--;
    if (end <= start || val[end - 1] != ']') return false;
    end--;

    usz pos = start;
    while (pos < end) {
        while (pos < end && toml_is_space(val[pos])) pos++;
        if (pos >= end) break;
        if (val[pos] != '"') {
            *count = 0;
            toml_clear_string_array(dest_ptr, max_items, item_size);
            return false;
        }

        usz quote_pos = pos;
        pos++;
        usz str_start = pos;
        usz str_end = toml_find_closing_quote(val, quote_pos, end);
        if (str_end >= end) {
            *count = 0;
            toml_clear_string_array(dest_ptr, max_items, item_size);
            return false;
        }
        pos = str_end + 1;

        if (*count >= max_items) {
            *count = 0;
            toml_clear_string_array(dest_ptr, max_items, item_size);
            return false;
        }

        usz offset = *count * item_size;
        char[] content = "";
        if (str_end > str_start) content = val[str_start .. str_end - 1];
        if (!toml_copy_escaped_string_content(content, &dest[offset], item_size - 1, false)) {
            *count = 0;
            toml_clear_string_array(dest_ptr, max_items, item_size);
            return false;
        }
        (*count)++;

        while (pos < end && toml_is_space(val[pos])) pos++;
        if (pos < end) {
            if (val[pos] != ',') {
                *count = 0;
                toml_clear_string_array(dest_ptr, max_items, item_size);
                return false;
            }
            pos++;
            if (pos >= end) {
                break;
            }
        }
    }
    return true;
}
