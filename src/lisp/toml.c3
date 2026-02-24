module lisp;

import std::io;

// =============================================================================
// Minimal TOML Parser — supports only what project.toml needs:
//   [section] and [section.sub.name] headers
//   key = "value" string assignments
//   key = ["a", "b"] string array assignments
//   # comments and blank lines
// =============================================================================

const usz TOML_MAX_DEPS = 16;
const usz TOML_MAX_HEADERS = 16;
const usz TOML_MAX_FUNCTIONS = 64;
const usz TOML_MAX_STR = 256;

struct TomlProject {
    char[128] name;
    char[32] version;
}

struct TomlFfiDep {
    char[64] name;                     // Section key (e.g., "math")
    char[128] library;                 // Shared lib name (e.g., "m")
    char[TOML_MAX_STR][TOML_MAX_HEADERS] headers;
    usz header_count;
    char[64][TOML_MAX_FUNCTIONS] functions;
    usz function_count;
    bool filter_functions;             // true if functions list specified
}

struct TomlConfig {
    TomlProject project;
    TomlFfiDep[TOML_MAX_DEPS] ffi_deps;
    usz ffi_dep_count;
}

/**
 * Load and parse a TOML file from disk.
 * Config must be heap-allocated by caller (too large for stack).
 */
fn bool toml_load(char[] path, TomlConfig* out) {
    if (try source = io::file::load_temp((String)path)) {
        parse_toml(source, out);
        return true;
    }
    return false;
}

/**
 * Parse TOML source text into caller-provided TomlConfig.
 */
fn void parse_toml(char[] source, TomlConfig* config) {
    // Zero-init key fields
    for (usz i = 0; i < 128; i++) config.project.name[i] = 0;
    for (usz i = 0; i < 32; i++) config.project.version[i] = 0;
    config.ffi_dep_count = 0;

    // Track current section
    // 0 = none, 1 = [project], 2 = [dependencies.ffi.X]
    int section_kind = 0;
    usz current_dep = 0;

    usz pos = 0;
    while (pos < source.len) {
        // Find line end
        usz line_start = pos;
        while (pos < source.len && source[pos] != '\n') pos++;
        usz line_end = pos;
        if (pos < source.len) pos++; // skip \n

        // Trim leading whitespace
        usz ls = line_start;
        while (ls < line_end && (source[ls] == ' ' || source[ls] == '\t')) ls++;

        // Trim trailing whitespace + \r
        usz le = line_end;
        while (le > ls && (source[le - 1] == ' ' || source[le - 1] == '\t' || source[le - 1] == '\r')) le--;

        usz line_len = le - ls;
        if (line_len == 0) continue;        // blank line
        if (source[ls] == '#') continue;    // comment

        // Section header: [...]
        if (source[ls] == '[' && source[le - 1] == ']') {
            char[] header = source[ls + 1 .. le - 2]; // inclusive: content between [ ]
            toml_parse_section(header, &section_kind, &current_dep, config);
            continue;
        }

        // Key = value — find '='
        isz eq_pos = -1;
        for (usz i = ls; i < le; i++) {
            if (source[i] == '=') { eq_pos = (isz)i; break; }
        }
        if (eq_pos < 0) continue;

        // Extract key (trimmed)
        usz ks = ls;
        usz ke = (usz)eq_pos;
        while (ke > ks && (source[ke - 1] == ' ' || source[ke - 1] == '\t')) ke--;
        char[] key = source[ks .. ke - 1]; // inclusive

        // Extract value (trimmed)
        usz vs = (usz)eq_pos + 1;
        while (vs < le && (source[vs] == ' ' || source[vs] == '\t')) vs++;
        char[] val = source[vs .. le - 1]; // inclusive

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

    // Check for "dependencies.ffi.NAME"
    char[] prefix = "dependencies.ffi.";
    if (header.len > prefix.len && toml_str_match(header[:prefix.len], prefix)) {
        char[] dep_name = header[prefix.len ..];
        if (config.ffi_dep_count < TOML_MAX_DEPS) {
            usz idx = config.ffi_dep_count;
            config.ffi_dep_count++;

            TomlFfiDep* dep = &config.ffi_deps[idx];
            for (usz i = 0; i < 64; i++) dep.name[i] = 0;
            for (usz i = 0; i < 128; i++) dep.library[i] = 0;
            dep.header_count = 0;
            dep.function_count = 0;
            dep.filter_functions = false;

            usz copy_len = dep_name.len;
            if (copy_len > 63) copy_len = 63;
            for (usz i = 0; i < copy_len; i++) dep.name[i] = dep_name[i];

            *current_dep = idx;
            *section_kind = 2;
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
        if (toml_str_match(key, "name")) {
            toml_copy_quoted(val, &config.project.name, 127);
        } else if (toml_str_match(key, "version")) {
            toml_copy_quoted(val, &config.project.version, 31);
        }
    } else if (section_kind == 2 && current_dep < config.ffi_dep_count) {
        TomlFfiDep* dep = &config.ffi_deps[current_dep];

        if (toml_str_match(key, "library")) {
            toml_copy_quoted(val, &dep.library, 127);
        } else if (toml_str_match(key, "headers")) {
            toml_parse_string_array(val, &dep.headers, &dep.header_count, TOML_MAX_HEADERS, TOML_MAX_STR);
        } else if (toml_str_match(key, "functions")) {
            toml_parse_string_array(val, &dep.functions, &dep.function_count, TOML_MAX_FUNCTIONS, 64);
            dep.filter_functions = true;
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
