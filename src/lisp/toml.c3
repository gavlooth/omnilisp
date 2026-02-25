module lisp;

import std::io;

// =============================================================================
// Minimal TOML Parser — supports only what omni.toml needs:
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

// Build configuration — controls project.json generation and C3 compiler flags
struct TomlBuild {
    char[128] output_dir;              // Build output directory (default: "build")
    char[16] safety;                   // "safe" (default), "fast", "none"
    char[8] opt;                       // "O0" (default), "O1", "O2", "O3", "Os", "Oz"
    char[16] debug_info;               // "full" (default), "line-tables", "none"
    bool single_module;                // Compile all modules together (default: false)
    char[16] sanitize;                 // "none" (default), "address", "memory", "thread"
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
    TomlBuild build;
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

    // Build defaults
    for (usz i = 0; i < 128; i++) config.build.output_dir[i] = 0;
    char[] default_out = "build";
    for (usz i = 0; i < default_out.len; i++) config.build.output_dir[i] = default_out[i];
    for (usz i = 0; i < 16; i++) config.build.safety[i] = 0;
    char[] default_safety = "safe";
    for (usz i = 0; i < default_safety.len; i++) config.build.safety[i] = default_safety[i];
    for (usz i = 0; i < 8; i++) config.build.opt[i] = 0;
    char[] default_opt = "O0";
    for (usz i = 0; i < default_opt.len; i++) config.build.opt[i] = default_opt[i];
    for (usz i = 0; i < 16; i++) config.build.debug_info[i] = 0;
    char[] default_dbg = "full";
    for (usz i = 0; i < default_dbg.len; i++) config.build.debug_info[i] = default_dbg[i];
    config.build.single_module = false;
    for (usz i = 0; i < 16; i++) config.build.sanitize[i] = 0;
    char[] default_san = "none";
    for (usz i = 0; i < default_san.len; i++) config.build.sanitize[i] = default_san[i];

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

    if (header.len == 5 && toml_str_match(header, "build")) {
        *section_kind = 3;
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
