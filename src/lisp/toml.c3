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
