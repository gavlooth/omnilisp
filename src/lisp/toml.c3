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
const usz TOML_MAX_FUNCTION_MATCHES = 128;
const usz TOML_MAX_STRIP_PREFIXES = 16;
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
    bool name_invalid;                 // true when dependency section name must fail closed
    char[128] library;                 // Shared lib name (e.g., "m")
    bool library_seen;                 // true after first library key
    bool library_invalid;              // true when library parse must fail closed
    char[16] raw_syntax;               // "legacy" (default) or "grouped"
    bool raw_syntax_seen;              // true after first raw-syntax key
    bool raw_syntax_invalid;           // true when raw-syntax parse must fail closed
    char[16] mode;                     // "abi" (default) or "api"
    bool mode_seen;                    // true after first mode key
    bool mode_invalid;                 // true when mode parse must fail closed
    char[32] generator;                // "clang" (default) or "cppinterop"
    bool generator_seen;               // true after first generator key
    bool generator_invalid;            // true when generator parse must fail closed
    char[TOML_MAX_STR][TOML_MAX_HEADERS] headers;
    usz header_count;
    bool headers_seen;                 // true after first headers key
    bool headers_invalid;              // true when headers parse must fail closed
    char[64][TOML_MAX_FUNCTIONS] functions;
    usz function_count;
    bool filter_functions;             // true if functions list specified
    bool functions_seen;               // true after first functions key
    bool functions_invalid;            // true when functions parse must fail closed
    char[64][TOML_MAX_FUNCTIONS] exclude_functions;
    usz exclude_function_count;
    bool exclude_functions_seen;       // true after first exclude-functions key
    bool exclude_functions_invalid;    // true when exclude-functions parse must fail closed
    char[64][TOML_MAX_STRIP_PREFIXES] strip_prefixes;
    usz strip_prefix_count;
    bool strip_prefixes_seen;          // true after first strip-prefixes key
    bool strip_prefixes_invalid;       // true when strip-prefixes parse must fail closed
    bool unknown_key_invalid;          // true when dependency section contains unsupported keys
}

struct TomlConfig {
    TomlProject project;
    TomlBuild build;
    TomlFfiDep[TOML_MAX_DEPS] ffi_deps;
    usz ffi_dep_count;
    bool ffi_dep_limit_hit;             // true when dependency sections exceed TOML_MAX_DEPS
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
