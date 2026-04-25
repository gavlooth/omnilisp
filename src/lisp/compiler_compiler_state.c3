module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 1: COMPILER STATE
// =============================================================================

/**
 * Compiler — Translates Lisp AST to C3 source code.
 *
 * Generated code calls aot:: wrapper functions (src/lisp/aot.c3)
 * which delegate to the JIT interpreter infrastructure (Value*,
 * scope-region memory, primitives, StackCtx-based effects).
 */
struct Compiler {
    Interp*     interp;          // For symbol table access
    DString     output;          // Generated C3 code (dynamic string)
    usz         indent;          // Current indentation level
    usz         temp_counter;    // For generating unique temp names
    usz         lambda_counter;  // For generating lambda function names
    bool        has_error;       // Hard compile failure encountered
    char[]      error_message;   // First hard compile failure message
    char[256]   error_storage;   // Backing storage for formatted compile errors

    // Track captured variables for closure generation
    List{SymbolId} current_captures;

    // Track defined globals for forward declarations
    List{SymbolId} defined_globals;

    // Lambda definitions to emit after scanning
    List{LambdaDef} lambda_defs;

    // Track which lambdas create nested closures
    bool[256] lambda_creates_closure;

    // Track mutable-captured variables (stored in interpreter env, not C3 locals)
    List{SymbolId} mutable_captures;

    // Track variables declared in current function scope (to avoid re-declaration/shadowing)
    List{SymbolId} declared_vars;

    // D2: Track referenced primitives for global caching
    List{SymbolId} referenced_prims;

    // Scoped module-open aliases active during AOT emission.
    List{SymbolId} module_open_exports;
    List{SymbolId} module_open_aliases;

    // Module-private bindings active while lowering an inline module body.
    List{SymbolId} module_private_symbols;
    List{SymbolId} module_private_backings;

    // When true, emit aot::print_value for the last non-define expression
    bool print_last;

    // When true, emit aot::print_value for EVERY non-define expression
    bool print_all;

    // When true, find_free_vars uses is_builtin_primitive instead of is_primitive
    // (for delegation contexts where user globals need injection into interpreter env)
    bool for_delegation;

// Compiled modules: track module name, exported symbols, and private AOT storage.
    usz compiled_module_count;
    usz compiled_module_capacity;
    CompiledModule* compiled_modules;
}

/**
 * CompiledModule — Tracks a module compiled inline for import resolution.
 * `locals` are the module-facing names and `local_backings` are generated
 * C3 globals that hold their values without publishing user-facing globals.
 */
struct CompiledModule {
    SymbolId     name;
    SymbolId*    exports;       // heap-allocated
    usz          export_count;
    usz          export_capacity;
    SymbolId*    locals;        // heap-allocated, module-local names
    SymbolId*    local_backings;// heap-allocated, parallel to locals
    usz          local_count;
    usz          local_capacity;
    bool         loaded;
}

/**
 * LambdaDef — Stored lambda definition for deferred emission.
 */
const usz LAMBDA_CAPTURE_INITIAL = 16;

struct LambdaDef {
    usz         id;              // Lambda ID
    Expr*       expr;            // Original lambda expression node
    SymbolId    param;           // Parameter name (for single-param lambdas)
    Expr*       body;            // Body expression
    SymbolId*   captures;        // Captured variables (heap-allocated)
    usz         capture_count;
    usz         capture_capacity;
    bool        creates_closure; // Does this lambda create nested closures?
    // Variadic support
    bool        has_rest;        // True if variadic lambda (.. rest)
    SymbolId    rest_param;      // Rest parameter name
    SymbolId*   params;          // All fixed params (heap-allocated)
    usz         param_count;     // Number of fixed params (for variadic)
    usz         param_capacity;
}

// Compiler allocation-failure seams (tests only).
bool g_compiler_force_program_source_alloc_fail = false;
bool g_compiler_force_compiled_modules_init_alloc_fail = false;
bool g_compiler_force_module_registry_alloc_fail = false;
bool g_compiler_force_module_exports_alloc_fail = false;
bool g_compiler_force_compiled_modules_init_size_overflow = false;
bool g_compiler_force_module_registry_growth_overflow = false;
bool g_compiler_force_module_exports_size_overflow = false;
bool g_compiler_force_lambda_capture_alloc_fail = false;
bool g_compiler_force_lambda_param_alloc_fail = false;
bool g_compiler_force_lambda_capture_growth_alloc_fail = false;
bool g_compiler_force_lambda_capture_size_overflow = false;
bool g_compiler_force_lambda_param_size_overflow = false;
bool g_compiler_force_lambda_lookup_miss = false;
bool g_compiler_force_lambda_emit_lookup_miss = false;
bool g_compiler_force_call_arg_temps_alloc_fail = false;

fn bool compiler_checked_mul_usz(usz left, usz right, usz* out) @inline {
    if (left == 0 || right == 0) {
        *out = 0;
        return true;
    }
    if (left > usz.max / right) return false;
    *out = left * right;
    return true;
}
