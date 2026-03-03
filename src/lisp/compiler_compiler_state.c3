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

    // When true, emit aot::print_value for the last non-define expression
    bool print_last;

    // When true, emit aot::print_value for EVERY non-define expression
    bool print_all;

    // When true, find_free_vars uses is_builtin_primitive instead of is_primitive
    // (for delegation contexts where user globals need injection into interpreter env)
    bool for_delegation;

    // Compiled modules: track module name → exported symbols
    usz compiled_module_count;
    usz compiled_module_capacity;
    CompiledModule* compiled_modules;
}

/**
 * CompiledModule — Tracks a module compiled inline for import resolution.
 */
struct CompiledModule {
    SymbolId     name;
    SymbolId*    exports;       // heap-allocated
    usz          export_count;
    usz          export_capacity;
    bool         loaded;
}

/**
 * LambdaDef — Stored lambda definition for deferred emission.
 */
const usz LAMBDA_CAPTURE_INITIAL = 16;

struct LambdaDef {
    usz         id;              // Lambda ID
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

