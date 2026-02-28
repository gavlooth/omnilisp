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

// =============================================================================
// SECTION 1b: PRIMITIVE VARIABLE HASH TABLE
// =============================================================================

const usz PRIM_HASH_SIZE = 256;  // Power of 2, > 2*87
struct PrimHashEntry {
    SymbolId key;
    char[]   emit;
}

// Module-level hash table for primitive variable → C3 code mapping
PrimHashEntry[PRIM_HASH_SIZE] g_prim_hash;
bool g_prim_hash_initialized = false;

fn void prim_hash_insert(SymbolId key, char[] emit) {
    usz idx = ((usz)key) & (PRIM_HASH_SIZE - 1);
    for (usz i = 0; i < PRIM_HASH_SIZE; i++) {
        usz slot = (idx + i) & (PRIM_HASH_SIZE - 1);
        if ((uint)g_prim_hash[slot].key == 0xFFFFFFFF) {
            g_prim_hash[slot].key = key;
            g_prim_hash[slot].emit = emit;
            return;
        }
    }
}

fn char[] prim_hash_lookup(SymbolId key) {
    usz idx = ((usz)key) & (PRIM_HASH_SIZE - 1);
    for (usz i = 0; i < PRIM_HASH_SIZE; i++) {
        usz slot = (idx + i) & (PRIM_HASH_SIZE - 1);
        if ((uint)g_prim_hash[slot].key == 0xFFFFFFFF) return "";
        if ((uint)g_prim_hash[slot].key == (uint)key) return g_prim_hash[slot].emit;
    }
    return "";
}

fn void init_prim_hash(Interp* interp) {
    if (g_prim_hash_initialized) return;
    g_prim_hash_initialized = true;

    // Clear
    for (usz i = 0; i < PRIM_HASH_SIZE; i++) {
        g_prim_hash[i].key = (SymbolId)0xFFFFFFFF;
    }

    SymbolTable* st = &interp.symbols;

    // All primitives looked up by name at runtime via aot::lookup_prim
    // Arithmetic
    prim_hash_insert(st.intern("+"), "aot::lookup_prim(\"+\")");
    prim_hash_insert(st.intern("-"), "aot::lookup_prim(\"-\")");
    prim_hash_insert(st.intern("*"), "aot::lookup_prim(\"*\")");
    prim_hash_insert(st.intern("/"), "aot::lookup_prim(\"/\")");
    prim_hash_insert(st.intern("%"), "aot::lookup_prim(\"%\")");

    // Comparison
    prim_hash_insert(st.intern("="), "aot::lookup_prim(\"=\")");
    prim_hash_insert(st.intern("<"), "aot::lookup_prim(\"<\")");
    prim_hash_insert(st.intern(">"), "aot::lookup_prim(\">\")");
    prim_hash_insert(st.intern("<="), "aot::lookup_prim(\"<=\")");
    prim_hash_insert(st.intern(">="), "aot::lookup_prim(\">=\")");

    // List ops
    prim_hash_insert(st.intern("cons"), "aot::lookup_prim(\"cons\")");
    prim_hash_insert(st.intern("car"), "aot::lookup_prim(\"car\")");
    prim_hash_insert(st.intern("cdr"), "aot::lookup_prim(\"cdr\")");
    prim_hash_insert(st.intern("null?"), "aot::lookup_prim(\"null?\")");
    prim_hash_insert(st.intern("pair?"), "aot::lookup_prim(\"pair?\")");
    prim_hash_insert(st.intern("list"), "aot::lookup_prim(\"list\")");
    prim_hash_insert(st.intern("length"), "aot::lookup_prim(\"length\")");
    prim_hash_insert(st.intern("not"), "aot::lookup_prim(\"not\")");

    // I/O
    prim_hash_insert(st.intern("print"), "aot::lookup_prim(\"print\")");
    prim_hash_insert(st.intern("println"), "aot::lookup_prim(\"println\")");
    prim_hash_insert(st.intern("newline"), "aot::lookup_prim(\"newline\")");
    prim_hash_insert(st.intern("display"), "aot::lookup_prim(\"display\")");

    // String ops
    prim_hash_insert(st.intern("string-append"), "aot::lookup_prim(\"string-append\")");
    prim_hash_insert(st.intern("string-join"), "aot::lookup_prim(\"string-join\")");
    prim_hash_insert(st.intern("substring"), "aot::lookup_prim(\"substring\")");
    prim_hash_insert(st.intern("string-split"), "aot::lookup_prim(\"string-split\")");
    prim_hash_insert(st.intern("string-length"), "aot::lookup_prim(\"string-length\")");
    prim_hash_insert(st.intern("string->list"), "aot::lookup_prim(\"string->list\")");
    prim_hash_insert(st.intern("list->string"), "aot::lookup_prim(\"list->string\")");
    prim_hash_insert(st.intern("string-upcase"), "aot::lookup_prim(\"string-upcase\")");
    prim_hash_insert(st.intern("string-downcase"), "aot::lookup_prim(\"string-downcase\")");
    prim_hash_insert(st.intern("string-trim"), "aot::lookup_prim(\"string-trim\")");
    prim_hash_insert(st.intern("string->number"), "aot::lookup_prim(\"string->number\")");
    prim_hash_insert(st.intern("number->string"), "aot::lookup_prim(\"number->string\")");

    // Type predicates
    prim_hash_insert(st.intern("string?"), "aot::lookup_prim(\"string?\")");
    prim_hash_insert(st.intern("int?"), "aot::lookup_prim(\"int?\")");
    prim_hash_insert(st.intern("symbol?"), "aot::lookup_prim(\"symbol?\")");
    prim_hash_insert(st.intern("closure?"), "aot::lookup_prim(\"closure?\")");
    prim_hash_insert(st.intern("continuation?"), "aot::lookup_prim(\"continuation?\")");
    prim_hash_insert(st.intern("double?"), "aot::lookup_prim(\"double?\")");
    prim_hash_insert(st.intern("list?"), "aot::lookup_prim(\"list?\")");
    prim_hash_insert(st.intern("boolean?"), "aot::lookup_prim(\"boolean?\")");
    prim_hash_insert(st.intern("number?"), "aot::lookup_prim(\"number?\")");
    prim_hash_insert(st.intern("equal?"), "aot::lookup_prim(\"equal?\")");
    prim_hash_insert(st.intern("type-of"), "aot::lookup_prim(\"type-of\")");

    // File I/O
    prim_hash_insert(st.intern("read-file"), "aot::lookup_prim(\"read-file\")");
    prim_hash_insert(st.intern("write-file"), "aot::lookup_prim(\"write-file\")");
    prim_hash_insert(st.intern("file-exists?"), "aot::lookup_prim(\"file-exists?\")");
    prim_hash_insert(st.intern("read-lines"), "aot::lookup_prim(\"read-lines\")");

    // Misc
    prim_hash_insert(st.intern("gensym"), "aot::lookup_prim(\"gensym\")");
    prim_hash_insert(st.intern("load"), "aot::lookup_prim(\"load\")");
    prim_hash_insert(st.intern("apply"), "aot::lookup_prim(\"apply\")");

    // Dict/collection ops
    prim_hash_insert(st.intern("dict"), "aot::lookup_prim(\"dict\")");
    prim_hash_insert(st.intern("dict-set!"), "aot::lookup_prim(\"dict-set!\")");
    prim_hash_insert(st.intern("dict?"), "aot::lookup_prim(\"dict?\")");
    prim_hash_insert(st.intern("ref"), "aot::lookup_prim(\"ref\")");
    prim_hash_insert(st.intern("keys"), "aot::lookup_prim(\"keys\")");
    prim_hash_insert(st.intern("values"), "aot::lookup_prim(\"values\")");
    prim_hash_insert(st.intern("has?"), "aot::lookup_prim(\"has?\")");
    prim_hash_insert(st.intern("remove!"), "aot::lookup_prim(\"remove!\")");
    prim_hash_insert(st.intern("push!"), "aot::lookup_prim(\"push!\")");

    // Math
    prim_hash_insert(st.intern("abs"), "aot::lookup_prim(\"abs\")");
    prim_hash_insert(st.intern("min"), "aot::lookup_prim(\"min\")");
    prim_hash_insert(st.intern("max"), "aot::lookup_prim(\"max\")");
    prim_hash_insert(st.intern("floor"), "aot::lookup_prim(\"floor\")");
    prim_hash_insert(st.intern("ceiling"), "aot::lookup_prim(\"ceiling\")");
    prim_hash_insert(st.intern("round"), "aot::lookup_prim(\"round\")");
    prim_hash_insert(st.intern("truncate"), "aot::lookup_prim(\"truncate\")");
    prim_hash_insert(st.intern("sqrt"), "aot::lookup_prim(\"sqrt\")");
    prim_hash_insert(st.intern("even?"), "aot::lookup_prim(\"even?\")");
    prim_hash_insert(st.intern("odd?"), "aot::lookup_prim(\"odd?\")");
    prim_hash_insert(st.intern("zero?"), "aot::lookup_prim(\"zero?\")");
    prim_hash_insert(st.intern("positive?"), "aot::lookup_prim(\"positive?\")");
    prim_hash_insert(st.intern("negative?"), "aot::lookup_prim(\"negative?\")");
    prim_hash_insert(st.intern("gcd"), "aot::lookup_prim(\"gcd\")");

    // Bitwise
    prim_hash_insert(st.intern("bitwise-and"), "aot::lookup_prim(\"bitwise-and\")");
    prim_hash_insert(st.intern("bitwise-or"), "aot::lookup_prim(\"bitwise-or\")");
    prim_hash_insert(st.intern("bitwise-xor"), "aot::lookup_prim(\"bitwise-xor\")");
    prim_hash_insert(st.intern("bitwise-not"), "aot::lookup_prim(\"bitwise-not\")");
    prim_hash_insert(st.intern("lshift"), "aot::lookup_prim(\"lshift\")");
    prim_hash_insert(st.intern("rshift"), "aot::lookup_prim(\"rshift\")");

    // Literals
    prim_hash_insert(st.intern("true"), "aot::make_true()");
    prim_hash_insert(st.intern("false"), "aot::make_false()");
    prim_hash_insert(st.intern("nil"), "aot::make_nil()");
}

// =============================================================================
// SECTION 2: COMPILER INITIALIZATION
// =============================================================================

fn void Compiler.init(Compiler* self, Interp* interp) {
    self.interp = interp;
    // Lists auto-initialize when declared
    self.indent = 0;
    self.temp_counter = 0;
    self.lambda_counter = 0;
    self.output.init(mem, 8192);
    init_prim_hash(interp);
    self.compiled_module_count = 0;
    self.compiled_module_capacity = 16;
    self.compiled_modules = (CompiledModule*)mem::malloc(CompiledModule.sizeof * 16);
}

fn void Compiler.free(Compiler* self) {
    self.output.free();
    self.current_captures.free();
    self.defined_globals.free();
    self.lambda_defs.free();
    self.mutable_captures.free();
    self.declared_vars.free();
    self.referenced_prims.free();
    for (usz i = 0; i < self.compiled_module_count; i++) {
        if (self.compiled_modules[i].exports != null) {
            mem::free(self.compiled_modules[i].exports);
        }
    }
    if (self.compiled_modules != null) {
        mem::free(self.compiled_modules);
        self.compiled_modules = null;
    }
}

fn bool Compiler.is_declared(Compiler* self, SymbolId sym) {
    foreach (v : self.declared_vars) {
        if (v == sym) return true;
    }
    return false;
}

fn bool Compiler.is_global(Compiler* self, SymbolId sym) {
    foreach (g : self.defined_globals) {
        if (g == sym) return true;
    }
    return false;
}


fn void Compiler.mark_declared(Compiler* self, SymbolId sym) {
    if (!self.is_declared(sym)) {
        self.declared_vars.push(sym);
    }
}

// =============================================================================
// SECTION 3: OUTPUT HELPERS
// =============================================================================

fn void Compiler.emit(Compiler* self, char[] s) {
    self.output.append_string((String)s);
}

fn void Compiler.emit_char(Compiler* self, char c) {
    self.output.append_char(c);
}

/**
 * Emit a string into the output with escaping for C3 string literal context.
 * Replaces \ with \\ and " with \" to prevent code injection via symbol names.
 */
fn void Compiler.emit_escaped(Compiler* self, char[] s) {
    foreach (c : s) {
        if (c == '"') {
            self.emit("\\\"");
        } else if (c == '\\') {
            self.emit("\\\\");
        } else if (c == '\n') {
            self.emit("\\n");
        } else if (c == '\t') {
            self.emit("\\t");
        } else {
            self.emit_char(c);
        }
    }
}

fn void Compiler.emit_line(Compiler* self, char[] s) {
    self.emit_indent();
    self.emit(s);
    self.emit_char('\n');
}

fn void Compiler.emit_indent(Compiler* self) {
    for (usz i = 0; i < self.indent; i++) {
        self.emit("    ");
    }
}

fn void Compiler.emit_newline(Compiler* self) {
    self.emit_char('\n');
}

fn void Compiler.emit_int(Compiler* self, long n) {
    // Convert integer to string manually
    if (n == 0) {
        self.emit_char('0');
        return;
    }

    bool negative = n < 0;
    if (negative) {
        self.emit_char('-');
        n = -n;
    }

    char[32] buf;
    usz pos = 0;
    while (n > 0 && pos < 31) {
        buf[pos++] = (char)('0' + (n % 10));
        n /= 10;
    }

    // Reverse
    for (isz i = (isz)pos - 1; i >= 0; i--) {
        self.emit_char(buf[(usz)i]);
    }
}

fn void Compiler.emit_usz(Compiler* self, usz n) {
    self.emit_int((long)n);
}

fn bool is_c3_reserved(char[] name) {
    char[][*] reserved = {"try", "var", "fn", "if", "else", "for", "while", "do",
        "switch", "case", "default", "return", "break", "continue", "import",
        "module", "struct", "enum", "union", "const", "static", "true", "false",
        "null", "void", "bool", "int", "uint", "char", "float", "double", "usz"};
    foreach (r : reserved) {
        if (name.len == r.len) {
            bool match = true;
            for (usz i = 0; i < name.len; i++) {
                if (name[i] != r[i]) { match = false; break; }
            }
            if (match) return true;
        }
    }
    return false;
}

fn void Compiler.emit_symbol_name(Compiler* self, SymbolId sym) {
    char[] name = self.interp.symbols.get_name(sym);
    // Escape C3 reserved words
    if (is_c3_reserved(name)) {
        self.emit("_omni_");
    }
    // Sanitize name for C3 (replace - with _)
    foreach (c : name) {
        if (c == '-') {
            self.emit_char('_');
        } else if (c == '?') {
            self.emit("_p");  // predicate suffix
        } else if (c == '!') {
            self.emit("_bang");
        } else if (c == '>') {
            self.emit("_to_");
        } else if (c == '<') {
            self.emit("_lt_");
        } else if (c == '=') {
            self.emit("_eq_");
        } else if (c == '+') {
            self.emit("_plus_");
        } else if (c == '*') {
            self.emit("_star_");
        } else if (c == '/') {
            self.emit("_slash_");
        } else if (c == '%') {
            self.emit("_mod_");
        } else {
            self.emit_char(c);
        }
    }
}

fn char[] Compiler.get_output(Compiler* self) {
    return self.output.str_view();
}

/**
 * D2: Emit the global variable name for a cached primitive.
 * Uses _prim_ prefix followed by the sanitized symbol name.
 */
fn void Compiler.emit_prim_global_name(Compiler* self, SymbolId sym) {
    self.emit("_prim_");
    self.emit_symbol_name(sym);
}

/**
 * D2: Record a primitive as referenced (for global caching).
 * Only adds if not already present.
 */
fn void Compiler.record_prim_ref(Compiler* self, SymbolId sym) {
    foreach (s : self.referenced_prims) {
        if ((uint)s == (uint)sym) return;
    }
    self.referenced_prims.push(sym);
}

// =============================================================================
// SECTION 4: FREE VARIABLE ANALYSIS
// =============================================================================

/**
 * Find free variables in an expression that need to be captured.
 * bound_vars contains variables that are bound in the current scope.
 */
fn void Compiler.find_free_vars(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    if (expr == null) return;

    switch (expr.tag) {
        case E_LIT:
            // Literals have no free variables
            return;

        case E_VAR:
            SymbolId name = expr.var_expr.name;
            // Check if bound locally (this lambda's own params/lets)
            foreach (b : *bound_vars) {
                if ((uint)b == (uint)name) return;
            }
            // Check if in enclosing scope (outer lambda params/lets) — must capture,
            // even if the name also exists as a global define
            if (enclosing_scope != null) {
                foreach (e : *enclosing_scope) {
                    if ((uint)e == (uint)name) {
                        foreach (f : *free_vars) {
                            if ((uint)f == (uint)name) return;
                        }
                        free_vars.push(name);
                        return;
                    }
                }
            }
            // Check if it's a global primitive (for delegation, only skip builtins)
            if (self.for_delegation) {
                if (self.is_builtin_primitive(name)) return;
            } else {
                if (self.is_primitive(name)) return;
            }
            // Check if it's a mutable-captured variable (accessed via interpreter env)
            if (self.is_mutable_captured_var(name)) return;
            // Check if already in free vars
            foreach (f : *free_vars) {
                if ((uint)f == (uint)name) return;
            }
            // It's a free variable
            free_vars.push(name);

        case E_LAMBDA:
            // Add ALL parameters to bound vars (multi-param lambdas have param_count > 1)
            List{SymbolId} new_bound;
            foreach (b : *bound_vars) {
                new_bound.push(b);
            }
            if (expr.lambda.param_count > 1) {
                for (usz pi = 0; pi < expr.lambda.param_count; pi++) {
                    new_bound.push(expr.lambda.params[pi]);
                }
            } else if ((uint)expr.lambda.param != 0xFFFFFFFF) {
                new_bound.push(expr.lambda.param);
            }
            self.find_free_vars(expr.lambda.body, &new_bound, free_vars, enclosing_scope);
            new_bound.free();

        case E_APP:
            self.find_free_vars(expr.app.func, bound_vars, free_vars, enclosing_scope);
            self.find_free_vars(expr.app.arg, bound_vars, free_vars, enclosing_scope);

        case E_IF:
            self.find_free_vars(expr.if_expr.test, bound_vars, free_vars, enclosing_scope);
            self.find_free_vars(expr.if_expr.then_branch, bound_vars, free_vars, enclosing_scope);
            self.find_free_vars(expr.if_expr.else_branch, bound_vars, free_vars, enclosing_scope);

        case E_LET:
            // For recursive let, name is bound in both init and body
            List{SymbolId} let_bound;
            foreach (b : *bound_vars) {
                let_bound.push(b);
            }
            if (expr.let_expr.is_recursive) {
                let_bound.push(expr.let_expr.name);
            }
            // Init expression
            self.find_free_vars(expr.let_expr.init, &let_bound, free_vars, enclosing_scope);
            // Body with name bound
            if (!expr.let_expr.is_recursive) {
                let_bound.push(expr.let_expr.name);
            }
            self.find_free_vars(expr.let_expr.body, &let_bound, free_vars, enclosing_scope);
            let_bound.free();

        case E_DEFINE:
            self.find_free_vars(expr.define.value, bound_vars, free_vars, enclosing_scope);

        case E_QUOTE:
            // Quoted data has no free variables
            return;

        case E_AND:
            self.find_free_vars(expr.and_expr.left, bound_vars, free_vars, enclosing_scope);
            self.find_free_vars(expr.and_expr.right, bound_vars, free_vars, enclosing_scope);

        case E_OR:
            self.find_free_vars(expr.or_expr.left, bound_vars, free_vars, enclosing_scope);
            self.find_free_vars(expr.or_expr.right, bound_vars, free_vars, enclosing_scope);

        case E_MATCH:
            self.find_free_vars(expr.match.scrutinee, bound_vars, free_vars, enclosing_scope);
            for (usz i = 0; i < expr.match.clause_count; i++) {
                // Add pattern bindings to bound vars
                List{SymbolId} clause_bound;
                foreach (b : *bound_vars) {
                    clause_bound.push(b);
                }
                self.collect_pattern_bindings(expr.match.clauses[i].pattern, &clause_bound);
                self.find_free_vars(expr.match.clauses[i].result, &clause_bound, free_vars, enclosing_scope);
                clause_bound.free();
            }

        case E_CALL:
            self.find_free_vars(expr.call.func, bound_vars, free_vars, enclosing_scope);
            for (usz i = 0; i < expr.call.arg_count; i++) {
                self.find_free_vars(expr.call.args[i], bound_vars, free_vars, enclosing_scope);
            }

        case E_INDEX:
            self.find_free_vars(expr.index.collection, bound_vars, free_vars, enclosing_scope);
            self.find_free_vars(expr.index.index, bound_vars, free_vars, enclosing_scope);

        case E_PATH:
            // First segment is a variable reference
            SymbolId path_name = expr.path.segments[0];
            foreach (b : *bound_vars) {
                if ((uint)b == (uint)path_name) return;
            }
            // Check enclosing scope before is_primitive (enclosing var shadows global)
            if (enclosing_scope != null) {
                foreach (e : *enclosing_scope) {
                    if ((uint)e == (uint)path_name) {
                        foreach (f : *free_vars) {
                            if ((uint)f == (uint)path_name) return;
                        }
                        free_vars.push(path_name);
                        return;
                    }
                }
            }
            if (self.is_primitive(path_name)) return;
            foreach (f : *free_vars) {
                if ((uint)f == (uint)path_name) return;
            }
            free_vars.push(path_name);

        case E_RESET:
            self.find_free_vars(expr.reset.body, bound_vars, free_vars, enclosing_scope);

        case E_SHIFT:
            // k_name is bound in the body
            List{SymbolId} shift_bound;
            foreach (b : *bound_vars) {
                shift_bound.push(b);
            }
            shift_bound.push(expr.shift.k_name);
            self.find_free_vars(expr.shift.body, &shift_bound, free_vars, enclosing_scope);
            shift_bound.free();

        case E_PERFORM:
            self.find_free_vars(expr.perform.arg, bound_vars, free_vars, enclosing_scope);

        case E_RESOLVE: {
            self.find_free_vars(expr.resolve.value, bound_vars, free_vars, enclosing_scope);
            bool k_bound = false;
            foreach (b : *bound_vars) {
                if ((uint)b == (uint)self.interp.sym__k) { k_bound = true; break; }
            }
            if (!k_bound && enclosing_scope != null) {
                foreach (e : *enclosing_scope) {
                    if ((uint)e == (uint)self.interp.sym__k) {
                        bool already_free = false;
                        foreach (f : *free_vars) {
                            if ((uint)f == (uint)self.interp.sym__k) { already_free = true; break; }
                        }
                        if (!already_free) free_vars.push(self.interp.sym__k);
                        break;
                    }
                }
            }
            break;
        }

        case E_HANDLE:
            self.find_free_vars(expr.handle.body, bound_vars, free_vars, enclosing_scope);
            for (usz i = 0; i < expr.handle.clause_count; i++) {
                // k_name and arg_name are bound in handler body
                List{SymbolId} handle_bound;
                foreach (b : *bound_vars) {
                    handle_bound.push(b);
                }
                handle_bound.push(expr.handle.clauses[i].k_name);
                handle_bound.push(expr.handle.clauses[i].arg_name);
                self.find_free_vars(expr.handle.clauses[i].handler_body, &handle_bound, free_vars, enclosing_scope);
                handle_bound.free();
            }

        case E_BEGIN:
            for (usz i = 0; i < expr.begin.expr_count; i++) {
                self.find_free_vars(expr.begin.exprs[i], bound_vars, free_vars, enclosing_scope);
            }

        case E_QUASIQUOTE:
            // Only look for free vars inside unquote nodes — the rest is quoted (literal symbols)
            self.find_free_vars_in_qq(expr.quasiquote.body, bound_vars, free_vars, enclosing_scope);

        case E_UNQUOTE:
            self.find_free_vars(expr.unquote.body, bound_vars, free_vars, enclosing_scope);

        case E_UNQUOTE_SPLICING:
            self.find_free_vars(expr.unquote_splicing.body, bound_vars, free_vars, enclosing_scope);

        case E_DEFMACRO:
            // Macro templates may reference free vars at expansion time
            // Conservative: treat as having no free vars (macros expand at compile time)
            return;

        case E_MODULE:
            for (usz i = 0; i < expr.module_expr.body_count; i++) {
                self.find_free_vars(expr.module_expr.body[i], bound_vars, free_vars, enclosing_scope);
            }

        case E_IMPORT:
        case E_EXPORT_FROM:
            // No free variables
            return;

        case E_SET:
            // The variable being set might be free
            SymbolId set_name = expr.set_expr.name;
            foreach (b : *bound_vars) {
                if ((uint)b == (uint)set_name) {
                    // Bound locally, just analyze the value
                    self.find_free_vars(expr.set_expr.value, bound_vars, free_vars, enclosing_scope);
                    return;
                }
            }
            // Check enclosing scope before is_primitive (enclosing var shadows global)
            if (enclosing_scope != null) {
                foreach (e : *enclosing_scope) {
                    if ((uint)e == (uint)set_name) {
                        bool set_enc_free = false;
                        foreach (f : *free_vars) {
                            if ((uint)f == (uint)set_name) { set_enc_free = true; break; }
                        }
                        if (!set_enc_free) {
                            free_vars.push(set_name);
                        }
                        self.find_free_vars(expr.set_expr.value, bound_vars, free_vars, enclosing_scope);
                        return;
                    }
                }
            }
            if (self.is_primitive(set_name)) {
                self.find_free_vars(expr.set_expr.value, bound_vars, free_vars, enclosing_scope);
                return;
            }
            // Check if already in free vars
            bool set_already_free = false;
            foreach (f : *free_vars) {
                if ((uint)f == (uint)set_name) { set_already_free = true; break; }
            }
            if (!set_already_free) {
                free_vars.push(set_name);
            }
            self.find_free_vars(expr.set_expr.value, bound_vars, free_vars, enclosing_scope);

        default:
            // Handle other expression types conservatively
            return;
    }
}

/**
 * Traverse quasiquote body, only descending into unquote/unquote-splicing for free vars.
 * In quasiquote, bare symbols are literals — only unquoted expressions reference variables.
 */
fn void Compiler.find_free_vars_in_qq(Compiler* self, Expr* expr, List{SymbolId}* bound_vars, List{SymbolId}* free_vars, List{SymbolId}* enclosing_scope) {
    if (expr == null) return;
    switch (expr.tag) {
        case E_UNQUOTE:
            // Unquoted expression — evaluate normally, so scan for free vars
            self.find_free_vars(expr.unquote.body, bound_vars, free_vars, enclosing_scope);
        case E_UNQUOTE_SPLICING:
            self.find_free_vars(expr.unquote_splicing.body, bound_vars, free_vars, enclosing_scope);
        case E_CALL:
            // Lists in qq template are parsed as E_CALL — traverse to find unquotes
            self.find_free_vars_in_qq(expr.call.func, bound_vars, free_vars, enclosing_scope);
            for (usz i = 0; i < expr.call.arg_count; i++) {
                self.find_free_vars_in_qq(expr.call.args[i], bound_vars, free_vars, enclosing_scope);
            }
        case E_APP:
            self.find_free_vars_in_qq(expr.app.func, bound_vars, free_vars, enclosing_scope);
            self.find_free_vars_in_qq(expr.app.arg, bound_vars, free_vars, enclosing_scope);
        case E_QUASIQUOTE:
            // Nested quasiquote — still only scan for unquotes
            self.find_free_vars_in_qq(expr.quasiquote.body, bound_vars, free_vars, enclosing_scope);
        default:
            // E_VAR, E_LIT, etc. in qq context are literal — no free vars
            return;
    }
}

fn void Compiler.collect_pattern_bindings(Compiler* self, Pattern* pat, List{SymbolId}* bindings) {
    if (pat == null) return;

    switch (pat.tag) {
        case PAT_VAR:
            bindings.push(pat.var_name);

        case PAT_CONS:
            self.collect_pattern_bindings(pat.car_pat, bindings);
            self.collect_pattern_bindings(pat.cdr_pat, bindings);

        case PAT_SEQ:
            for (usz i = 0; i < pat.elem_count; i++) {
                self.collect_pattern_bindings(pat.elements[i], bindings);
            }
            if (pat.rest_pos == REST_MIDDLE) {
                bindings.push(pat.rest_binding);
            }

        case PAT_GUARD:
            if (pat.guard_sub != null) {
                self.collect_pattern_bindings(pat.guard_sub, bindings);
            }

        default:
            return;
    }
}

fn bool Compiler.is_primitive(Compiler* self, SymbolId sym) {
    char[] name = self.interp.symbols.get_name(sym);

    // Check known primitives
    char[][*] primitives = {
        "+", "-", "*", "/", "%",
        "=", "<", ">", "<=", ">=",
        "cons", "car", "cdr", "null?", "pair?",
        "list", "length", "not",
        "print", "println", "newline", "display",
        "string-append", "string-join", "substring",
        "string-split", "string-length", "string->list",
        "list->string", "string-upcase", "string-downcase",
        "string-trim", "string?", "int?", "symbol?",
        "closure?", "continuation?", "double?", "list?",
        "boolean?", "number?",
        "string->number", "number->string",
        "gensym", "load", "apply", "equal?",
        "dict", "dict-set!", "dict?",
        "ref", "push!", "keys", "values", "has?", "remove!",
        "read-file", "write-file", "file-exists?", "read-lines",
        "type-of",
        "abs", "min", "max", "floor", "ceiling", "round",
        "truncate", "sqrt", "even?", "odd?", "zero?",
        "positive?", "negative?", "gcd",
        "bitwise-and", "bitwise-or", "bitwise-xor",
        "bitwise-not", "lshift", "rshift",
        "true", "false", "nil"
    };

    foreach (prim : primitives) {
        if (self.str_eq(name, prim)) return true;
    }

    // Check if it's a defined global
    foreach (g : self.defined_globals) {
        if ((uint)g == (uint)sym) return true;
    }

    return false;
}

fn bool Compiler.is_mutable_captured_var(Compiler* self, SymbolId sym) {
    foreach (mc : self.mutable_captures) {
        if ((uint)mc == (uint)sym) return true;
    }
    return false;
}

fn bool Compiler.str_eq(Compiler* self, char[] a, char[] b) {
    if (a.len != b.len) return false;
    for (usz i = 0; i < a.len; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}

// =============================================================================
// SECTION 4a1: MUTABLE CAPTURE PRESCAN
// =============================================================================

/**
 * Pre-scan the entire expression tree to identify all mutable-captured variables.
 * Must be called before scan_lambdas so that lambda scanning can exclude
 * mutable-captured variables from closure struct captures.
 */
fn void Compiler.prescan_mutable_captures(Compiler* self, Expr* expr) {
    if (expr == null) return;
    switch (expr.tag) {
        case E_LET:
            // Check if this let-bound variable is mutable-captured
            if (self.is_mutable_capture(expr.let_expr.name, expr.let_expr.body)) {
                // Add to mutable captures if not already present
                bool already = false;
                foreach (mc : self.mutable_captures) {
                    if ((uint)mc == (uint)expr.let_expr.name) { already = true; break; }
                }
                if (!already) {
                    self.mutable_captures.push(expr.let_expr.name);
                }
            }
            self.prescan_mutable_captures(expr.let_expr.init);
            self.prescan_mutable_captures(expr.let_expr.body);
        case E_LAMBDA:
            self.prescan_mutable_captures(expr.lambda.body);
        case E_APP:
            self.prescan_mutable_captures(expr.app.func);
            self.prescan_mutable_captures(expr.app.arg);
        case E_IF:
            self.prescan_mutable_captures(expr.if_expr.test);
            self.prescan_mutable_captures(expr.if_expr.then_branch);
            self.prescan_mutable_captures(expr.if_expr.else_branch);
        case E_DEFINE:
            self.prescan_mutable_captures(expr.define.value);
        case E_BEGIN:
            for (usz i = 0; i < expr.begin.expr_count; i++) {
                self.prescan_mutable_captures(expr.begin.exprs[i]);
            }
        case E_CALL:
            self.prescan_mutable_captures(expr.call.func);
            for (usz i = 0; i < expr.call.arg_count; i++) {
                self.prescan_mutable_captures(expr.call.args[i]);
            }
        case E_AND:
            self.prescan_mutable_captures(expr.and_expr.left);
            self.prescan_mutable_captures(expr.and_expr.right);
        case E_OR:
            self.prescan_mutable_captures(expr.or_expr.left);
            self.prescan_mutable_captures(expr.or_expr.right);
        case E_MATCH:
            self.prescan_mutable_captures(expr.match.scrutinee);
            for (usz i = 0; i < expr.match.clause_count; i++) {
                self.prescan_mutable_captures(expr.match.clauses[i].result);
            }
        case E_SET:
            self.prescan_mutable_captures(expr.set_expr.value);
        case E_RESET:
            self.prescan_mutable_captures(expr.reset.body);
        case E_SHIFT:
            self.prescan_mutable_captures(expr.shift.body);
        case E_HANDLE:
            self.prescan_mutable_captures(expr.handle.body);
            for (usz i = 0; i < expr.handle.clause_count; i++) {
                self.prescan_mutable_captures(expr.handle.clauses[i].handler_body);
            }
        case E_PERFORM:
            self.prescan_mutable_captures(expr.perform.arg);
        case E_RESOLVE:
            self.prescan_mutable_captures(expr.resolve.value);
        default:
            return;
    }
}

// =============================================================================
// SECTION 4a2: MUTABLE CAPTURE DETECTION
// =============================================================================

/**
 * Check if a variable is both captured by a nested lambda AND mutated with set!.
 * This is done by scanning the expression tree for (set! name ...) where name
 * is a free variable of some nested lambda.
 *
 * We detect: is `name` set! inside any sub-expression that also contains
 * a lambda that captures `name`?
 */
fn bool Compiler.is_mutable_capture(Compiler* self, SymbolId name, Expr* body) {
    // Check if name is set! anywhere AND captured by any nested lambda
    bool has_set = self.has_set_on(name, body);
    bool has_capture = self.is_captured_by_nested_lambda(name, body);
    return has_set && has_capture;
}

fn bool Compiler.has_set_on(Compiler* self, SymbolId name, Expr* expr) {
    if (expr == null) return false;
    switch (expr.tag) {
        case E_SET:
            if ((uint)expr.set_expr.name == (uint)name) return true;
            return self.has_set_on(name, expr.set_expr.value);
        case E_APP:
            return self.has_set_on(name, expr.app.func) || self.has_set_on(name, expr.app.arg);
        case E_IF:
            return self.has_set_on(name, expr.if_expr.test) ||
                   self.has_set_on(name, expr.if_expr.then_branch) ||
                   self.has_set_on(name, expr.if_expr.else_branch);
        case E_LET:
            return self.has_set_on(name, expr.let_expr.init) || self.has_set_on(name, expr.let_expr.body);
        case E_LAMBDA:
            return self.has_set_on(name, expr.lambda.body);
        case E_BEGIN:
            for (usz i = 0; i < expr.begin.expr_count; i++) {
                if (self.has_set_on(name, expr.begin.exprs[i])) return true;
            }
            return false;
        case E_CALL:
            if (self.has_set_on(name, expr.call.func)) return true;
            for (usz i = 0; i < expr.call.arg_count; i++) {
                if (self.has_set_on(name, expr.call.args[i])) return true;
            }
            return false;
        case E_AND:
            return self.has_set_on(name, expr.and_expr.left) || self.has_set_on(name, expr.and_expr.right);
        case E_OR:
            return self.has_set_on(name, expr.or_expr.left) || self.has_set_on(name, expr.or_expr.right);
        case E_MATCH:
            if (self.has_set_on(name, expr.match.scrutinee)) return true;
            for (usz i = 0; i < expr.match.clause_count; i++) {
                if (self.has_set_on(name, expr.match.clauses[i].result)) return true;
            }
            return false;
        default:
            return false;
    }
}

fn bool Compiler.is_captured_by_nested_lambda(Compiler* self, SymbolId name, Expr* expr) {
    if (expr == null) return false;
    switch (expr.tag) {
        case E_LAMBDA:
            // Check if this lambda captures name as a free variable
            List{SymbolId} bound;
            if ((uint)expr.lambda.param != 0xFFFFFFFF) {
                bound.push(expr.lambda.param);
            }
            List{SymbolId} free;
            self.find_free_vars(expr.lambda.body, &bound, &free, null);
            bool captured = false;
            foreach (f : free) {
                if ((uint)f == (uint)name) { captured = true; break; }
            }
            bound.free();
            free.free();
            if (captured) return true;
            // Also check deeper nesting
            return self.is_captured_by_nested_lambda(name, expr.lambda.body);
        case E_APP:
            return self.is_captured_by_nested_lambda(name, expr.app.func) ||
                   self.is_captured_by_nested_lambda(name, expr.app.arg);
        case E_IF:
            return self.is_captured_by_nested_lambda(name, expr.if_expr.test) ||
                   self.is_captured_by_nested_lambda(name, expr.if_expr.then_branch) ||
                   self.is_captured_by_nested_lambda(name, expr.if_expr.else_branch);
        case E_LET:
            return self.is_captured_by_nested_lambda(name, expr.let_expr.init) ||
                   self.is_captured_by_nested_lambda(name, expr.let_expr.body);
        case E_BEGIN:
            for (usz i = 0; i < expr.begin.expr_count; i++) {
                if (self.is_captured_by_nested_lambda(name, expr.begin.exprs[i])) return true;
            }
            return false;
        case E_CALL:
            if (self.is_captured_by_nested_lambda(name, expr.call.func)) return true;
            for (usz i = 0; i < expr.call.arg_count; i++) {
                if (self.is_captured_by_nested_lambda(name, expr.call.args[i])) return true;
            }
            return false;
        case E_AND:
            return self.is_captured_by_nested_lambda(name, expr.and_expr.left) ||
                   self.is_captured_by_nested_lambda(name, expr.and_expr.right);
        case E_OR:
            return self.is_captured_by_nested_lambda(name, expr.or_expr.left) ||
                   self.is_captured_by_nested_lambda(name, expr.or_expr.right);
        case E_MATCH:
            if (self.is_captured_by_nested_lambda(name, expr.match.scrutinee)) return true;
            for (usz i = 0; i < expr.match.clause_count; i++) {
                if (self.is_captured_by_nested_lambda(name, expr.match.clauses[i].result)) return true;
            }
            return false;
        case E_SET:
            return self.is_captured_by_nested_lambda(name, expr.set_expr.value);
        default:
            return false;
    }
}

// =============================================================================
// SECTION 4b: CLOSURE CREATION ANALYSIS
// =============================================================================

/**
 * Check if an expression body creates closures that might escape.
 * Used to determine if a lambda needs frame region management.
 *
 * A lambda "creates closures" if its body contains lambda expressions.
 * This is conservative but correct - if a lambda is created, it might escape.
 */
// =============================================================================
// SECTION 4c: EXPRESSION SERIALIZER
// =============================================================================

/**
 * Serialize an Expr* back to Pika source text.
 * This is used to convert continuation forms (reset/shift/handle/perform)
 * into source strings that can be evaluated by the interpreter at runtime.
 *
 * The serialized output is appended to the compiler's output buffer temporarily
 * via a separate List{char} buffer.
 */
fn void Compiler.serialize_expr_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    if (expr == null) {
        self.buf_append(buf, "nil");
        return;
    }

    switch (expr.tag) {
        case E_LIT:
            self.serialize_value_to_buf(expr.lit.value, buf);

        case E_VAR:
            char[] name = self.interp.symbols.get_name(expr.var_expr.name);
            self.buf_append(buf, name);

        case E_LAMBDA:
            self.buf_append(buf, "(lambda (");
            // Check for zero-arg lambda
            if ((uint)expr.lambda.param == 0xFFFFFFFF) {
                // zero-arg
            } else {
                char[] pname = self.interp.symbols.get_name(expr.lambda.param);
                self.buf_append(buf, pname);
            }
            self.buf_append(buf, ") ");
            self.serialize_expr_to_buf(expr.lambda.body, buf);
            buf.push(')');

        case E_APP:
            buf.push('(');
            self.serialize_expr_to_buf(expr.app.func, buf);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.app.arg, buf);
            buf.push(')');

        case E_IF:
            self.buf_append(buf, "(if ");
            self.serialize_expr_to_buf(expr.if_expr.test, buf);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.if_expr.then_branch, buf);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.if_expr.else_branch, buf);
            buf.push(')');

        case E_LET:
            if (expr.let_expr.is_recursive) {
                self.buf_append(buf, "(let ^rec (");
            } else {
                self.buf_append(buf, "(let (");
            }
            char[] let_name = self.interp.symbols.get_name(expr.let_expr.name);
            self.buf_append(buf, let_name);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.let_expr.init, buf);
            self.buf_append(buf, ") ");
            self.serialize_expr_to_buf(expr.let_expr.body, buf);
            buf.push(')');

        case E_DEFINE:
            self.buf_append(buf, "(define ");
            char[] def_name = self.interp.symbols.get_name(expr.define.name);
            self.buf_append(buf, def_name);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.define.value, buf);
            buf.push(')');

        case E_QUOTE:
            buf.push('\'');
            self.serialize_value_to_buf(expr.quote.datum, buf);

        case E_RESET:
            self.buf_append(buf, "(reset ");
            self.serialize_expr_to_buf(expr.reset.body, buf);
            buf.push(')');

        case E_SHIFT:
            self.buf_append(buf, "(shift ");
            char[] k_name = self.interp.symbols.get_name(expr.shift.k_name);
            self.buf_append(buf, k_name);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.shift.body, buf);
            buf.push(')');

        case E_PERFORM:
            self.buf_append(buf, "(perform ");
            char[] tag_name = self.interp.symbols.get_name(expr.perform.tag);
            self.buf_append(buf, tag_name);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.perform.arg, buf);
            buf.push(')');

        case E_RESOLVE:
            self.buf_append(buf, "(resolve ");
            self.serialize_expr_to_buf(expr.resolve.value, buf);
            buf.push(')');

        case E_HANDLE:
            self.buf_append(buf, "(handle ");
            self.serialize_expr_to_buf(expr.handle.body, buf);
            for (usz i = 0; i < expr.handle.clause_count; i++) {
                self.buf_append(buf, " ((");
                char[] etag = self.interp.symbols.get_name(expr.handle.clauses[i].effect_tag);
                self.buf_append(buf, etag);
                buf.push(' ');
                char[] ek = self.interp.symbols.get_name(expr.handle.clauses[i].k_name);
                self.buf_append(buf, ek);
                buf.push(' ');
                char[] ea = self.interp.symbols.get_name(expr.handle.clauses[i].arg_name);
                self.buf_append(buf, ea);
                self.buf_append(buf, ") ");
                self.serialize_expr_to_buf(expr.handle.clauses[i].handler_body, buf);
                buf.push(')');
            }
            buf.push(')');

        case E_AND:
            self.buf_append(buf, "(and ");
            self.serialize_expr_to_buf(expr.and_expr.left, buf);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.and_expr.right, buf);
            buf.push(')');

        case E_OR:
            self.buf_append(buf, "(or ");
            self.serialize_expr_to_buf(expr.or_expr.left, buf);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.or_expr.right, buf);
            buf.push(')');

        case E_BEGIN:
            self.buf_append(buf, "(begin");
            for (usz i = 0; i < expr.begin.expr_count; i++) {
                buf.push(' ');
                self.serialize_expr_to_buf(expr.begin.exprs[i], buf);
            }
            buf.push(')');

        case E_CALL:
            buf.push('(');
            self.serialize_expr_to_buf(expr.call.func, buf);
            for (usz i = 0; i < expr.call.arg_count; i++) {
                buf.push(' ');
                self.serialize_expr_to_buf(expr.call.args[i], buf);
            }
            buf.push(')');

        case E_SET:
            self.buf_append(buf, "(set! ");
            char[] set_name = self.interp.symbols.get_name(expr.set_expr.name);
            self.buf_append(buf, set_name);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.set_expr.value, buf);
            buf.push(')');

        case E_MATCH:
            self.buf_append(buf, "(match ");
            self.serialize_expr_to_buf(expr.match.scrutinee, buf);
            for (usz i = 0; i < expr.match.clause_count; i++) {
                buf.push(' ');
                self.serialize_pattern_to_buf(expr.match.clauses[i].pattern, buf);
                buf.push(' ');
                self.serialize_expr_to_buf(expr.match.clauses[i].result, buf);
            }
            buf.push(')');

        case E_INDEX:
            self.serialize_expr_to_buf(expr.index.collection, buf);
            self.buf_append(buf, ".[");
            self.serialize_expr_to_buf(expr.index.index, buf);
            buf.push(']');

        case E_PATH:
            for (usz i = 0; i < expr.path.segment_count; i++) {
                if (i > 0) buf.push('.');
                char[] seg = self.interp.symbols.get_name(expr.path.segments[i]);
                self.buf_append(buf, seg);
            }

        case E_QUASIQUOTE:
            buf.push('`');
            self.serialize_expr_to_buf(expr.quasiquote.body, buf);

        case E_UNQUOTE:
            buf.push(',');
            self.serialize_expr_to_buf(expr.unquote.body, buf);

        case E_UNQUOTE_SPLICING:
            self.buf_append(buf, ",@");
            self.serialize_expr_to_buf(expr.unquote_splicing.body, buf);

        case E_DEFMACRO:
            self.buf_append(buf, "(define [macro] ");
            char[] macro_name = self.interp.symbols.get_name(expr.define_macro.name);
            self.buf_append(buf, macro_name);
            for (usz i = 0; i < expr.define_macro.clause_count; i++) {
                self.buf_append(buf, " (");
                self.serialize_pattern_to_buf(expr.define_macro.clauses[i].pattern, buf);
                buf.push(' ');
                self.buf_append(buf, "(");
                self.serialize_value_to_buf(expr.define_macro.clauses[i].tmpl, buf);
                self.buf_append(buf, "))");
            }
            buf.push(')');

        case E_MODULE:
            self.buf_append(buf, "(module ");
            char[] mod_name = self.interp.symbols.get_name(expr.module_expr.name);
            self.buf_append(buf, mod_name);
            self.buf_append(buf, " (export");
            for (usz i = 0; i < expr.module_expr.export_count; i++) {
                buf.push(' ');
                char[] exp_name = self.interp.symbols.get_name(expr.module_expr.exports[i]);
                self.buf_append(buf, exp_name);
            }
            buf.push(')');
            for (usz i = 0; i < expr.module_expr.body_count; i++) {
                buf.push(' ');
                self.serialize_expr_to_buf(expr.module_expr.body[i], buf);
            }
            buf.push(')');

        case E_IMPORT:
            self.buf_append(buf, "(import ");
            char[] imp_name = self.interp.symbols.get_name(expr.import_expr.name);
            self.buf_append(buf, imp_name);
            if (expr.import_expr.import_all) {
                self.buf_append(buf, " :all");
            } else if (expr.import_expr.import_count > 0) {
                self.buf_append(buf, " (");
                for (usz i = 0; i < expr.import_expr.import_count; i++) {
                    if (i > 0) buf.push(' ');
                    char[] sel_name = self.interp.symbols.get_name(expr.import_expr.imports[i]);
                    self.buf_append(buf, sel_name);
                    if ((uint)expr.import_expr.aliases[i] != 0) {
                        self.buf_append(buf, " :as ");
                        char[] alias_name = self.interp.symbols.get_name(expr.import_expr.aliases[i]);
                        self.buf_append(buf, alias_name);
                    }
                }
                buf.push(')');
            }
            buf.push(')');

        case E_EXPORT_FROM:
            self.buf_append(buf, "(export-from ");
            char[] efmod = self.interp.symbols.get_name(expr.export_from.source_module);
            self.buf_append(buf, efmod);
            if (expr.export_from.all) {
                self.buf_append(buf, " :all)");
            } else {
                self.buf_append(buf, " (");
                for (usz i = 0; i < expr.export_from.name_count; i++) {
                    if (i > 0) buf.push(' ');
                    char[] ef_name = self.interp.symbols.get_name(expr.export_from.names[i]);
                    self.buf_append(buf, ef_name);
                }
                self.buf_append(buf, "))");
            }

        default:
            self.buf_append(buf, "nil");
    }
}

fn void Compiler.serialize_value_to_buf(Compiler* self, Value* v, List{char}* buf) {
    if (v == null || v.tag == NIL) {
        self.buf_append(buf, "nil");
        return;
    }

    switch (v.tag) {
        case INT:
            // Convert int to string
            long n = v.int_val;
            if (n < 0) {
                buf.push('-');
                n = -n;
            }
            if (n == 0) {
                buf.push('0');
            } else {
                char[32] digits;
                usz dpos = 0;
                while (n > 0 && dpos < 31) {
                    digits[dpos++] = (char)('0' + (n % 10));
                    n /= 10;
                }
                for (isz di = (isz)dpos - 1; di >= 0; di--) {
                    buf.push(digits[(usz)di]);
                }
            }

        case DOUBLE:
            char[64] dbuf;
            char[] dslice = io::bprintf(&dbuf, "%.15g", v.double_val)!!;
            self.buf_append(buf, dslice);

        case STRING:
            buf.push('"');
            for (usz i = 0; i < v.str_val.len; i++) {
                char c = v.str_val.chars[i];
                if (c == '"') {
                    buf.push('\\');
                    buf.push('"');
                } else if (c == '\\') {
                    buf.push('\\');
                    buf.push('\\');
                } else if (c == '\n') {
                    buf.push('\\');
                    buf.push('n');
                } else {
                    buf.push(c);
                }
            }
            buf.push('"');

        case SYMBOL:
            char[] sym_name = self.interp.symbols.get_name(v.sym_val);
            self.buf_append(buf, sym_name);

        case CONS:
            buf.push('(');
            self.serialize_value_to_buf(v.cons_val.car, buf);
            Value* rest = v.cons_val.cdr;
            while (rest != null && rest.tag == CONS) {
                buf.push(' ');
                self.serialize_value_to_buf(rest.cons_val.car, buf);
                rest = rest.cons_val.cdr;
            }
            if (rest != null && rest.tag != NIL) {
                self.buf_append(buf, " . ");
                self.serialize_value_to_buf(rest, buf);
            }
            buf.push(')');

        default:
            self.buf_append(buf, "nil");
    }
}

fn void Compiler.serialize_pattern_to_buf(Compiler* self, Pattern* pat, List{char}* buf) {
    if (pat == null) {
        buf.push('_');
        return;
    }

    switch (pat.tag) {
        case PAT_WILDCARD:
            buf.push('_');

        case PAT_VAR:
            char[] vname = self.interp.symbols.get_name(pat.var_name);
            self.buf_append(buf, vname);

        case PAT_LIT:
            self.serialize_value_to_buf(pat.lit_value, buf);

        case PAT_QUOTE:
            buf.push('\'');
            self.serialize_value_to_buf(pat.quote_datum, buf);

        case PAT_SEQ:
            buf.push('[');
            for (usz i = 0; i < pat.elem_count; i++) {
                if (i > 0) buf.push(' ');
                self.serialize_pattern_to_buf(pat.elements[i], buf);
            }
            if (pat.rest_pos == REST_MIDDLE) {
                self.buf_append(buf, " .. ");
                char[] rname = self.interp.symbols.get_name(pat.rest_binding);
                self.buf_append(buf, rname);
            }
            buf.push(']');

        case PAT_CONS:
            buf.push('(');
            self.serialize_pattern_to_buf(pat.car_pat, buf);
            self.buf_append(buf, " . ");
            self.serialize_pattern_to_buf(pat.cdr_pat, buf);
            buf.push(')');

        default:
            buf.push('_');
    }
}

fn void Compiler.buf_append(Compiler* self, List{char}* buf, char[] s) {
    foreach (c : s) {
        buf.push(c);
    }
}

/**
 * Emit the serialized form of an expression as a C3 string literal
 * directly into the compiler output. Escapes special characters.
 */
fn void Compiler.emit_serialized_expr(Compiler* self, Expr* expr) {
    List{char} buf;
    self.serialize_expr_to_buf(expr, &buf);

    // Emit the serialized source as a C3 string literal with escaping
    self.emit("\"");
    for (usz i = 0; i < buf.len(); i++) {
        char c = buf.entries[i];
        if (c == '"') {
            self.emit("\\\"");
        } else if (c == '\\') {
            self.emit("\\\\");
        } else if (c == '\n') {
            self.emit("\\n");
        } else if (c == '\t') {
            self.emit("\\t");
        } else {
            self.emit_char(c);
        }
    }
    self.emit("\"");

    buf.free();
}

/**
 * Collect free variables from a continuation expression (reset/shift/handle/perform).
 * These are variables that need to be injected into the interpreter before evaluation.
 * Returns the list of free variables (caller must free).
 */
fn List{SymbolId} Compiler.collect_cont_free_vars(Compiler* self, Expr* expr) {
    List{SymbolId} bound;
    List{SymbolId} free;
    self.for_delegation = true;
    self.find_free_vars(expr, &bound, &free, null);
    self.for_delegation = false;
    bound.free();
    return free;
}

// =============================================================================
// SECTION 4d: STDLIB PRELUDE
// =============================================================================

/**
 * Standard library definitions prepended to all compiled programs.
 * These are the same HOFs defined in register_stdlib() in eval.c3.
 */
const char[] STDLIB_PRELUDE =
`(define with-trampoline (lambda (thunk) (handle (thunk nil) (bounce next-thunk (resolve (with-trampoline next-thunk))))))
(define (reverse lst) (let loop (xs lst acc nil) (if (null? xs) acc (loop (cdr xs) (cons (car xs) acc)))))
(define (map f lst) (let loop (xs lst acc nil) (if (null? xs) (reverse acc) (loop (cdr xs) (cons (f (car xs)) acc)))))
(define (filter pred lst) (let loop (xs lst acc nil) (if (null? xs) (reverse acc) (if (pred (car xs)) (loop (cdr xs) (cons (car xs) acc)) (loop (cdr xs) acc)))))
(define (foldl f acc lst) (let loop (a acc xs lst) (if (null? xs) a (loop (f a (car xs)) (cdr xs)))))
(define (foldr f init lst) (foldl (lambda (acc x) (f x acc)) init (reverse lst)))
(define (append a b) (let loop (xs (reverse a) acc b) (if (null? xs) acc (loop (cdr xs) (cons (car xs) acc)))))
(define (compose f g) (lambda (x) (f (g x))))
(define (id x) x)
(define (nth n lst) (let loop (i n xs lst) (if (= i 0) (car xs) (loop (- i 1) (cdr xs)))))
(define (take n lst) (let loop (i n xs lst acc nil) (if (= i 0) (reverse acc) (if (null? xs) (reverse acc) (loop (- i 1) (cdr xs) (cons (car xs) acc))))))
(define (drop n lst) (let loop (i n xs lst) (if (= i 0) xs (if (null? xs) nil (loop (- i 1) (cdr xs))))))
(define (zip a b) (let loop (xs a ys b acc nil) (if (or (null? xs) (null? ys)) (reverse acc) (loop (cdr xs) (cdr ys) (cons (cons (car xs) (car ys)) acc)))))
(define (range n) (let loop (i (- n 1) acc nil) (if (< i 0) acc (loop (- i 1) (cons i acc)))))
(define (for-each f lst) (let loop (xs lst) (if (null? xs) nil (begin (f (car xs)) (loop (cdr xs))))))
(define (any? pred lst) (let loop (xs lst) (if (null? xs) nil (if (pred (car xs)) true (loop (cdr xs))))))
(define (every? pred lst) (let loop (xs lst) (if (null? xs) true (if (pred (car xs)) (loop (cdr xs)) nil))))
(define (try thunk handler) (handle (thunk nil) (raise msg (handler msg))))
(define (assert! condition msg) (if condition true (signal raise msg)))
(define (assoc key alist) (let loop (xs alist) (if (null? xs) nil (if (= (car (car xs)) key) (car xs) (loop (cdr xs))))))
(define (assoc-ref key alist) (let (pair (assoc key alist)) (if (null? pair) nil (cdr pair))))
(define when (lambda (test body) (if test body nil)))
(define unless (lambda (test body) (if test nil body)))`;

// =============================================================================
// SECTION 5: MAIN COMPILATION
// =============================================================================

/**
 * Compile a complete program to C3 source code.
 * Prepends stdlib definitions so HOFs are available.
 */
fn char[] Compiler.compile_program(Compiler* self, char[] source) {
    // Prepend stdlib prelude to user source using bulk copy
    usz total = STDLIB_PRELUDE.len + 1 + source.len;
    char* full_buf = (char*)mem::malloc(total);
    defer mem::free(full_buf);
    mem::copy(full_buf, STDLIB_PRELUDE.ptr, STDLIB_PRELUDE.len);
    full_buf[STDLIB_PRELUDE.len] = ' ';
    mem::copy(&full_buf[STDLIB_PRELUDE.len + 1], source.ptr, source.len);
    char[] full = full_buf[:total];

    // Parse the program
    Lexer lex;
    lex.init(full);
    Parser p;
    p.init(&lex, self.interp);

    List{Expr*} exprs;
    while (!lex.at_end() && !p.has_error) {
        Expr* e = p.parse_expr();
        if (e != null) {
            exprs.push(e);
        }
    }
    defer exprs.free();

    if (p.has_error) {
        io::printfn("Syntax Error at line %d, column %d: %s", (int)p.error_line, (int)p.error_col, (ZString)&p.error_msg[0]);
        return "";
    }

    // First pass: collect all top-level defines (including inside module bodies)
    foreach (expr : exprs) {
        if (expr.tag == E_DEFINE) {
            self.defined_globals.push(expr.define.name);
        } else if (expr.tag == E_MODULE) {
            // Module body defines become globals when compiled inline
            for (usz mi = 0; mi < expr.module_expr.body_count; mi++) {
                if (expr.module_expr.body[mi].tag == E_DEFINE) {
                    self.defined_globals.push(expr.module_expr.body[mi].define.name);
                }
            }
        }
    }

    // Prescan for mutable captures (before lambda scanning)
    foreach (expr : exprs) {
        self.prescan_mutable_captures(expr);
    }

    // Second pass: scan all lambdas to generate definitions
    foreach (expr : exprs) {
        self.scan_lambdas(expr);
    }

    // D2: Two-pass compilation — compile all code into a temp buffer first
    // so we discover all referenced prims before emitting global declarations.
    DString code_buf;
    code_buf.init(mem, 8192);
    DString orig_output = self.output;
    self.output = code_buf;

    // Emit lambda struct and function definitions (compiles lambda bodies → discovers prims)
    self.emit_lambda_definitions();

    // Mark where lambda defs end and main() begins
    usz lambda_defs_end = self.output.str_view().len;

    // Emit main function body
    self.emit("fn int main() {\n");
    self.indent++;
    self.emit_line("aot::aot_init();");
    self.emit_newline();

    // Initialize all global variables to nil
    if (self.defined_globals.len() > 0) {
        self.emit_line("// Initialize global variables to nil");
        foreach (name : self.defined_globals) {
            self.emit_indent();
            self.emit_symbol_name(name);
            self.emit(" = aot::make_nil();\n");
        }
        self.emit_newline();
    }

    // Mark position for prim init insertion
    usz prim_init_pos = self.output.str_view().len;

    self.emit_newline();

    // Clear declared vars for main scope
    self.declared_vars.free();

    // Compile each top-level expression
    isz last_non_define = -1;
    if (self.print_last) {
        for (usz i = 0; i < exprs.len(); i++) {
            if (exprs[i].tag != E_DEFINE) {
                last_non_define = (isz)i;
            }
        }
    }

    bool print_all_active = false;
    for (usz i = 0; i < exprs.len(); i++) {
        if (exprs[i].tag == E_DEFINE) {
            char[] dname = self.interp.symbols.get_name(exprs[i].define.name);
            if (self.str_eq(dname, "__e2e_start__")) {
                print_all_active = true;
                usz init_r = self.compile_to_temp(exprs[i].define.value);
                self.emit_indent();
                self.emit_symbol_name(exprs[i].define.name);
                self.emit(" = ");
                self.emit_temp_ref(init_r);
                self.emit(";\n");
                continue;
            }
        }

        bool should_print = (self.print_all && print_all_active)
            || (self.print_last && (isz)i == last_non_define);

        if (should_print) {
            usz r = self.compile_to_temp(exprs[i]);
            self.emit_indent();
            self.emit("aot::print_value(");
            self.emit_temp_ref(r);
            self.emit(");\n");
        } else if (exprs[i].tag == E_DEFINE) {
            usz init_r = self.compile_to_temp(exprs[i].define.value);
            self.emit_indent();
            self.emit_symbol_name(exprs[i].define.name);
            self.emit(" = ");
            self.emit_temp_ref(init_r);
            self.emit(";\n");
        } else {
            self.compile_to_temp(exprs[i]);
        }
    }

    self.emit_main_end();

    // Swap back to original output — now we know all referenced prims
    code_buf = self.output;
    self.output = orig_output;

    // Assemble final output: prelude + globals + compiled code (with prim init inserted)
    self.emit_prelude();
    self.emit_global_declarations();

    char[] code_str = code_buf.str_view();

    if (self.referenced_prims.len() > 0 && prim_init_pos <= code_str.len) {
        // Emit code before prim init point
        self.emit(code_str[:prim_init_pos]);
        // Emit prim init code
        self.emit("    // Initialize cached primitives\n");
        foreach (sym : self.referenced_prims) {
            self.emit("    ");
            self.emit_prim_global_name(sym);
            self.emit(" = ");
            char[] init_code = prim_hash_lookup(sym);
            self.emit(init_code);
            self.emit(";\n");
        }
        self.emit("\n");
        // Emit rest of code after init point
        if (prim_init_pos < code_str.len) {
            self.emit(code_str[prim_init_pos..]);
        }
    } else {
        self.emit(code_str);
    }

    code_buf.free();

    return self.get_output();
}

/**
 * Scan expression tree for lambdas and register them.
 */
fn void Compiler.scan_lambdas(Compiler* self, Expr* expr) {
    List{SymbolId} empty;
    self.scan_lambdas_with_scope(expr, &empty);
    empty.free();
}

// Returns true if any E_LAMBDA was found in the subtree (replaces body_creates_closure)
fn bool Compiler.scan_lambdas_with_scope(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    if (expr == null) return false;

    switch (expr.tag) {
        case E_LAMBDA:
            List{SymbolId} own_bound;
            // Add all params to bound set for free-var scanning
            if (expr.lambda.param_count > 1) {
                for (usz pi = 0; pi < expr.lambda.param_count; pi++) {
                    own_bound.push(expr.lambda.params[pi]);
                }
            } else if ((uint)expr.lambda.param != 0xFFFFFFFF) {
                own_bound.push(expr.lambda.param);
            }
            // For variadic lambdas, add all fixed params + rest param to bound set
            if (expr.lambda.has_rest) {
                for (usz pi = 0; pi < expr.lambda.param_count; pi++) {
                    own_bound.push(expr.lambda.params[pi]);
                }
                if ((uint)expr.lambda.rest_param != 0) {
                    own_bound.push(expr.lambda.rest_param);
                }
            }
            List{SymbolId} free;
            self.find_free_vars(expr.lambda.body, &own_bound, &free, enclosing_bound);

            LambdaDef def;
            def.id = self.lambda_counter++;
            def.param = expr.lambda.param;
            def.body = expr.lambda.body;
            def.capture_count = 0;
            def.capture_capacity = LAMBDA_CAPTURE_INITIAL;
            def.captures = (SymbolId*)mem::malloc(SymbolId.sizeof * def.capture_capacity);
            // Store variadic info
            def.has_rest = expr.lambda.has_rest;
            def.rest_param = expr.lambda.rest_param;
            def.param_count = expr.lambda.param_count;
            def.param_capacity = expr.lambda.param_count < LAMBDA_CAPTURE_INITIAL ? LAMBDA_CAPTURE_INITIAL : expr.lambda.param_count;
            def.params = (SymbolId*)mem::malloc(SymbolId.sizeof * def.param_capacity);
            for (usz pi = 0; pi < expr.lambda.param_count; pi++) {
                def.params[pi] = expr.lambda.params[pi];
            }

            foreach (f : free) {
                if (def.capture_count >= def.capture_capacity) {
                    usz new_cap = def.capture_capacity * 2;
                    SymbolId* new_caps = (SymbolId*)mem::malloc(SymbolId.sizeof * new_cap);
                    for (usz ci = 0; ci < def.capture_count; ci++) new_caps[ci] = def.captures[ci];
                    mem::free(def.captures);
                    def.captures = new_caps;
                    def.capture_capacity = new_cap;
                }
                def.captures[def.capture_count++] = f;
            }

            // Recursively scan body — the return value tells us if body has nested lambdas
            List{SymbolId} new_enclosing;
            foreach (b : *enclosing_bound) {
                new_enclosing.push(b);
            }
            // Add all params to enclosing scope for nested lambda free var analysis
            if (expr.lambda.param_count > 1) {
                for (usz pi = 0; pi < expr.lambda.param_count; pi++) {
                    new_enclosing.push(expr.lambda.params[pi]);
                }
            } else if ((uint)expr.lambda.param != 0xFFFFFFFF) {
                new_enclosing.push(expr.lambda.param);
            }
            if (expr.lambda.has_rest && (uint)expr.lambda.rest_param != 0) {
                new_enclosing.push(expr.lambda.rest_param);
            }
            bool body_has_lambda = self.scan_lambdas_with_scope(expr.lambda.body, &new_enclosing);

            // Set creates_closure from recursive scan (replaces body_creates_closure)
            def.creates_closure = body_has_lambda;
            if (def.id < 256) {
                self.lambda_creates_closure[def.id] = body_has_lambda;
            }

            self.lambda_defs.push(def);

            new_enclosing.free();
            own_bound.free();
            free.free();
            return true;  // This node IS a lambda

        case E_APP:
            bool app_f = self.scan_lambdas_with_scope(expr.app.func, enclosing_bound);
            return self.scan_lambdas_with_scope(expr.app.arg, enclosing_bound) || app_f;

        case E_IF:
            bool if_t = self.scan_lambdas_with_scope(expr.if_expr.test, enclosing_bound);
            bool if_th = self.scan_lambdas_with_scope(expr.if_expr.then_branch, enclosing_bound);
            return self.scan_lambdas_with_scope(expr.if_expr.else_branch, enclosing_bound) || if_t || if_th;

        case E_LET:
            List{SymbolId} let_scope;
            foreach (b : *enclosing_bound) {
                let_scope.push(b);
            }
            let_scope.push(expr.let_expr.name);
            bool let_found;
            if (expr.let_expr.is_recursive) {
                let_found = self.scan_lambdas_with_scope(expr.let_expr.init, &let_scope);
            } else {
                let_found = self.scan_lambdas_with_scope(expr.let_expr.init, enclosing_bound);
            }
            bool let_body = self.scan_lambdas_with_scope(expr.let_expr.body, &let_scope);
            let_scope.free();
            return let_found || let_body;

        case E_DEFINE:
            return self.scan_lambdas_with_scope(expr.define.value, enclosing_bound);

        case E_AND:
            bool and_l = self.scan_lambdas_with_scope(expr.and_expr.left, enclosing_bound);
            return self.scan_lambdas_with_scope(expr.and_expr.right, enclosing_bound) || and_l;

        case E_OR:
            bool or_l = self.scan_lambdas_with_scope(expr.or_expr.left, enclosing_bound);
            return self.scan_lambdas_with_scope(expr.or_expr.right, enclosing_bound) || or_l;

        case E_MATCH:
            bool m_found = self.scan_lambdas_with_scope(expr.match.scrutinee, enclosing_bound);
            for (usz i = 0; i < expr.match.clause_count; i++) {
                m_found |= self.scan_lambdas_with_scope(expr.match.clauses[i].result, enclosing_bound);
            }
            return m_found;

        case E_CALL:
            bool c_found = self.scan_lambdas_with_scope(expr.call.func, enclosing_bound);
            for (usz i = 0; i < expr.call.arg_count; i++) {
                c_found |= self.scan_lambdas_with_scope(expr.call.args[i], enclosing_bound);
            }
            return c_found;

        case E_INDEX:
            bool idx_c = self.scan_lambdas_with_scope(expr.index.collection, enclosing_bound);
            return self.scan_lambdas_with_scope(expr.index.index, enclosing_bound) || idx_c;

        case E_PATH:
            return false;

        case E_RESET: {
            // Wrap body as synthetic lambda: (lambda (_eff_dummy) body)
            SymbolId dummy_param = self.interp.symbols.intern("_eff_dummy");
            Expr* orig_body = expr.reset.body;
            Expr* wrapper = self.interp.alloc_expr();
            wrapper.tag = E_LAMBDA;
            wrapper.lambda = (ExprLambda*)mem::malloc(ExprLambda.sizeof);
            wrapper.loc_line = expr.loc_line;
            wrapper.loc_column = expr.loc_column;
            wrapper.lambda.param = dummy_param;
            wrapper.lambda.param_count = 1;
            wrapper.lambda.params = (SymbolId*)mem::malloc(SymbolId.sizeof * 1);
            wrapper.lambda.params[0] = dummy_param;
            wrapper.lambda.param_annotations = null;
            wrapper.lambda.has_rest = false;
            wrapper.lambda.rest_param = 0;
            wrapper.lambda.has_typed_params = false;
            wrapper.lambda.body = orig_body;
            expr.reset.body = wrapper;
            return self.scan_lambdas_with_scope(wrapper, enclosing_bound);
        }

        case E_SHIFT: {
            // Wrap body as synthetic lambda: (lambda (k_name) body)
            Expr* orig_body = expr.shift.body;
            Expr* wrapper = self.interp.alloc_expr();
            wrapper.tag = E_LAMBDA;
            wrapper.lambda = (ExprLambda*)mem::malloc(ExprLambda.sizeof);
            wrapper.loc_line = expr.loc_line;
            wrapper.loc_column = expr.loc_column;
            wrapper.lambda.param = expr.shift.k_name;
            wrapper.lambda.param_count = 1;
            wrapper.lambda.params = (SymbolId*)mem::malloc(SymbolId.sizeof * 1);
            wrapper.lambda.params[0] = expr.shift.k_name;
            wrapper.lambda.param_annotations = null;
            wrapper.lambda.has_rest = false;
            wrapper.lambda.rest_param = 0;
            wrapper.lambda.has_typed_params = false;
            wrapper.lambda.body = orig_body;
            expr.shift.body = wrapper;
            return self.scan_lambdas_with_scope(wrapper, enclosing_bound);
        }

        case E_PERFORM:
            return self.scan_lambdas_with_scope(expr.perform.arg, enclosing_bound);

        case E_RESOLVE:
            return self.scan_lambdas_with_scope(expr.resolve.value, enclosing_bound);

        case E_HANDLE: {
            // Wrap body as synthetic lambda
            SymbolId dummy_param = self.interp.symbols.intern("_eff_dummy");
            Expr* orig_body = expr.handle.body;
            Expr* body_wrapper = self.interp.alloc_expr();
            body_wrapper.tag = E_LAMBDA;
            body_wrapper.lambda = (ExprLambda*)mem::malloc(ExprLambda.sizeof);
            body_wrapper.loc_line = expr.loc_line;
            body_wrapper.loc_column = expr.loc_column;
            body_wrapper.lambda.param = dummy_param;
            body_wrapper.lambda.param_count = 1;
            body_wrapper.lambda.params = (SymbolId*)mem::malloc(SymbolId.sizeof * 1);
            body_wrapper.lambda.params[0] = dummy_param;
            body_wrapper.lambda.param_annotations = null;
            body_wrapper.lambda.has_rest = false;
            body_wrapper.lambda.rest_param = 0;
            body_wrapper.lambda.has_typed_params = false;
            body_wrapper.lambda.body = orig_body;
            expr.handle.body = body_wrapper;
            bool h_found = self.scan_lambdas_with_scope(body_wrapper, enclosing_bound);

            // Wrap each handler clause as synthetic lambda: (lambda (_pair) (let (k (car _pair)) (let (arg (cdr _pair)) body)))
            SymbolId pair_param = self.interp.symbols.intern("_eff_pair");
            SymbolId sym_car = self.interp.symbols.intern("car");
            SymbolId sym_cdr = self.interp.symbols.intern("cdr");
            for (usz i = 0; i < expr.handle.clause_count; i++) {
                Expr* orig_handler = expr.handle.clauses[i].handler_body;
                SymbolId k_name = expr.handle.clauses[i].k_name;
                SymbolId arg_name = expr.handle.clauses[i].arg_name;

                // Build (cdr _pair) for arg extraction
                Expr* pair_ref2 = self.interp.alloc_expr();
                pair_ref2.tag = E_VAR;
                pair_ref2.var_expr.name = pair_param;
                Expr* cdr_fn = self.interp.alloc_expr();
                cdr_fn.tag = E_VAR;
                cdr_fn.var_expr.name = sym_cdr;
                Expr* cdr_call = self.interp.alloc_expr();
                cdr_call.tag = E_APP;
                cdr_call.app.func = cdr_fn;
                cdr_call.app.arg = pair_ref2;

                // Build: (let (arg_name (cdr _pair)) orig_handler)
                Expr* let_arg = self.interp.alloc_expr();
                let_arg.tag = E_LET;
                let_arg.let_expr.name = arg_name;
                let_arg.let_expr.init = cdr_call;
                let_arg.let_expr.body = orig_handler;
                let_arg.let_expr.is_recursive = false;

                // Build (car _pair) for k extraction
                Expr* pair_ref1 = self.interp.alloc_expr();
                pair_ref1.tag = E_VAR;
                pair_ref1.var_expr.name = pair_param;
                Expr* car_fn = self.interp.alloc_expr();
                car_fn.tag = E_VAR;
                car_fn.var_expr.name = sym_car;
                Expr* car_call = self.interp.alloc_expr();
                car_call.tag = E_APP;
                car_call.app.func = car_fn;
                car_call.app.arg = pair_ref1;

                // Build: (let (k_name (car _pair)) (let (arg_name ...) ...))
                Expr* let_k = self.interp.alloc_expr();
                let_k.tag = E_LET;
                let_k.let_expr.name = k_name;
                let_k.let_expr.init = car_call;
                let_k.let_expr.body = let_arg;
                let_k.let_expr.is_recursive = false;

                // Lambda wrapper: (lambda (_pair) (let (k ...) (let (arg ...) body)))
                Expr* handler_wrapper = self.interp.alloc_expr();
                handler_wrapper.tag = E_LAMBDA;
                handler_wrapper.lambda = (ExprLambda*)mem::malloc(ExprLambda.sizeof);
                handler_wrapper.loc_line = expr.loc_line;
                handler_wrapper.loc_column = expr.loc_column;
                handler_wrapper.lambda.param = pair_param;
                handler_wrapper.lambda.param_count = 1;
                handler_wrapper.lambda.params = (SymbolId*)mem::malloc(SymbolId.sizeof * 1);
                handler_wrapper.lambda.params[0] = pair_param;
                handler_wrapper.lambda.param_annotations = null;
                handler_wrapper.lambda.has_rest = false;
                handler_wrapper.lambda.rest_param = 0;
                handler_wrapper.lambda.has_typed_params = false;
                handler_wrapper.lambda.body = let_k;
                expr.handle.clauses[i].handler_body = handler_wrapper;
                h_found |= self.scan_lambdas_with_scope(handler_wrapper, enclosing_bound);
            }
            return h_found;
        }

        case E_BEGIN:
            bool b_found = false;
            for (usz i = 0; i < expr.begin.expr_count; i++) {
                b_found |= self.scan_lambdas_with_scope(expr.begin.exprs[i], enclosing_bound);
            }
            return b_found;

        case E_SET:
            return self.scan_lambdas_with_scope(expr.set_expr.value, enclosing_bound);

        case E_QUASIQUOTE:
            return self.scan_lambdas_with_scope(expr.quasiquote.body, enclosing_bound);

        case E_UNQUOTE:
            return self.scan_lambdas_with_scope(expr.unquote.body, enclosing_bound);

        case E_UNQUOTE_SPLICING:
            return self.scan_lambdas_with_scope(expr.unquote_splicing.body, enclosing_bound);

        case E_DEFMACRO:
            return false;

        case E_MODULE:
            bool mod_found = false;
            for (usz i = 0; i < expr.module_expr.body_count; i++) {
                List{SymbolId} mod_empty;
                mod_found |= self.scan_lambdas_with_scope(expr.module_expr.body[i], &mod_empty);
                mod_empty.free();
            }
            return mod_found;

        case E_IMPORT:
        case E_EXPORT_FROM:
            return false;

        default:
            return false;
    }
}

// =============================================================================
// SECTION 6: CODE EMISSION
// =============================================================================

fn void Compiler.emit_prelude(Compiler* self) {
    self.emit("// Generated by Omni Lisp Compiler\n");
    self.emit("// Do not edit manually\n\n");
    self.emit("import std::io;\n");
    self.emit("import main;\n");
    self.emit("import lisp;\n");
    self.emit("import lisp::aot;\n\n");
}

fn void Compiler.emit_lambda_definitions(Compiler* self) {
    // Emit struct definitions for closures with captures
    foreach (def : self.lambda_defs) {
        if (def.capture_count > 0) {
            self.emit("struct Lambda_");
            self.emit_usz(def.id);
            self.emit(" {\n");

            for (usz i = 0; i < def.capture_count; i++) {
                self.emit("    lisp::Value* captured_");
                self.emit_symbol_name(def.captures[i]);
                self.emit(";\n");
            }

            self.emit("}\n\n");
        }
    }

    // Emit function definitions for lambdas
    foreach (def : self.lambda_defs) {
        // Check if this is a zero-arg lambda (sentinel param)
        bool is_zero_arg = (uint)def.param == 0xFFFFFFFF;

        self.emit("fn lisp::Value* invoke_lambda_");
        self.emit_usz(def.id);

        if (def.has_rest || (def.param_count > 1)) {
            // Variadic or multi-param lambda: receives all args as a cons list
            self.emit("(void* _self, lisp::Value* _arg_list) {\n");
        } else if (is_zero_arg) {
            self.emit("(void* _self, lisp::Value* _unused) {\n");
        } else {
            self.emit("(void* _self, lisp::Value* ");
            self.emit_symbol_name(def.param);
            self.emit(") {\n");
        }

        self.indent++;

        // Clear declared vars for this function scope
        self.declared_vars.free();

        if (def.has_rest || (def.param_count > 1 && !def.has_rest)) {
            // Variadic or multi-param: unpack params from arg list
            self.emit_indent();
            self.emit("lisp::Value* _curr = _arg_list;\n");
            for (usz pi = 0; pi < def.param_count; pi++) {
                self.mark_declared(def.params[pi]);
                self.emit_indent();
                self.emit("lisp::Value* ");
                self.emit_symbol_name(def.params[pi]);
                self.emit(" = aot::car(_curr);\n");
                self.emit_indent();
                self.emit("_curr = aot::cdr(_curr);\n");
            }
            if (def.has_rest) {
                // Rest param gets remaining list
                self.mark_declared(def.rest_param);
                self.emit_indent();
                self.emit("lisp::Value* ");
                self.emit_symbol_name(def.rest_param);
                self.emit(" = _curr;\n");
            }
        } else {
            // Mark parameter as declared
            if (!is_zero_arg) {
                self.mark_declared(def.param);
            }
        }

        // Frame region push removed: scope-region handles memory management

        // Extract captures if any
        if (def.capture_count > 0) {
            self.emit_indent();
            self.emit("Lambda_");
            self.emit_usz(def.id);
            self.emit("* self = (Lambda_");
            self.emit_usz(def.id);
            self.emit("*)_self;\n");

            for (usz i = 0; i < def.capture_count; i++) {
                self.mark_declared(def.captures[i]);
                self.emit_indent();
                self.emit("lisp::Value* ");
                self.emit_symbol_name(def.captures[i]);
                self.emit(" = self.captured_");
                self.emit_symbol_name(def.captures[i]);
                self.emit(";\n");
            }
        }

        // Compile body and return using statement-level compilation
        if (def.body.tag == E_LAMBDA) {
            self.emit_lambda_return_with_frame(def.body, def.creates_closure);
        } else {
            usz body_r = self.compile_to_temp_tail(def.body);
            // Frame region pop removed: scope-region handles memory management
            self.emit_indent();
            self.emit("return ");
            self.emit_temp_ref(body_r);
            self.emit(";\n");
        }

        self.indent--;
        self.emit("}\n\n");
    }
}

fn bool Compiler.is_variadic_lambda(Compiler* self, usz lambda_id) {
    foreach (def : self.lambda_defs) {
        if (def.id == lambda_id) return def.has_rest;
    }
    return false;
}

// Multi-param lambdas also receive args as a cons list (like variadic)
fn bool Compiler.needs_arg_list(Compiler* self, usz lambda_id) {
    foreach (def : self.lambda_defs) {
        if (def.id == lambda_id) return def.has_rest || def.param_count > 1;
    }
    return false;
}

fn void Compiler.emit_make_closure_call(Compiler* self, usz lambda_id) {
    if (self.needs_arg_list(lambda_id)) {
        self.emit("make_variadic_closure");
    } else {
        self.emit("make_closure");
    }
}

fn void Compiler.emit_lambda_return(Compiler* self, Expr* expr) {
    // Find the lambda definition for this lambda
    usz lambda_id = 0;
    LambdaDef* found_def = null;

    foreach (&def : self.lambda_defs) {
        if (def.body == expr.lambda.body && (uint)def.param == (uint)expr.lambda.param) {
            lambda_id = def.id;
            found_def = def;
            break;
        }
    }

    if (found_def != null && found_def.capture_count > 0) {
        // Allocate closure struct on heap
        self.emit_indent();
        self.emit("Lambda_");
        self.emit_usz(lambda_id);
        self.emit("* _closure_data = (Lambda_");
        self.emit_usz(lambda_id);
        self.emit("*)mem::malloc(Lambda_");
        self.emit_usz(lambda_id);
        self.emit(".sizeof);\n");

        // Set captured values
        for (usz i = 0; i < found_def.capture_count; i++) {
            self.emit_indent();
            self.emit("_closure_data.captured_");
            self.emit_symbol_name(found_def.captures[i]);
            self.emit(" = ");
            self.emit_symbol_name(found_def.captures[i]);
            self.emit(";\n");
        }

        self.emit_indent();
        self.emit("return aot::");
        self.emit_make_closure_call(lambda_id);
        self.emit("((void*)_closure_data, &invoke_lambda_");
        self.emit_usz(lambda_id);
        self.emit(");\n");
    } else {
        // Simple closure without captures
        self.emit_indent();
        self.emit("return aot::");
        self.emit_make_closure_call(lambda_id);
        self.emit("(null, &invoke_lambda_");
        self.emit_usz(lambda_id);
        self.emit(");\n");
    }
}

fn void Compiler.emit_lambda_return_with_frame(Compiler* self, Expr* expr, bool has_frame) {
    // Find the lambda definition for this lambda
    usz lambda_id = 0;
    LambdaDef* found_def = null;

    foreach (&def : self.lambda_defs) {
        if (def.body == expr.lambda.body && (uint)def.param == (uint)expr.lambda.param) {
            lambda_id = def.id;
            found_def = def;
            break;
        }
    }

    if (found_def != null && found_def.capture_count > 0) {
        // Allocate closure struct on heap
        self.emit_indent();
        self.emit("Lambda_");
        self.emit_usz(lambda_id);
        self.emit("* _closure_data = (Lambda_");
        self.emit_usz(lambda_id);
        self.emit("*)mem::malloc(Lambda_");
        self.emit_usz(lambda_id);
        self.emit(".sizeof);\n");

        // Set captured values
        for (usz i = 0; i < found_def.capture_count; i++) {
            self.emit_indent();
            self.emit("_closure_data.captured_");
            self.emit_symbol_name(found_def.captures[i]);
            self.emit(" = ");
            self.emit_symbol_name(found_def.captures[i]);
            self.emit(";\n");
        }

        // Make closure value
        self.emit_indent();
        self.emit("lisp::Value* _result = aot::");
        self.emit_make_closure_call(lambda_id);
        self.emit("((void*)_closure_data, &invoke_lambda_");
        self.emit_usz(lambda_id);
        self.emit(");\n");

        // Frame pop removed: scope-region handles memory management
        // (has_frame parameter retained for API compatibility but unused)

        self.emit_indent();
        self.emit("return _result;\n");
    } else {
        // Simple closure without captures
        self.emit_indent();
        self.emit("lisp::Value* _result = aot::");
        self.emit_make_closure_call(lambda_id);
        self.emit("(null, &invoke_lambda_");
        self.emit_usz(lambda_id);
        self.emit(");\n");

        // Frame pop removed: scope-region handles memory management

        self.emit_indent();
        self.emit("return _result;\n");
    }
}

fn void Compiler.emit_global_declarations(Compiler* self) {
    if (self.defined_globals.len() == 0 && self.referenced_prims.len() == 0) return;

    if (self.defined_globals.len() > 0) {
        self.emit("// Global variables\n");
        foreach (name : self.defined_globals) {
            self.emit("lisp::Value* ");
            self.emit_symbol_name(name);
            self.emit(";\n");
        }
        self.emit_newline();
    }

    // D2: Cached primitive globals (initialized once in main after aot_init)
    if (self.referenced_prims.len() > 0) {
        self.emit("// Cached primitives\n");
        foreach (sym : self.referenced_prims) {
            self.emit("lisp::Value* ");
            self.emit_prim_global_name(sym);
            self.emit(";\n");
        }
        self.emit_newline();
    }
}

fn void Compiler.emit_main_start(Compiler* self) {
    self.emit("fn int main() {\n");
    self.indent++;

    // Initialize AOT interpreter
    self.emit_line("aot::aot_init();");
    self.emit_newline();

    // Initialize all global variables to nil
    if (self.defined_globals.len() > 0) {
        self.emit_line("// Initialize global variables to nil");
        foreach (name : self.defined_globals) {
            self.emit_indent();
            self.emit_symbol_name(name);
            self.emit(" = aot::make_nil();\n");
        }
        self.emit_newline();
    }

    // Macros (when, unless, cond) are expanded at compile time — no runtime registration needed
    self.emit_newline();
}

fn void Compiler.emit_main_end(Compiler* self) {
    self.emit_newline();
    // Shutdown AOT interpreter
    self.emit_line("aot::aot_shutdown();");
    self.emit_line("return 0;");
    self.indent--;
    self.emit("}\n");
}

// =============================================================================
// SECTION 7: STATEMENT COMPILATION
// =============================================================================

fn void Compiler.compile_statement(Compiler* self, Expr* expr) {
    if (expr == null) return;

    switch (expr.tag) {
        case E_DEFINE:
            // (define name value) => name = <compile value>;
            usz init_r = self.compile_to_temp(expr.define.value);
            self.emit_indent();
            self.emit_symbol_name(expr.define.name);
            self.emit(" = ");
            self.emit_temp_ref(init_r);
            self.emit(";\n");

        default:
            // Other expressions: evaluate and discard result
            self.compile_to_temp(expr);
    }
}

// =============================================================================
// SECTION 8: EXPRESSION COMPILATION
// =============================================================================

fn void Compiler.compile_expr(Compiler* self, Expr* expr) {
    if (expr == null) {
        self.emit("aot::make_nil()");
        return;
    }

    switch (expr.tag) {
        case E_LIT:
            self.compile_literal(expr.lit.value);

        case E_VAR:
            self.compile_var(expr);

        case E_DEFINE:
            // Define in expression context
            self.emit("(");
            self.emit_symbol_name(expr.define.name);
            self.emit(" = ");
            self.compile_expr(expr.define.value);
            self.emit(")");

        case E_QUOTE:
            self.compile_quote(expr.quote.datum);

        case E_PATH:
            self.compile_path(expr);

        default:
            io::printfn("WARNING: compiler: unsupported expr tag %d", expr.tag.ordinal);
            self.emit("aot::make_nil() /* WARNING: unsupported expr type */");
    }
}

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (compile_to_temp)
//
// compile_to_temp emits statements that compute an expression's value
// and store it in a temp variable _rN. Returns N.
// This avoids GCC-style statement expressions ({ ... }) which C3 doesn't support.
// =============================================================================

fn usz Compiler.next_result(Compiler* self) {
    usz id = self.temp_counter;
    self.temp_counter++;
    return id;
}

fn void Compiler.emit_temp_decl(Compiler* self, usz id) {
    self.emit_indent();
    self.emit("lisp::Value* _r");
    self.emit_usz(id);
}

fn void Compiler.emit_temp_ref(Compiler* self, usz id) {
    self.emit("_r");
    self.emit_usz(id);
}

/**
 * Compile an expression as statements, storing result in _rN.
 * Returns N. All temp declarations are emitted as proper C3 statements.
 */
fn usz Compiler.compile_to_temp(Compiler* self, Expr* expr) {
    if (expr == null) {
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::make_nil();\n");
        return id;
    }

    switch (expr.tag) {
        case E_CALL:
            return self.compile_call_flat(expr);

        case E_LET:
            return self.compile_let_flat(expr);

        case E_AND:
            return self.compile_and_flat(expr);

        case E_OR:
            return self.compile_or_flat(expr);

        case E_BEGIN:
            return self.compile_begin_flat(expr);

        case E_SET:
            return self.compile_set_flat(expr);

        case E_RESET:
            return self.compile_reset_flat(expr);

        case E_SHIFT:
            return self.compile_shift_flat(expr);

        case E_PERFORM:
            return self.compile_perform_flat(expr);

        case E_RESOLVE: {
            // Compile value, then call aot::compiled_resolve(__k, val)
            usz val_r = self.compile_to_temp(expr.resolve.value);
            // Emit __k resolution (since it is a free var, we can emit a var read)
            // Wait! The symbol is literally __k.
            // I'll emit it as a C3 identifier `__k` or as captured.
            // But how do I emit a variable read without an AST node?
            // Actually, I can just create a dummy E_VAR node!
            Expr dummy_k;
            dummy_k.tag = E_VAR;
            dummy_k.var_expr.name = self.interp.sym__k;
            usz k_r = self.compile_to_temp(&dummy_k);

            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = aot::compiled_resolve(");
            self.emit_temp_ref(k_r);
            self.emit(", ");
            self.emit_temp_ref(val_r);
            self.emit(");\n");
            return id;
        }

        case E_HANDLE:
            return self.compile_handle_flat(expr);

        case E_QUASIQUOTE:
            return self.compile_qq_flat(expr.quasiquote.body, 0);

        case E_DEFMACRO:
            // Macros expanded at compile time — no-op at runtime
            usz defmacro_id = self.next_result();
            self.emit_temp_decl(defmacro_id);
            self.emit(" = aot::make_nil(); // macro: expanded at compile time\n");
            return defmacro_id;

        case E_MODULE:
            return self.compile_module_flat(expr);

        case E_IMPORT:
            return self.compile_import_flat(expr);

        case E_EXPORT_FROM:
            return self.compile_export_from_flat(expr);

        case E_IF:
            return self.compile_if_flat(expr);

        case E_APP:
            return self.compile_app_flat(expr);

        case E_LAMBDA:
            return self.compile_lambda_flat(expr);

        case E_MATCH:
            return self.compile_match_flat(expr);

        case E_INDEX:
            usz idx_coll_r = self.compile_to_temp(expr.index.collection);
            usz idx_idx_r = self.compile_to_temp(expr.index.index);
            usz idx_id = self.next_result();
            self.emit_temp_decl(idx_id);
            self.emit(" = aot::index(");
            self.emit_temp_ref(idx_coll_r);
            self.emit(", ");
            self.emit_temp_ref(idx_idx_r);
            self.emit(");\n");
            return idx_id;

        case E_DEFINE:
            usz def_val_r = self.compile_to_temp(expr.define.value);
            self.emit_indent();
            self.emit_symbol_name(expr.define.name);
            self.emit(" = ");
            self.emit_temp_ref(def_val_r);
            self.emit(";\n");
            return def_val_r;

        default:
            // Leaf expressions (E_LIT, E_VAR, E_QUOTE, E_PATH):
            // compile_expr produces inline code without ({ }) or recursion
            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = ");
            self.compile_expr(expr);
            self.emit(";\n");
            return id;
    }
}

/**
 * compile_to_temp for tail position. For calls, emits make_thunk instead of rt_invoke.
 */
fn usz Compiler.compile_to_temp_tail(Compiler* self, Expr* expr) {
    if (expr == null) {
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::make_nil();\n");
        return id;
    }

    switch (expr.tag) {
        case E_APP:
            return self.compile_app_tail_flat(expr);

        case E_CALL:
            return self.compile_call_tail_flat(expr);

        case E_IF:
            // Propagate tail to branches
            usz cond = self.compile_to_temp(expr.if_expr.test);
            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(";\n");
            self.emit_indent();
            self.emit("if (aot::is_truthy(");
            self.emit_temp_ref(cond);
            self.emit(")) {\n");
            self.indent++;
            usz then_r = self.compile_to_temp_tail(expr.if_expr.then_branch);
            self.emit_indent();
            self.emit_temp_ref(id);
            self.emit(" = ");
            self.emit_temp_ref(then_r);
            self.emit(";\n");
            self.indent--;
            self.emit_indent();
            self.emit("} else {\n");
            self.indent++;
            usz else_r = self.compile_to_temp_tail(expr.if_expr.else_branch);
            self.emit_indent();
            self.emit_temp_ref(id);
            self.emit(" = ");
            self.emit_temp_ref(else_r);
            self.emit(";\n");
            self.indent--;
            self.emit_indent();
            self.emit("}\n");
            return id;

        case E_BEGIN:
            usz bcount = expr.begin.expr_count;
            if (bcount == 0) {
                usz bid = self.next_result();
                self.emit_temp_decl(bid);
                self.emit(" = aot::make_nil();\n");
                return bid;
            }
            for (usz i = 0; i < bcount - 1; i++) {
                self.compile_to_temp(expr.begin.exprs[i]);
            }
            return self.compile_to_temp_tail(expr.begin.exprs[bcount - 1]);

        case E_LET:
            return self.compile_let_flat_tail(expr);

        case E_AND:
            usz and_left = self.compile_to_temp(expr.and_expr.left);
            usz and_id = self.next_result();
            self.emit_temp_decl(and_id);
            self.emit(";\n");
            self.emit_indent();
            self.emit("if (aot::is_truthy(");
            self.emit_temp_ref(and_left);
            self.emit(")) {\n");
            self.indent++;
            usz and_right = self.compile_to_temp_tail(expr.and_expr.right);
            self.emit_indent();
            self.emit_temp_ref(and_id);
            self.emit(" = ");
            self.emit_temp_ref(and_right);
            self.emit(";\n");
            self.indent--;
            self.emit_indent();
            self.emit("} else {\n");
            self.indent++;
            self.emit_indent();
            self.emit_temp_ref(and_id);
            self.emit(" = ");
            self.emit_temp_ref(and_left);
            self.emit(";\n");
            self.indent--;
            self.emit_indent();
            self.emit("}\n");
            return and_id;

        case E_OR:
            usz or_left = self.compile_to_temp(expr.or_expr.left);
            usz or_id = self.next_result();
            self.emit_temp_decl(or_id);
            self.emit(";\n");
            self.emit_indent();
            self.emit("if (aot::is_truthy(");
            self.emit_temp_ref(or_left);
            self.emit(")) {\n");
            self.indent++;
            self.emit_indent();
            self.emit_temp_ref(or_id);
            self.emit(" = ");
            self.emit_temp_ref(or_left);
            self.emit(";\n");
            self.indent--;
            self.emit_indent();
            self.emit("} else {\n");
            self.indent++;
            usz or_right = self.compile_to_temp_tail(expr.or_expr.right);
            self.emit_indent();
            self.emit_temp_ref(or_id);
            self.emit(" = ");
            self.emit_temp_ref(or_right);
            self.emit(";\n");
            self.indent--;
            self.emit_indent();
            self.emit("}\n");
            return or_id;

        default:
            // Non-tail forms: just use compile_to_temp
            return self.compile_to_temp(expr);
    }
}

// --- Flat (statement-level) compilation of individual forms ---

fn usz Compiler.compile_call_flat(Compiler* self, Expr* expr) {
    usz arg_count = expr.call.arg_count;

    if (expr.call.func.tag == E_VAR) {
        char[] fname = self.interp.symbols.get_name(expr.call.func.var_expr.name);
        if (self.str_eq(fname, "dict")) {
            if (arg_count == 0) {
                usz id = self.next_result();
                self.emit_temp_decl(id);
                self.emit(" = aot::dict_from_args(aot::make_nil());\n");
                return id;
            }
            usz[32] arg_temps;
            for (usz i = 0; i < arg_count && i < 32; i++) {
                arg_temps[i] = self.compile_to_temp(expr.call.args[i]);
            }
            usz args_list = self.next_result();
            self.emit_temp_decl(args_list);
            self.emit(" = aot::make_nil();\n");
            for (isz i = (isz)arg_count - 1; i >= 0; i--) {
                self.emit_indent();
                self.emit_temp_ref(args_list);
                self.emit(" = aot::cons(");
                self.emit_temp_ref(arg_temps[(usz)i]);
                self.emit(", ");
                self.emit_temp_ref(args_list);
                self.emit(");\n");
            }
            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = aot::dict_from_args(");
            self.emit_temp_ref(args_list);
            self.emit(");\n");
            return id;
        }
        if (self.str_eq(fname, "list")) {
            if (arg_count == 0) {
                usz id = self.next_result();
                self.emit_temp_decl(id);
                self.emit(" = aot::make_nil();\n");
                return id;
            }
            usz[32] arg_temps;
            for (usz i = 0; i < arg_count && i < 32; i++) {
                arg_temps[i] = self.compile_to_temp(expr.call.args[i]);
            }
            usz result_r = self.next_result();
            self.emit_temp_decl(result_r);
            self.emit(" = aot::make_nil();\n");
            for (isz i = (isz)arg_count - 1; i >= 0; i--) {
                self.emit_indent();
                self.emit_temp_ref(result_r);
                self.emit(" = aot::cons(");
                self.emit_temp_ref(arg_temps[(usz)i]);
                self.emit(", ");
                self.emit_temp_ref(result_r);
                self.emit(");\n");
            }
            return result_r;
        }
    }

    if (arg_count == 0) {
        usz func_r = self.compile_to_temp(expr.call.func);
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::invoke(");
        self.emit_temp_ref(func_r);
        self.emit(", aot::make_nil());\n");
        return id;
    }

    usz[32] arg_temps;
    for (usz i = 0; i < arg_count && i < 32; i++) {
        arg_temps[i] = self.compile_to_temp(expr.call.args[i]);
    }

    usz func_r = self.compile_to_temp(expr.call.func);

    usz args_list = self.next_result();
    self.emit_temp_decl(args_list);
    self.emit(" = aot::make_nil();\n");
    for (isz i = (isz)arg_count - 1; i >= 0; i--) {
        self.emit_indent();
        self.emit_temp_ref(args_list);
        self.emit(" = aot::cons(");
        self.emit_temp_ref(arg_temps[(usz)i]);
        self.emit(", ");
        self.emit_temp_ref(args_list);
        self.emit(");\n");
    }

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::apply_multi(");
    self.emit_temp_ref(func_r);
    self.emit(", ");
    self.emit_temp_ref(args_list);
    self.emit(", ");
    self.emit_usz(arg_count);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_call_tail_flat(Compiler* self, Expr* expr) {
    usz arg_count = expr.call.arg_count;

    // Special cases: list/dict build data structures, delegate to non-tail version
    if (expr.call.func.tag == E_VAR) {
        char[] fname = self.interp.symbols.get_name(expr.call.func.var_expr.name);
        if (self.str_eq(fname, "list") || self.str_eq(fname, "dict")) {
            return self.compile_call_flat(expr);
        }
    }

    if (arg_count == 0) {
        usz func_r = self.compile_to_temp(expr.call.func);
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::invoke_tail(");
        self.emit_temp_ref(func_r);
        self.emit(", aot::make_nil());\n");
        return id;
    }

    // Compile each arg to a temp
    usz[32] arg_temps;
    for (usz i = 0; i < arg_count && i < 32; i++) {
        arg_temps[i] = self.compile_to_temp(expr.call.args[i]);
    }

    // Compile func
    usz func_r = self.compile_to_temp(expr.call.func);

    // Build cons list of args
    usz args_list = self.next_result();
    self.emit_temp_decl(args_list);
    self.emit(" = aot::make_nil();\n");
    for (isz i = (isz)arg_count - 1; i >= 0; i--) {
        self.emit_indent();
        self.emit_temp_ref(args_list);
        self.emit(" = aot::cons(");
        self.emit_temp_ref(arg_temps[(usz)i]);
        self.emit(", ");
        self.emit_temp_ref(args_list);
        self.emit(");\n");
    }

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::apply_multi_tail(");
    self.emit_temp_ref(func_r);
    self.emit(", ");
    self.emit_temp_ref(args_list);
    self.emit(", ");
    self.emit_usz(arg_count);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_app_flat(Compiler* self, Expr* expr) {
    usz func_r = self.compile_to_temp(expr.app.func);
    usz arg_r = self.compile_to_temp(expr.app.arg);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::invoke(");
    self.emit_temp_ref(func_r);
    self.emit(", ");
    self.emit_temp_ref(arg_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_app_tail_flat(Compiler* self, Expr* expr) {
    usz func_r = self.compile_to_temp(expr.app.func);
    usz arg_r = self.compile_to_temp(expr.app.arg);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::invoke_tail(");
    self.emit_temp_ref(func_r);
    self.emit(", ");
    self.emit_temp_ref(arg_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_if_flat(Compiler* self, Expr* expr) {
    usz cond = self.compile_to_temp(expr.if_expr.test);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(";\n");
    self.emit_indent();
    self.emit("if (aot::is_truthy(");
    self.emit_temp_ref(cond);
    self.emit(")) {\n");
    self.indent++;
    usz then_r = self.compile_to_temp(expr.if_expr.then_branch);
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = ");
    self.emit_temp_ref(then_r);
    self.emit(";\n");
    self.indent--;
    self.emit_indent();
    self.emit("} else {\n");
    self.indent++;
    usz else_r = self.compile_to_temp(expr.if_expr.else_branch);
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = ");
    self.emit_temp_ref(else_r);
    self.emit(";\n");
    self.indent--;
    self.emit_indent();
    self.emit("}\n");
    return id;
}

fn usz Compiler.compile_let_flat(Compiler* self, Expr* expr) {
    bool is_mc = self.is_mutable_captured_var(expr.let_expr.name);

    if (is_mc) {
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_indent();
        self.emit("aot::define_var(\"");
        char[] mc_name = self.interp.symbols.get_name(expr.let_expr.name);
        self.emit_escaped(mc_name);
        self.emit("\", ");
        self.emit_temp_ref(init_r);
        self.emit(");\n");
        return self.compile_to_temp(expr.let_expr.body);
    }

    bool already = self.is_declared(expr.let_expr.name);

    if (expr.let_expr.is_recursive) {
        if (!already) {
            self.emit_indent();
            self.emit("lisp::Value* ");
            self.emit_symbol_name(expr.let_expr.name);
            self.emit(";\n");
            self.mark_declared(expr.let_expr.name);
        }
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_indent();
        self.emit_symbol_name(expr.let_expr.name);
        self.emit(" = ");
        self.emit_temp_ref(init_r);
        self.emit(";\n");

        Expr* rec_init = expr.let_expr.init;
        if (rec_init.tag == E_LAMBDA) {
            foreach (&def : self.lambda_defs) {
                if (def.body == rec_init.lambda.body && (uint)def.param == (uint)rec_init.lambda.param) {
                    for (usz ci = 0; ci < def.capture_count; ci++) {
                        if ((uint)def.captures[ci] == (uint)expr.let_expr.name) {
                            self.emit_indent();
                            self.emit("lisp::aot::AotClosureData* _acd_");
                            self.emit_usz(def.id);
                            self.emit(" = (lisp::aot::AotClosureData*)");
                            self.emit_symbol_name(expr.let_expr.name);
                            self.emit(".prim_val.user_data;\n");
                            self.emit_indent();
                            self.emit("Lambda_");
                            self.emit_usz(def.id);
                            self.emit("* _sp_");
                            self.emit_usz(def.id);
                            self.emit(" = (Lambda_");
                            self.emit_usz(def.id);
                            self.emit("*)_acd_");
                            self.emit_usz(def.id);
                            self.emit(".data;\n");
                            self.emit_indent();
                            self.emit("_sp_");
                            self.emit_usz(def.id);
                            self.emit(".captured_");
                            self.emit_symbol_name(expr.let_expr.name);
                            self.emit(" = ");
                            self.emit_symbol_name(expr.let_expr.name);
                            self.emit(";\n");
                            break;
                        }
                    }
                    break;
                }
            }
        }
    } else {
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_indent();
        if (!already) {
            self.emit("lisp::Value* ");
            self.mark_declared(expr.let_expr.name);
        }
        self.emit_symbol_name(expr.let_expr.name);
        self.emit(" = ");
        self.emit_temp_ref(init_r);
        self.emit(";\n");
    }

    return self.compile_to_temp(expr.let_expr.body);
}
fn usz Compiler.compile_let_flat_tail(Compiler* self, Expr* expr) {
    bool is_mc = self.is_mutable_captured_var(expr.let_expr.name);

    if (is_mc) {
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_indent();
        self.emit("aot::define_var(\"");
        char[] mc_name = self.interp.symbols.get_name(expr.let_expr.name);
        self.emit_escaped(mc_name);
        self.emit("\", ");
        self.emit_temp_ref(init_r);
        self.emit(");\n");
        return self.compile_to_temp_tail(expr.let_expr.body);
    }

    bool already = self.is_declared(expr.let_expr.name);

    if (expr.let_expr.is_recursive) {
        if (!already) {
            self.emit_indent();
            self.emit("lisp::Value* ");
            self.emit_symbol_name(expr.let_expr.name);
            self.emit(";\n");
            self.mark_declared(expr.let_expr.name);
        }
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_indent();
        self.emit_symbol_name(expr.let_expr.name);
        self.emit(" = ");
        self.emit_temp_ref(init_r);
        self.emit(";\n");

        // Patch self-reference for recursive closure
        Expr* rec_init = expr.let_expr.init;
        if (rec_init.tag == E_LAMBDA) {
            foreach (&def : self.lambda_defs) {
                if (def.body == rec_init.lambda.body && (uint)def.param == (uint)rec_init.lambda.param) {
                    for (usz ci = 0; ci < def.capture_count; ci++) {
                        if ((uint)def.captures[ci] == (uint)expr.let_expr.name) {
                            // Patch: AOT closures are wrapped as Primitives
                            // Access via prim_val.user_data -> AotClosureData -> data
                            self.emit_indent();
                            self.emit("lisp::aot::AotClosureData* _acd_");
                            self.emit_usz(def.id);
                            self.emit(" = (lisp::aot::AotClosureData*)");
                            self.emit_symbol_name(expr.let_expr.name);
                            self.emit(".prim_val.user_data;\n");
                            self.emit_indent();
                            self.emit("Lambda_");
                            self.emit_usz(def.id);
                            self.emit("* _sp_");
                            self.emit_usz(def.id);
                            self.emit(" = (Lambda_");
                            self.emit_usz(def.id);
                            self.emit("*)_acd_");
                            self.emit_usz(def.id);
                            self.emit(".data;\n");
                            self.emit_indent();
                            self.emit("_sp_");
                            self.emit_usz(def.id);
                            self.emit(".captured_");
                            self.emit_symbol_name(expr.let_expr.name);
                            self.emit(" = ");
                            self.emit_symbol_name(expr.let_expr.name);
                            self.emit(";\n");
                            break;
                        }
                    }
                    break;
                }
            }
        }
    } else {
        usz init_r = self.compile_to_temp(expr.let_expr.init);
        self.emit_indent();
        if (!already) {
            self.emit("lisp::Value* ");
            self.mark_declared(expr.let_expr.name);
        }
        self.emit_symbol_name(expr.let_expr.name);
        self.emit(" = ");
        self.emit_temp_ref(init_r);
        self.emit(";\n");
    }

    return self.compile_to_temp_tail(expr.let_expr.body);
}

fn usz Compiler.compile_and_flat(Compiler* self, Expr* expr) {
    usz left = self.compile_to_temp(expr.and_expr.left);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(";\n");
    self.emit_indent();
    self.emit("if (aot::is_truthy(");
    self.emit_temp_ref(left);
    self.emit(")) {\n");
    self.indent++;
    usz right = self.compile_to_temp(expr.and_expr.right);
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = ");
    self.emit_temp_ref(right);
    self.emit(";\n");
    self.indent--;
    self.emit_indent();
    self.emit("} else {\n");
    self.indent++;
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = ");
    self.emit_temp_ref(left);
    self.emit(";\n");
    self.indent--;
    self.emit_indent();
    self.emit("}\n");
    return id;
}

fn usz Compiler.compile_or_flat(Compiler* self, Expr* expr) {
    usz left = self.compile_to_temp(expr.or_expr.left);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(";\n");
    self.emit_indent();
    self.emit("if (aot::is_truthy(");
    self.emit_temp_ref(left);
    self.emit(")) {\n");
    self.indent++;
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = ");
    self.emit_temp_ref(left);
    self.emit(";\n");
    self.indent--;
    self.emit_indent();
    self.emit("} else {\n");
    self.indent++;
    usz right = self.compile_to_temp(expr.or_expr.right);
    self.emit_indent();
    self.emit_temp_ref(id);
    self.emit(" = ");
    self.emit_temp_ref(right);
    self.emit(";\n");
    self.indent--;
    self.emit_indent();
    self.emit("}\n");
    return id;
}

fn usz Compiler.compile_begin_flat(Compiler* self, Expr* expr) {
    usz count = expr.begin.expr_count;
    if (count == 0) {
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::make_nil();\n");
        return id;
    }
    // Compile all but last as discarded statements
    for (usz i = 0; i < count - 1; i++) {
        self.compile_to_temp(expr.begin.exprs[i]);
    }
    // Last expression is the result
    return self.compile_to_temp(expr.begin.exprs[count - 1]);
}

fn usz Compiler.compile_set_flat(Compiler* self, Expr* expr) {
    if (self.is_mutable_captured_var(expr.set_expr.name)) {
        usz val_r = self.compile_to_temp(expr.set_expr.value);
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::set_var(\"");
        char[] set_name = self.interp.symbols.get_name(expr.set_expr.name);
        self.emit_escaped(set_name);
        self.emit("\", ");
        self.emit_temp_ref(val_r);
        self.emit(");\n");
        return id;
    } else {
        usz val_r = self.compile_to_temp(expr.set_expr.value);
        self.emit_indent();
        self.emit_symbol_name(expr.set_expr.name);
        self.emit(" = ");
        self.emit_temp_ref(val_r);
        self.emit(";\n");
        // Result is the variable itself
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = ");
        self.emit_symbol_name(expr.set_expr.name);
        self.emit(";\n");
        return id;
    }
}

fn bool Compiler.is_builtin_primitive(Compiler* self, SymbolId sym) {
    // Check ONLY the hardcoded primitives list, NOT defined_globals.
    // Used for delegation injection — defined globals still need injection
    // into the interpreter env since they're C3 locals, not interpreter vars.
    char[] name = self.interp.symbols.get_name(sym);
    char[][*] primitives = {
        "+", "-", "*", "/", "%",
        "=", "<", ">", "<=", ">=",
        "cons", "car", "cdr", "null?", "pair?",
        "list", "length", "not",
        "print", "println", "newline", "display",
        "string-append", "string-join", "substring",
        "string-split", "string-length", "string->list",
        "list->string", "string-upcase", "string-downcase",
        "string-trim", "string?", "int?", "symbol?",
        "closure?", "continuation?", "double?", "list?",
        "boolean?", "number?",
        "string->number", "number->string",
        "gensym", "load", "apply", "equal?",
        "dict", "dict-set!", "dict?",
        "ref", "push!", "keys", "values", "has?", "remove!",
        "read-file", "write-file", "file-exists?", "read-lines",
        "type-of",
        "abs", "min", "max", "floor", "ceiling", "round",
        "truncate", "sqrt", "even?", "odd?", "zero?",
        "positive?", "negative?", "gcd",
        "bitwise-and", "bitwise-or", "bitwise-xor",
        "bitwise-not", "lshift", "rshift",
        "true", "false", "nil"
    };
    foreach (prim : primitives) {
        if (self.str_eq(name, prim)) return true;
    }
    return false;
}

// compile_delegate_flat — REMOVED: All expression types now compiled natively.
// Delegation via rt_eval_source() is no longer used.

/**
 * compile_qq_flat — Flat-style quasiquote compilation.
 * Emits statements that build the quoted structure, storing result in _rN.
 * At depth 0, unquote sub-expressions are compiled as normal expressions.
 */
fn usz Compiler.compile_qq_flat(Compiler* self, Expr* tmpl, usz depth) {
    if (tmpl == null) {
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::make_nil();\n");
        return id;
    }

    switch (tmpl.tag) {
        case E_UNQUOTE: {
            if (depth == 0) {
                // Evaluate the unquoted expression
                return self.compile_to_temp(tmpl.unquote.body);
            }
            // Nested: build (unquote <inner>)
            usz inner_r = self.compile_qq_flat(tmpl.unquote.body, depth - 1);
            usz nil_r = self.next_result();
            self.emit_temp_decl(nil_r);
            self.emit(" = aot::make_nil();\n");

            usz rest_r = self.next_result();
            self.emit_temp_decl(rest_r);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(inner_r);
            self.emit(", ");
            self.emit_temp_ref(nil_r);
            self.emit(");\n");

            usz sym_r = self.next_result();
            self.emit_temp_decl(sym_r);
            self.emit(" = aot::make_symbol(\"unquote\");\n");

            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(sym_r);
            self.emit(", ");
            self.emit_temp_ref(rest_r);
            self.emit(");\n");
            return id;
        }
        case E_UNQUOTE_SPLICING: {
            if (depth == 0) {
                // Splicing at top level is an error
                usz id = self.next_result();
                self.emit_temp_decl(id);
                self.emit(" = aot::make_nil(); /* ERROR: ,@ in non-list context */\n");
                return id;
            }
            // Nested: build (unquote-splicing <inner>)
            usz inner_r = self.compile_qq_flat(tmpl.unquote_splicing.body, depth - 1);
            usz nil_r = self.next_result();
            self.emit_temp_decl(nil_r);
            self.emit(" = aot::make_nil();\n");

            usz rest_r = self.next_result();
            self.emit_temp_decl(rest_r);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(inner_r);
            self.emit(", ");
            self.emit_temp_ref(nil_r);
            self.emit(");\n");

            usz sym_r = self.next_result();
            self.emit_temp_decl(sym_r);
            self.emit(" = aot::make_symbol(\"unquote-splicing\");\n");

            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(sym_r);
            self.emit(", ");
            self.emit_temp_ref(rest_r);
            self.emit(");\n");
            return id;
        }
        case E_QUASIQUOTE: {
            // Nested quasiquote: build (quasiquote <inner>)
            usz inner_r = self.compile_qq_flat(tmpl.quasiquote.body, depth + 1);
            usz nil_r = self.next_result();
            self.emit_temp_decl(nil_r);
            self.emit(" = aot::make_nil();\n");

            usz rest_r = self.next_result();
            self.emit_temp_decl(rest_r);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(inner_r);
            self.emit(", ");
            self.emit_temp_ref(nil_r);
            self.emit(");\n");

            usz sym_r = self.next_result();
            self.emit_temp_decl(sym_r);
            self.emit(" = aot::make_symbol(\"quasiquote\");\n");

            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(sym_r);
            self.emit(", ");
            self.emit_temp_ref(rest_r);
            self.emit(");\n");
            return id;
        }
        case E_LIT: {
            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = ");
            self.compile_literal(tmpl.lit.value);
            self.emit(";\n");
            return id;
        }
        case E_VAR: {
            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = aot::make_symbol(\"");
            char[] name = self.interp.symbols.get_name(tmpl.var_expr.name);
            self.emit_escaped(name);
            self.emit("\");\n");
            return id;
        }
        case E_QUOTE: {
            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = ");
            self.compile_literal(tmpl.quote.datum);
            self.emit(";\n");
            return id;
        }
        case E_APP: {
            // Two-element list: (func arg)
            usz func_r = self.compile_qq_flat(tmpl.app.func, depth);
            usz arg_r = self.compile_qq_flat(tmpl.app.arg, depth);

            usz nil_r = self.next_result();
            self.emit_temp_decl(nil_r);
            self.emit(" = aot::make_nil();\n");

            usz inner_r = self.next_result();
            self.emit_temp_decl(inner_r);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(arg_r);
            self.emit(", ");
            self.emit_temp_ref(nil_r);
            self.emit(");\n");

            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(func_r);
            self.emit(", ");
            self.emit_temp_ref(inner_r);
            self.emit(");\n");
            return id;
        }
        case E_CALL:
            return self.compile_qq_call_flat(tmpl, depth);
        default: {
            usz id = self.next_result();
            self.emit_temp_decl(id);
            self.emit(" = aot::make_nil();\n");
            return id;
        }
    }
}

/**
 * compile_qq_call_flat — Flat-style compilation of E_CALL in QQ context.
 * Builds cons chain from elements, handling ,@ splicing via rt_list_append.
 */
fn usz Compiler.compile_qq_call_flat(Compiler* self, Expr* tmpl, usz depth) {
    usz total = tmpl.call.arg_count + 1;
    Expr*[17] elements;
    elements[0] = tmpl.call.func;
    for (usz i = 0; i < tmpl.call.arg_count && i < 16; i++) {
        elements[i + 1] = tmpl.call.args[i];
    }

    // Check for splicing
    bool has_splice = false;
    if (depth == 0) {
        for (usz i = 0; i < total; i++) {
            if (elements[i].tag == E_UNQUOTE_SPLICING) {
                has_splice = true;
                break;
            }
        }
    }

    if (!has_splice) {
        // No splicing: build cons chain right to left
        // Start with nil, then cons each element from the end
        usz result_r = self.next_result();
        self.emit_temp_decl(result_r);
        self.emit(" = aot::make_nil();\n");

        for (usz i = total; i > 0; i--) {
            usz elem_r = self.compile_qq_flat(elements[i - 1], depth);
            usz new_r = self.next_result();
            self.emit_temp_decl(new_r);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(elem_r);
            self.emit(", ");
            self.emit_temp_ref(result_r);
            self.emit(");\n");
            result_r = new_r;
        }
        return result_r;
    }

    // Has splicing: process elements right to left
    // For each element:
    //   - if ,@ at depth 0: compile inner, append to accumulated result
    //   - otherwise: compile qq, cons to accumulated result
    usz result_r = self.next_result();
    self.emit_temp_decl(result_r);
    self.emit(" = aot::make_nil();\n");

    for (usz i = total; i > 0; i--) {
        Expr* elem = elements[i - 1];
        if (elem.tag == E_UNQUOTE_SPLICING && depth == 0) {
            // Splice: compile the inner expression and append it before current result
            usz splice_r = self.compile_to_temp(elem.unquote_splicing.body);
            usz new_r = self.next_result();
            self.emit_temp_decl(new_r);
            self.emit(" = aot::list_append(");
            self.emit_temp_ref(splice_r);
            self.emit(", ");
            self.emit_temp_ref(result_r);
            self.emit(");\n");
            result_r = new_r;
        } else {
            // Normal element: compile qq and cons
            usz elem_r = self.compile_qq_flat(elem, depth);
            usz new_r = self.next_result();
            self.emit_temp_decl(new_r);
            self.emit(" = aot::cons(");
            self.emit_temp_ref(elem_r);
            self.emit(", ");
            self.emit_temp_ref(result_r);
            self.emit(");\n");
            result_r = new_r;
        }
    }
    return result_r;
}

// =============================================================================
// SECTION: NATIVE EFFECT COMPILATION (flat style)
// =============================================================================

fn usz Compiler.compile_reset_flat(Compiler* self, Expr* expr) {
    // Body has been wrapped as a lambda during prescan
    usz body_r = self.compile_to_temp(expr.reset.body);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::compiled_reset(");
    self.emit_temp_ref(body_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_shift_flat(Compiler* self, Expr* expr) {
    // Body has been wrapped as lambda (lambda (k) ...) during prescan
    usz body_r = self.compile_to_temp(expr.shift.body);
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::compiled_shift(");
    self.emit_temp_ref(body_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_perform_flat(Compiler* self, Expr* expr) {
    // Compile the argument
    usz arg_r = self.compile_to_temp(expr.perform.arg);
    // Create tag symbol
    usz tag_r = self.next_result();
    self.emit_temp_decl(tag_r);
    self.emit(" = aot::make_symbol(\"");
    char[] tag_name = self.interp.symbols.get_name(expr.perform.tag);
    self.emit_escaped(tag_name);
    self.emit("\");\n");

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::compiled_signal(");
    self.emit_temp_ref(tag_r);
    self.emit(", ");
    self.emit_temp_ref(arg_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_handle_flat(Compiler* self, Expr* expr) {
    usz clause_count = expr.handle.clause_count;

    // Compile all handler closures and tags
    usz[8] handler_rs;
    usz[8] tag_rs;
    for (usz i = 0; i < clause_count && i < 8; i++) {
        // Tag symbol
        tag_rs[i] = self.next_result();
        self.emit_temp_decl(tag_rs[i]);
        self.emit(" = aot::make_symbol(\"");
        char[] tag_name = self.interp.symbols.get_name(expr.handle.clauses[i].effect_tag);
        self.emit_escaped(tag_name);
        self.emit("\");\n");

        // Handler closure (synthetic lambda from prescan)
        handler_rs[i] = self.compile_to_temp(expr.handle.clauses[i].handler_body);
    }

    // Compile body closure (synthetic lambda from prescan)
    usz body_r = self.compile_to_temp(expr.handle.body);

    // Build tags and handlers arrays (C3 array syntax: Type[N] name)
    usz tags_arr = self.next_result();
    self.emit_indent();
    self.emit("lisp::Value*[");
    self.emit_usz(clause_count > 0 ? clause_count : 1);
    self.emit("] _eff_tags_");
    self.emit_usz(tags_arr);
    self.emit(";\n");

    usz hdlrs_arr = self.next_result();
    self.emit_indent();
    self.emit("lisp::Value*[");
    self.emit_usz(clause_count > 0 ? clause_count : 1);
    self.emit("] _eff_hdlrs_");
    self.emit_usz(hdlrs_arr);
    self.emit(";\n");

    for (usz i = 0; i < clause_count && i < 8; i++) {
        self.emit_indent();
        self.emit("_eff_tags_");
        self.emit_usz(tags_arr);
        self.emit("[");
        self.emit_usz(i);
        self.emit("] = ");
        self.emit_temp_ref(tag_rs[i]);
        self.emit(";\n");

        self.emit_indent();
        self.emit("_eff_hdlrs_");
        self.emit_usz(hdlrs_arr);
        self.emit("[");
        self.emit_usz(i);
        self.emit("] = ");
        self.emit_temp_ref(handler_rs[i]);
        self.emit(";\n");
    }

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::compiled_handle(&_eff_tags_");
    self.emit_usz(tags_arr);
    self.emit(", &_eff_hdlrs_");
    self.emit_usz(hdlrs_arr);
    self.emit(", ");
    self.emit_usz(clause_count);
    self.emit(", ");
    self.emit_temp_ref(body_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_lambda_flat(Compiler* self, Expr* expr) {
    // Find the lambda definition we created earlier
    usz lambda_id = 0;
    LambdaDef* found_def = null;

    foreach (&def : self.lambda_defs) {
        if (def.body == expr.lambda.body && (uint)def.param == (uint)expr.lambda.param) {
            lambda_id = def.id;
            found_def = def;
            break;
        }
    }

    if (found_def != null && found_def.capture_count > 0) {
        // Closure with captures - allocate on heap
        self.emit_indent();
        self.emit("Lambda_");
        self.emit_usz(lambda_id);
        self.emit("* _closure_data_");
        self.emit_usz(lambda_id);
        self.emit(" = (Lambda_");
        self.emit_usz(lambda_id);
        self.emit("*)mem::malloc(Lambda_");
        self.emit_usz(lambda_id);
        self.emit(".sizeof);\n");

        // Set captured values
        for (usz i = 0; i < found_def.capture_count; i++) {
            self.emit_indent();
            self.emit("_closure_data_");
            self.emit_usz(lambda_id);
            self.emit(".captured_");
            self.emit_symbol_name(found_def.captures[i]);
            self.emit(" = ");
            self.emit_symbol_name(found_def.captures[i]);
            self.emit(";\n");
        }

        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::");
        self.emit_make_closure_call(lambda_id);
        self.emit("((void*)_closure_data_");
        self.emit_usz(lambda_id);
        self.emit(", &invoke_lambda_");
        self.emit_usz(lambda_id);
        self.emit(");\n");
        return id;
    } else {
        // Simple closure without captures
        usz id = self.next_result();
        self.emit_temp_decl(id);
        self.emit(" = aot::");
        self.emit_make_closure_call(lambda_id);
        self.emit("(null, &invoke_lambda_");
        self.emit_usz(lambda_id);
        self.emit(");\n");
        return id;
    }
}

fn usz Compiler.compile_match_flat(Compiler* self, Expr* expr) {
    // Compile scrutinee to temp
    usz scrutinee_r = self.compile_to_temp(expr.match.scrutinee);

    // Declare result temp
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_nil();\n");

    for (usz i = 0; i < expr.match.clause_count; i++) {
        self.emit_indent();
        if (i > 0) {
            self.emit("else ");
        }
        self.emit("if (");
        // Use temp ref name for pattern checks
        char[16] ref_name;
        usz ref_len = io::bprintf(&ref_name, "_r%d", (int)scrutinee_r)!!.len;
        self.compile_pattern_check(expr.match.clauses[i].pattern, ref_name[:ref_len]);
        self.emit(") {\n");
        self.indent++;

        // Bind pattern variables
        self.emit_indent();
        self.compile_pattern_bindings(expr.match.clauses[i].pattern, ref_name[:ref_len]);

        // Compile result
        usz clause_r = self.compile_to_temp(expr.match.clauses[i].result);
        self.emit_indent();
        self.emit_temp_ref(id);
        self.emit(" = ");
        self.emit_temp_ref(clause_r);
        self.emit(";\n");
        self.indent--;
        self.emit_indent();
        self.emit("} ");
    }

    self.emit("\n");
    return id;
}

fn void Compiler.compile_literal(Compiler* self, Value* v) {
    if (v == null || v.tag == NIL) {
        self.emit("aot::make_nil()");
        return;
    }

    switch (v.tag) {
        case INT:
            self.emit("aot::make_int(");
            self.emit_int(v.int_val);
            self.emit(")");

        case DOUBLE:
            self.emit("aot::make_double(");
            char[64] dbuf;
            char[] dslice = io::bprintf(&dbuf, "%.17g", v.double_val)!!;
            self.emit(dslice);
            self.emit(")");

        case STRING:
            self.emit("aot::make_string(\"");
            // Emit string with escaping
            for (usz i = 0; i < v.str_val.len; i++) {
                char c = v.str_val.chars[i];
                if (c == '"') {
                    self.emit("\\\"");
                } else if (c == '\\') {
                    self.emit("\\\\");
                } else if (c == '\n') {
                    self.emit("\\n");
                } else if (c == '\t') {
                    self.emit("\\t");
                } else {
                    self.emit_char(c);
                }
            }
            self.emit("\")");

        case SYMBOL:
            self.emit("aot::make_symbol(\"");
            char[] name = self.interp.symbols.get_name(v.sym_val);
            self.emit_escaped(name);
            self.emit("\")");

        case CONS:
            self.emit("aot::cons(");
            self.compile_literal(v.cons_val.car);
            self.emit(", ");
            self.compile_literal(v.cons_val.cdr);
            self.emit(")");

        default:
            self.emit("aot::make_nil()");
    }
}

fn void Compiler.compile_var(Compiler* self, Expr* expr) {
    SymbolId name = expr.var_expr.name;

    // O(1) hash lookup for known primitives and literals
    char[] emit_code = prim_hash_lookup(name);
    if (emit_code.len > 0) {
        // D2: For actual primitives (lookup_prim calls), use cached global instead.
        // Literals (true/false/nil) are cheap constructors — emit inline.
        if (emit_code.len > 18 && emit_code[0] == 'a' && emit_code[4] == ':') {
            // Starts with "aot::" — it's a lookup_prim call, use cached global
            self.record_prim_ref(name);
            self.emit_prim_global_name(name);
        } else {
            // Literal (make_true, make_false, make_nil) — emit inline
            self.emit(emit_code);
        }
        return;
    }

    // Mutable-captured variable: look up from interpreter env
    if (self.is_mutable_captured_var(name)) {
        char[] sym_name = self.interp.symbols.get_name(name);
        self.emit("aot::lookup_var(\"");
        self.emit_escaped(sym_name);
        self.emit("\")");
        return;
    }

    // Regular variable reference
    self.emit_symbol_name(name);
}

fn void Compiler.compile_quote(Compiler* self, Value* datum) {
    // Quote compiles to literal value construction
    self.compile_literal(datum);
}

fn void Compiler.compile_pattern_check(Compiler* self, Pattern* pat, char[] val_name) {
    if (pat == null) {
        self.emit("true");
        return;
    }

    switch (pat.tag) {
        case PAT_WILDCARD:
            self.emit("true");

        case PAT_VAR:
            self.emit("true");

        case PAT_LIT:
            self.emit("aot::values_equal(");
            self.emit(val_name);
            self.emit(", ");
            self.compile_literal(pat.lit_value);
            self.emit(")");

        case PAT_QUOTE:
            self.emit("aot::values_equal(");
            self.emit(val_name);
            self.emit(", ");
            self.compile_literal(pat.quote_datum);
            self.emit(")");

        case PAT_SEQ:
            // Check list length and elements
            self.emit("aot::is_list(");
            self.emit(val_name);
            self.emit(")");

            if (pat.rest_pos == REST_NONE) {
                // Exact length match
                self.emit(" && aot::list_length(");
                self.emit(val_name);
                self.emit(") == ");
                self.emit_usz(pat.elem_count);
            } else if (pat.rest_pos == REST_START || pat.rest_pos == REST_MIDDLE || pat.rest_pos == REST_END) {
                // At least N elements
                self.emit(" && aot::list_length(");
                self.emit(val_name);
                self.emit(") >= ");
                self.emit_usz(pat.elem_count);
            }

        case PAT_GUARD:
            // Guard patterns: delegate to true for now (guard check at runtime)
            self.emit("true");

        default:
            self.emit("true");
    }
}

fn void Compiler.emit_pattern_var_decl(Compiler* self, SymbolId name) {
    if (!self.is_declared(name)) {
        self.emit("lisp::Value* ");
        self.mark_declared(name);
    }
}

fn void Compiler.compile_pattern_bindings(Compiler* self, Pattern* pat, char[] val_name) {
    if (pat == null) return;

    switch (pat.tag) {
        case PAT_VAR:
            self.emit_pattern_var_decl(pat.var_name);
            self.emit_symbol_name(pat.var_name);
            self.emit(" = ");
            self.emit(val_name);
            self.emit("; ");

        case PAT_SEQ:
            // Bind sequence elements
            for (usz i = 0; i < pat.elem_count; i++) {
                // Generate element access — index depends on rest position
                self.emit("lisp::Value* _elem_");
                self.emit_usz(i);
                if (pat.rest_pos == REST_START) {
                    // [.. a b] — elements are at end of list
                    self.emit(" = aot::list_nth(");
                    self.emit(val_name);
                    self.emit(", aot::list_length(");
                    self.emit(val_name);
                    self.emit(") - ");
                    self.emit_usz(pat.elem_count - i);
                    self.emit("); ");
                } else {
                    // REST_NONE, REST_END, REST_MIDDLE — elements at start
                    self.emit(" = aot::list_nth(");
                    self.emit(val_name);
                    self.emit(", ");
                    self.emit_usz(i);
                    self.emit("); ");
                }

                // Recursively bind
                if (pat.elements[i].tag == PAT_VAR) {
                    self.emit_pattern_var_decl(pat.elements[i].var_name);
                    self.emit_symbol_name(pat.elements[i].var_name);
                    self.emit(" = _elem_");
                    self.emit_usz(i);
                    self.emit("; ");
                }
            }

            // Bind rest if REST_MIDDLE
            if (pat.rest_pos == REST_MIDDLE) {
                self.emit_pattern_var_decl(pat.rest_binding);
                self.emit_symbol_name(pat.rest_binding);
                self.emit(" = aot::list_rest(");
                self.emit(val_name);
                self.emit(", ");
                self.emit_usz(pat.elem_count);
                self.emit("); ");
            }

        case PAT_GUARD:
            // Guard pattern: bind sub-pattern if present
            if (pat.guard_sub != null) {
                self.compile_pattern_bindings(pat.guard_sub, val_name);
            }

        default:
            return;
    }
}

/**
 * compile_path — Field access: point.x, person.address.city
 * person.address.city => rt_field_access(rt_field_access(person, "address"), "city")
 */
fn void Compiler.compile_path(Compiler* self, Expr* expr) {
    if (expr.path.segment_count == 0) {
        self.emit("aot::make_nil()");
        return;
    }

    // First segment is a variable, remaining segments are field lookups
    for (usz i = 1; i < expr.path.segment_count; i++) {
        self.emit("aot::field_access(");
    }

    // Emit first segment as variable reference
    self.emit_symbol_name(expr.path.segments[0]);

    // Emit remaining segments as string field names (wrapped in make_string for Value type)
    for (usz i = 1; i < expr.path.segment_count; i++) {
        self.emit(", aot::make_string(\"");
        char[] name = self.interp.symbols.get_name(expr.path.segments[i]);
        self.emit_escaped(name);
        self.emit("\"))");
    }
}

// =============================================================================
// SECTION 8b2: TAIL-POSITION COMPILATION (TCO)
// =============================================================================

/**
 * compile_module_flat — Flat-style module compilation.
 * Compiles module body expressions inline, registers module in table.
 */
fn usz Compiler.compile_module_flat(Compiler* self, Expr* expr) {
    // Register the module in compiled_modules table
    if (self.compiled_module_count >= self.compiled_module_capacity) {
        usz new_cap = self.compiled_module_capacity * 2;
        CompiledModule* new_mods = (CompiledModule*)mem::malloc(CompiledModule.sizeof * new_cap);
        for (usz i = 0; i < self.compiled_module_count; i++) new_mods[i] = self.compiled_modules[i];
        mem::free(self.compiled_modules);
        self.compiled_modules = new_mods;
        self.compiled_module_capacity = new_cap;
    }
    {
        CompiledModule* mod = &self.compiled_modules[self.compiled_module_count];
        mod.name = expr.module_expr.name;
        usz cap = expr.module_expr.export_count < 16 ? 16 : expr.module_expr.export_count;
        mod.exports = (SymbolId*)mem::malloc(SymbolId.sizeof * cap);
        mod.export_capacity = cap;
        mod.export_count = expr.module_expr.export_count;
        for (usz i = 0; i < expr.module_expr.export_count; i++) {
            mod.exports[i] = expr.module_expr.exports[i];
        }
        mod.loaded = true;
        self.compiled_module_count++;
    }

    // Compile body expressions inline
    usz last_id = 0;
    for (usz i = 0; i < expr.module_expr.body_count; i++) {
        Expr* body_expr = expr.module_expr.body[i];
        if (body_expr.tag == E_DEFINE) {
            // Define: compile init, assign to global
            usz init_r = self.compile_to_temp(body_expr.define.value);
            self.emit_indent();
            self.emit_symbol_name(body_expr.define.name);
            self.emit(" = ");
            self.emit_temp_ref(init_r);
            self.emit(";\n");
            last_id = init_r;
        } else {
            // Non-define: compile inline
            last_id = self.compile_to_temp(body_expr);
        }
    }

    // Module evaluates to nil
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_nil();\n");
    return id;
}

/**
 * compile_import_flat — Flat-style import compilation.
 * For inline-compiled modules, all definitions are already globals — no-op.
 */
fn usz Compiler.compile_import_flat(Compiler* self, Expr* expr) {
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_nil(); // import: inline module\n");
    return id;
}

/**
 * compile_export_from_flat — export-from is a no-op in compiled mode
 * (re-exported symbols are already inlined as globals).
 */
fn usz Compiler.compile_export_from_flat(Compiler* self, Expr* expr) {
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_nil(); // export-from: no-op in compiled mode\n");
    return id;
}

// =============================================================================
// SECTION 9: TOP-LEVEL COMPILE FUNCTION
// =============================================================================

/**
 * Compile Lisp source code to C3 source code.
 * Returns the generated C3 code as a string.
 */
fn char[] compile_to_c3(char[] source, Interp* interp) {
    return compile_to_c3_ext(source, interp, false, false);
}

fn char[] compile_to_c3_ext(char[] source, Interp* interp, bool print_last, bool print_all) {
    Compiler compiler;
    compiler.init(interp);
    compiler.print_last = print_last;
    compiler.print_all = print_all;
    return compiler.compile_program(source);
}

/**
 * Compile Lisp source code to C3 source code with print_last mode.
 * The last non-define expression is wrapped with rt_print_value + printn
 * so the compiled binary prints its result.
 */
fn char[] compile_to_c3_with_print(char[] source, Interp* interp) {
    Compiler compiler;
    compiler.init(interp);
    compiler.print_last = true;

    char[] result = compiler.compile_program(source);

    // Note: caller must copy result before compiler is freed
    return result;
}

/**
 * Compile Lisp source code to C3 source code with print_all mode.
 * Every non-define expression is wrapped with rt_print_value + printn
 * so the compiled binary prints all results (for e2e testing).
 */
fn char[] compile_to_c3_print_all(char[] source, Interp* interp) {
    Compiler compiler;
    compiler.init(interp);
    compiler.print_all = true;

    char[] result = compiler.compile_program(source);

    // Note: caller must copy result before compiler is freed
    return result;
}
