module lisp;

import std::collections::list;
import std::io;
import main;

// =============================================================================
// SECTION 8: INTERPRETER STATE
// =============================================================================
// =============================================================================
// SECTION 8: INTERPRETER STATE
// =============================================================================

/**
 * InterpFlags — Packed boolean state flags for the interpreter.
 */
bitstruct InterpFlags : ushort {
    bool jit_enabled      : 0;  // true after init, enables JIT-first execution
    bool jit_tco_bounce   : 1;  // True = return sentinel, jit_eval loop continues
    bool raise_pending    : 2;  // raise_error found handler, pending dispatch
}

/**
 * Interp — Interpreter state.
 */
struct Interp {
    SymbolTable  symbols;
    Env*         global_env;

    // Region-based allocation (Expr/Pattern only)
    main::RegionHandle root_region;      // For Expr, Pattern allocation (AST nodes are permanent)

    // Scope-region allocation (Values, Envs, closures)
    main::ScopeRegion* root_scope;       // Permanent scope for defines (lives forever)
    main::ScopeRegion* current_scope;    // Current scope (changes at let/lambda/loop)

    // Pre-interned common symbols
    SymbolId sym_lambda;
    SymbolId sym_if;
    SymbolId sym_let;
    SymbolId sym_define;
    SymbolId sym_quote;
    SymbolId sym_reset;
    SymbolId sym_shift;
    SymbolId sym_perform;
    SymbolId sym_signal;
    SymbolId sym_handle;
    SymbolId sym_resolve;
    SymbolId sym_with_continuation;
    SymbolId sym__k;       // hidden continuation name for match-style handle clauses
    SymbolId sym_match;
    SymbolId sym_and;
    SymbolId sym_or;
    SymbolId sym_true;
    SymbolId sym_false;
    SymbolId sym_begin;
    SymbolId sym_set;
    SymbolId sym_quasiquote;
    SymbolId sym_unquote;
    SymbolId sym_unquote_splicing;
    SymbolId sym_defmacro;
    SymbolId sym_dotdot;    // ".." symbol for template splice detection
    SymbolId sym_macro;     // "macro" symbol for [macro] annotation

    // Type system symbols
    SymbolId sym_type;      // "type" for [type] annotation
    SymbolId sym_abstract;  // "abstract" for [abstract] annotation
    SymbolId sym_union;     // "union" for [union] annotation
    SymbolId sym_alias;     // "alias" for [alias] annotation
    SymbolId sym_effect;    // "effect" for [effect] annotation

    // FFI annotation symbols
    SymbolId sym_ffi;       // "ffi" for [ffi ...] annotation
    SymbolId sym_lib;       // "lib" for [ffi lib]
    SymbolId sym_Ptr;       // "Ptr" for ^Ptr type
    SymbolId sym_Void;      // "Void" for ^Void type

    // Built-in type name symbols
    SymbolId sym_Int;
    SymbolId sym_Double;
    SymbolId sym_String;
    SymbolId sym_Symbol;
    SymbolId sym_List;
    SymbolId sym_Bool;
    SymbolId sym_Nil;
    SymbolId sym_Closure;
    SymbolId sym_Array;
    SymbolId sym_Dict;
    SymbolId sym_Any;
    SymbolId sym_Val;
    SymbolId sym_Number;
    SymbolId sym_Collection;
    SymbolId sym_Iterator;

    // Error-as-effect
    SymbolId sym_raise;

    // Cached cons path symbols
    SymbolId sym_car;
    SymbolId sym_cdr;

    // Placeholder/pipe/guard symbols
    SymbolId sym_placeholder;  // "__placeholder" sentinel for _ in expression context
    SymbolId sym_pipe;         // "|>" pipe operator
    SymbolId sym_question;     // "?" guard pattern

    // Effect fast-path dispatch table: maps effect tag → raw primitive
    // When a signal has no handler, the fast path looks up this table.
    FastPathEntry[32] fast_path_table;
    usz fast_path_count;
    bool jit_debug_trace;  // trace JIT compile/exec steps

    // Cached TypeIds for built-in types (avoid repeated hash lookups)
    TypeId tid_Int;
    TypeId tid_Double;
    TypeId tid_String;
    TypeId tid_Symbol;
    TypeId tid_List;
    TypeId tid_Bool;
    TypeId tid_Nil;
    TypeId tid_Closure;
    TypeId tid_Array;
    TypeId tid_Dict;
    TypeId tid_Any;
    TypeId tid_Number;
    TypeId tid_Collection;
    TypeId tid_Iterator;

    // Type registry
    TypeRegistry types;
    int constructor_type_id;  // Set before calling type constructor primitive
    void* prim_user_data;     // Set before calling primitive with user_data

    // Packed boolean state flags
    InterpFlags flags;

    // Pending raise error message (for raise_error → jit_handle_impl dispatch)
    char[256] raise_msg;
    usz raise_msg_len;

    // JIT execution mode
    Env* jit_env;      // Current local env for JIT sub-expression evaluation
    Env* match_env;    // Current env for evaluating guard predicates in match

    // JIT trampoline TCO state
    Expr* jit_tco_expr;        // Next expression to evaluate (set by tail-call helpers)
    Env*  jit_tco_env;         // Environment for next expression
    main::ScopeRegion* tco_recycle_scope; // Scope eligible for TCO recycling (set by jit_eval_in_call_scope)
    usz tco_scope_defer_slot;             // Defer slot index for call-scope cleanup retargeting
    bool tco_scope_defer_active;          // True when call-scope defer slot is valid
    main::ScopeRegion* releasing_scope;   // Set during copy_to_parent to skip values not in this scope
    bool escape_env_mode;                 // When true, make_env/make_cons use current scope ESCAPE lane
    uint promotion_epoch_counter;         // Monotonic epoch counter for boundary promotion contexts
    PromotionContext* active_promotion_ctx; // Current boundary promotion context (nullable)

    // Stack overflow protection
    usz eval_depth;
    usz max_eval_depth;

    // Macro table (dynamic)
    MacroDef* macro_table;
    usz macro_count;
    usz macro_capacity;
    usz gensym_counter;
    usz* macro_hash_index;
    usz macro_hash_capacity;

    // Module system (dynamic)
    Module* modules;
    usz module_count;
    usz module_capacity;
    usz* module_hash_index;
    usz module_hash_capacity;
    SymbolId sym_module;
    SymbolId sym_import;
    SymbolId sym_export;
    SymbolId sym_as;           // ":as" for (import mod (name :as alias))
    SymbolId sym_all;          // ":all" for (import mod :all)
    SymbolId sym_export_from;  // "export-from" for (export-from mod (sym...))

    // Continuation/reset depth (saved/restored across context boundaries)
    usz      reset_depth;

    // Effect handler stack (dynamic)
    EffectHandler* handler_stack;
    usz            handler_count;
    usz            handler_capacity;

    // Last call site symbol (for error messages)
    SymbolId last_call_name;

    // Source file directory stack (for relative import resolution)
    char[256][16] source_dirs;  // stack of directory paths (null-terminated)
    usz source_dir_count;

    // StackCtx-based continuation system (stack engine)
    main::StackPool stack_ctx_pool;
    Value* resume_value;           // Value passed to (k val) — read by shift on resume
    Value* yield_value;            // Value passed by (yield val) — read by resume
    void* current_reset_state;     // Pointer to current ResetState (in jit.c3)

}

fn void interp_init_runtime_state(Interp* self) {
    self.symbols.init();
    self.global_env = null;

    self.root_region = main::thread_root_region();
    self.root_scope = main::scope_create(null);
    self.current_scope = self.root_scope;

    self.handler_capacity = HANDLER_INITIAL_CAPACITY;
    self.handler_count = 0;
    self.handler_stack = (EffectHandler*)mem::malloc(EffectHandler.sizeof * self.handler_capacity);
    self.reset_depth = 0;
    self.source_dir_count = 0;

    main::stack_pool_init(&self.stack_ctx_pool);
    g_stack_ctx_pool = &self.stack_ctx_pool;
    self.resume_value = null;
    self.yield_value = null;
    self.current_reset_state = null;
}

fn void interp_init_core_symbols(Interp* self) {
    self.sym_lambda = self.symbols.intern("lambda");
    self.sym_if = self.symbols.intern("if");
    self.sym_let = self.symbols.intern("let");
    self.sym_define = self.symbols.intern("define");
    self.sym_quote = self.symbols.intern("quote");
    self.sym_reset = self.symbols.intern("reset");
    self.sym_shift = self.symbols.intern("shift");
    self.sym_perform = self.symbols.intern("perform");
    self.sym_signal = self.symbols.intern("signal");
    self.sym_handle = self.symbols.intern("handle");
    self.sym_resolve = self.symbols.intern("resolve");
    self.sym_with_continuation = self.symbols.intern("with-continuation");
    self.sym__k = self.symbols.intern("__k");
    self.sym_match = self.symbols.intern("match");
    self.sym_and = self.symbols.intern("and");
    self.sym_or = self.symbols.intern("or");
    self.sym_true = self.symbols.intern("true");
    self.sym_false = self.symbols.intern("false");
    self.sym_begin = self.symbols.intern("begin");
    self.sym_set = self.symbols.intern("set!");
    self.sym_quasiquote = self.symbols.intern("quasiquote");
    self.sym_unquote = self.symbols.intern("unquote");
    self.sym_unquote_splicing = self.symbols.intern("unquote-splicing");
    self.sym_defmacro = self.symbols.intern("defmacro");
    self.sym_dotdot = self.symbols.intern("..");
    self.sym_macro = self.symbols.intern("macro");
}

fn void interp_init_type_symbols(Interp* self) {
    self.sym_type = self.symbols.intern("type");
    self.sym_abstract = self.symbols.intern("abstract");
    self.sym_union = self.symbols.intern("union");
    self.sym_alias = self.symbols.intern("alias");
    self.sym_effect = self.symbols.intern("effect");

    self.sym_ffi = self.symbols.intern("ffi");
    self.sym_lib = self.symbols.intern("lib");
    self.sym_Ptr = self.symbols.intern("Ptr");
    self.sym_Void = self.symbols.intern("Void");

    self.sym_Int = self.symbols.intern("Int");
    self.sym_Double = self.symbols.intern("Double");
    self.sym_String = self.symbols.intern("String");
    self.sym_Symbol = self.symbols.intern("Symbol");
    self.sym_List = self.symbols.intern("List");
    self.sym_Bool = self.symbols.intern("Bool");
    self.sym_Nil = self.symbols.intern("Nil");
    self.sym_Closure = self.symbols.intern("Closure");
    self.sym_Array = self.symbols.intern("Array");
    self.sym_Dict = self.symbols.intern("Dict");
    self.sym_Any = self.symbols.intern("Any");
    self.sym_Val = self.symbols.intern("Val");
    self.sym_Number = self.symbols.intern("Number");
    self.sym_Collection = self.symbols.intern("Collection");
    self.sym_Iterator = self.symbols.intern("Iterator");
}

fn void interp_init_misc_symbols(Interp* self) {
    self.sym_raise = self.symbols.intern("raise");
    self.sym_car = self.symbols.intern("car");
    self.sym_cdr = self.symbols.intern("cdr");
    self.sym_placeholder = self.symbols.intern("__placeholder");
    self.sym_pipe = self.symbols.intern("|>");
    self.sym_question = self.symbols.intern("?");
}

fn void interp_init_module_state(Interp* self) {
    self.sym_module = self.symbols.intern("module");
    self.sym_import = self.symbols.intern("import");
    self.sym_export = self.symbols.intern("export");
    self.sym_as = self.symbols.intern(":as");
    self.sym_all = self.symbols.intern(":all");
    self.sym_export_from = self.symbols.intern("export-from");
    self.module_count = 0;
    self.module_capacity = MODULE_INITIAL_CAPACITY;
    self.modules = (Module*)mem::malloc(Module.sizeof * self.module_capacity);
    self.module_hash_capacity = self.module_capacity * 2;
    self.module_hash_index = (usz*)mem::malloc(usz.sizeof * self.module_hash_capacity);
    for (usz i = 0; i < self.module_hash_capacity; i++) self.module_hash_index[i] = usz.max;
}

fn void interp_init_runtime_flags(Interp* self) {
    self.flags.jit_enabled = false;
    self.jit_env = null;
    self.jit_tco_expr = null;
    self.jit_tco_env = null;
    self.tco_recycle_scope = null;
    self.tco_scope_defer_slot = 0;
    self.tco_scope_defer_active = false;
    self.releasing_scope = null;
    self.escape_env_mode = false;
    self.promotion_epoch_counter = 0;
    self.active_promotion_ctx = null;
    self.flags.jit_tco_bounce = false;

    self.eval_depth = 0;
    self.max_eval_depth = 1024;
}

fn void interp_init_macro_state(Interp* self) {
    self.macro_count = 0;
    self.macro_capacity = MACRO_INITIAL_CAPACITY;
    self.macro_table = (MacroDef*)mem::malloc(MacroDef.sizeof * self.macro_capacity);
    self.gensym_counter = 0;
    self.macro_hash_capacity = self.macro_capacity * 2;
    self.macro_hash_index = (usz*)mem::malloc(usz.sizeof * self.macro_hash_capacity);
    for (usz i = 0; i < self.macro_hash_capacity; i++) self.macro_hash_index[i] = usz.max;
}

fn void Interp.init(Interp* self) {
    interp_init_runtime_state(self);
    interp_init_core_symbols(self);
    interp_init_type_symbols(self);
    interp_init_misc_symbols(self);
    self.types.init();
    interp_init_module_state(self);
    interp_init_runtime_flags(self);
    interp_init_macro_state(self);

    self.global_env = make_env(self, null);
}

fn void Interp.grow_macro_table(Interp* self) {
    usz new_cap = self.macro_capacity * 2;
    MacroDef* new_table = (MacroDef*)mem::malloc(MacroDef.sizeof * new_cap);
    for (usz i = 0; i < self.macro_count; i++) new_table[i] = self.macro_table[i];
    mem::free(self.macro_table);
    self.macro_table = new_table;
    self.macro_capacity = new_cap;

    // Rebuild hash
    usz new_hash_cap = new_cap * 2;
    usz* new_hash = (usz*)mem::malloc(usz.sizeof * new_hash_cap);
    for (usz i = 0; i < new_hash_cap; i++) new_hash[i] = usz.max;
    mem::free(self.macro_hash_index);
    self.macro_hash_index = new_hash;
    self.macro_hash_capacity = new_hash_cap;

    usz mask = new_hash_cap - 1;
    for (usz i = 0; i < self.macro_count; i++) {
        char[] name_str = self.symbols.get_name(self.macro_table[i].name);
        usz h = hash_symbol(name_str);
        for (usz probe = 0; probe < new_hash_cap; probe++) {
            usz slot = (h + probe) & mask;
            if (new_hash[slot] == usz.max) {
                new_hash[slot] = i;
                break;
            }
        }
    }
}

fn void Interp.grow_module_table(Interp* self) {
    usz new_cap = self.module_capacity * 2;
    Module* new_mods = (Module*)mem::malloc(Module.sizeof * new_cap);
    for (usz i = 0; i < self.module_count; i++) new_mods[i] = self.modules[i];
    mem::free(self.modules);
    self.modules = new_mods;
    self.module_capacity = new_cap;

    // Rebuild hash
    usz new_hash_cap = new_cap * 2;
    usz* new_hash = (usz*)mem::malloc(usz.sizeof * new_hash_cap);
    for (usz i = 0; i < new_hash_cap; i++) new_hash[i] = usz.max;
    mem::free(self.module_hash_index);
    self.module_hash_index = new_hash;
    self.module_hash_capacity = new_hash_cap;

    usz mask = new_hash_cap - 1;
    for (usz i = 0; i < self.module_count; i++) {
        char[] name_str = self.symbols.get_name(self.modules[i].name);
        usz h = hash_symbol(name_str);
        for (usz probe = 0; probe < new_hash_cap; probe++) {
            usz slot = (h + probe) & mask;
            if (new_hash[slot] == usz.max) {
                new_hash[slot] = i;
                break;
            }
        }
    }
}

fn void Interp.grow_handler_stack(Interp* self) {
    usz new_cap = self.handler_capacity * 2;
    EffectHandler* new_stack = (EffectHandler*)mem::malloc(EffectHandler.sizeof * new_cap);
    for (usz i = 0; i < self.handler_count; i++) new_stack[i] = self.handler_stack[i];
    mem::free(self.handler_stack);
    self.handler_stack = new_stack;
    self.handler_capacity = new_cap;
}

fn void Interp.destroy(Interp* self) {
    // Release root scope last among value-owned structures so registered dtors run.
    // This is the primary deterministic cleanup point for scope-managed values/envs.
    if (self.root_scope != null) {
        self.current_scope = self.root_scope;
        self.releasing_scope = null;
        self.tco_recycle_scope = null;
        self.tco_scope_defer_slot = 0;
        self.tco_scope_defer_active = false;
        self.active_promotion_ctx = null;
        main::scope_release(self.root_scope);
        self.root_scope = null;
        self.current_scope = null;
    }

    self.symbols.destroy();
    self.types.destroy();
    if (self.macro_table != null) { mem::free(self.macro_table); self.macro_table = null; }
    if (self.macro_hash_index != null) { mem::free(self.macro_hash_index); self.macro_hash_index = null; }
    if (self.modules != null) {
        for (usz i = 0; i < self.module_count; i++) {
            if (self.modules[i].exports != null) {
                mem::free(self.modules[i].exports);
            }
        }
        mem::free(self.modules);
        self.modules = null;
    }
    if (self.module_hash_index != null) { mem::free(self.module_hash_index); self.module_hash_index = null; }
    if (self.handler_stack != null) { mem::free(self.handler_stack); self.handler_stack = null; }
    main::stack_pool_shutdown(&self.stack_ctx_pool);
    if (g_stack_ctx_pool == &self.stack_ctx_pool) g_stack_ctx_pool = null;
    jit_global_shutdown();
}

fn Continuation* Interp.alloc_lisp_continuation(Interp* self) {
    Continuation* k = (Continuation*)self.root_scope.alloc(Continuation.sizeof);
    k.ctx = null;
    k.is_coroutine_based = false;
    k.handle_state = null;
    return k;
}

fn Value* Interp.alloc_value(Interp* self) @inline {
    Value* v = (Value*)self.current_scope.alloc(Value.sizeof);
    v.tag = NIL;  // Safe default
    v.scope_gen = self.current_scope.generation;  // Stamp for O(1) scope checks
    // Dtor registration moved to heap-backed constructors (make_string, make_closure, etc.)
    // Flat types (NIL, INT, DOUBLE, SYMBOL, CONS, BOOL) skip dtor — no heap backing.
    return v;
}

// Allocate a Value in current scope's ESCAPE lane.
// Not wired into constructors yet; used by dual-lane rollout phases.
fn Value* Interp.alloc_value_escape(Interp* self) @inline {
    Value* v = (Value*)self.current_scope.alloc_escape(Value.sizeof);
    v.tag = NIL;  // Safe default
    v.scope_gen = self.current_scope.escape_generation;
    return v;
}

// Allocate a Value in root_scope (for Expr literals and other permanent values).
// Root-scope dtors only run at program exit, so unconditional registration is fine here.
// The perf win is from removing dtors in alloc_value() (child scopes, per-call teardown).
fn Value* Interp.alloc_value_root(Interp* self) @inline {
    Value* v = (Value*)self.root_scope.alloc(Value.sizeof);
    v.tag = NIL;
    v.scope_gen = self.root_scope.generation;  // Stamp for O(1) scope checks
    main::scope_register_dtor(self.root_scope, (void*)v, &scope_dtor_value);
    return v;
}

fn Env* Interp.alloc_env(Interp* self) @inline {
    Env* e = (Env*)self.current_scope.alloc(Env.sizeof);
    e.bindings = null;
    e.binding_count = 0;
    e.capacity = 0;
    e.parent = null;
    e.hash_table = null;
    e.hash_capacity = 0;
    e.persistent = false;
    e.scope_gen = self.current_scope.generation;
    return e;
}

fn Env* Interp.alloc_env_escape(Interp* self) @inline {
    Env* e = (Env*)self.current_scope.alloc_escape(Env.sizeof);
    e.bindings = null;
    e.binding_count = 0;
    e.capacity = 0;
    e.parent = null;
    e.hash_table = null;
    e.hash_capacity = 0;
    e.persistent = false;
    e.scope_gen = self.current_scope.escape_generation;
    return e;
}

fn Expr* Interp.alloc_expr(Interp* self) {
    Expr e;
    main::ObjectHandle h = main::allocate_in(self.root_region, Expr, e);
    return main::dereference_as(Expr, h);
}

fn Pattern* Interp.alloc_pattern(Interp* self) {
    Pattern p;
    main::ObjectHandle h = main::allocate_in(self.root_region, Pattern, p);
    return main::dereference_as(Pattern, h);
}

