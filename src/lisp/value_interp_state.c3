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

    // Dedicated AST allocator (Expr/Pattern lifetime is interpreter-scoped)
    AstArena ast_arena;

    // Scope-region allocation (Values, Envs, closures)
    main::ScopeRegion* root_scope;       // Permanent scope for defines (lives forever)
    main::ScopeRegion* current_scope;    // Current scope (changes at let/lambda/loop)

    // Pre-interned common symbols
    SymbolId sym_lambda;
    SymbolId sym_if;
    SymbolId sym_let;
    SymbolId sym_define;
    SymbolId sym_quote;
    SymbolId sym_checkpoint;
    SymbolId sym_capture;
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
    SymbolId sym_block;
    SymbolId sym_set;
    SymbolId sym_quasiquote;
    SymbolId sym_unquote;
    SymbolId sym_unquote_splicing;
    SymbolId sym_defmacro;
    SymbolId sym_dotdot;    // ".." symbol for template splice detection
    SymbolId sym_macro;     // "macro" symbol for [macro] annotation

    // Type system symbols
    SymbolId sym_type;      // "type" for [type] annotation
    SymbolId sym_struct;    // "struct" alias for [type] annotation
    SymbolId sym_abstract;  // "abstract" for [abstract] annotation
    SymbolId sym_union;     // "union" for [union] annotation
    SymbolId sym_alias;     // "alias" for [alias] annotation
    SymbolId sym_effect;    // "effect" for [effect] annotation

    // FFI annotation symbols and shared builtin type names
    SymbolId sym_ffi;       // "ffi" for [ffi ...] annotation
    SymbolId sym_lib;       // "lib" for [ffi lib]
    SymbolId sym_Pointer;   // "Pointer" canonical FFI pointer annotation type
    SymbolId sym_Ptr;       // "Ptr" compatibility alias for ^Pointer
    SymbolId sym_Void;      // "Void" for ^Void type and runtime Void value/type

    // Built-in type name symbols
    SymbolId sym_Integer;
    SymbolId sym_Int;
    SymbolId sym_Double;
    SymbolId sym_String;
    SymbolId sym_Symbol;
    SymbolId sym_List;
    SymbolId sym_Boolean;
    SymbolId sym_Bool;
    SymbolId sym_Nil;
    SymbolId sym_Closure;
    SymbolId sym_Array;
    SymbolId sym_Dictionary;
    SymbolId sym_Dict;
    SymbolId sym_Set;
    SymbolId sym_Any;
    SymbolId sym_Value;
    SymbolId sym_Number;
    SymbolId sym_Collection;
    SymbolId sym_Iterator;
    SymbolId sym_TimePoint;
    SymbolId sym_Coroutine;

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
    FastPathEntry[64] fast_path_table;
    usz fast_path_count;
    bool jit_debug_trace;  // trace JIT compile/exec steps

    // Cached TypeIds for built-in types (avoid repeated hash lookups)
    TypeId tid_Integer;
    TypeId tid_Int;
    TypeId tid_Double;
    TypeId tid_String;
    TypeId tid_Symbol;
    TypeId tid_List;
    TypeId tid_Boolean;
    TypeId tid_Bool;
    TypeId tid_Nil;
    TypeId tid_Void;
    TypeId tid_Closure;
    TypeId tid_Array;
    TypeId tid_Dictionary;
    TypeId tid_Dict;
    TypeId tid_Set;
    TypeId tid_Any;
    TypeId tid_Number;
    TypeId tid_Collection;
    TypeId tid_Iterator;
    TypeId tid_TimePoint;
    TypeId tid_Coroutine;

    // Type registry
    TypeRegistry types;
    int constructor_type_id;  // Set before calling type constructor primitive
    void* prim_user_data;     // Set before calling primitive with user_data

    // Packed boolean state flags
    InterpFlags flags;

    // Pending raise error message (for raise_error → jit_handle_impl dispatch)
    char[256] raise_msg;
    usz raise_msg_len;
    Value* raise_payload;  // Optional canonical payload for pending raise dispatch

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

    // Stack overflow diagnostics
    usz eval_depth;

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
    SymbolId sym_as;           // "as" for quoted marker (import mod (name 'as alias))
    SymbolId sym_all;          // "all" for quoted marker (import mod 'all)
    SymbolId sym_export_from;  // "export-from" for (export-from mod (sym...))

    // Continuation/checkpoint depth (saved/restored across context boundaries)
    usz      reset_depth;

    // Effect handler stack (dynamic)
    EffectHandler* handler_stack;
    usz            handler_count;
    usz            handler_capacity;

    // Last call site symbol (for error messages)
    SymbolId last_call_name;

    // Source file directory stack (for relative import resolution)
    char** source_dirs;         // stack of directory paths (heap-allocated strings)
    usz source_dir_count;
    usz source_dir_capacity;

    // StackCtx-based continuation system (stack engine)
    main::StackPool stack_ctx_pool;
    Value* resume_value;           // Value passed to (k val) — read by capture on resume
    Value* yield_value;            // Value passed by (yield val) — read by resume
    main::ScopeRegion* yield_scope; // Scope owning yield_value while coroutine is suspended
    void* current_reset_state;     // Pointer to current ResetState (in jit.c3)

}

fn void interp_init_runtime_state(Interp* self) {
    self.symbols.init();
    self.global_env = null;

    ast_arena_init(&self.ast_arena);
    self.root_scope = main::scope_create(null);
    self.current_scope = self.root_scope;

    self.handler_capacity = HANDLER_INITIAL_CAPACITY;
    self.handler_count = 0;
    self.handler_stack = (EffectHandler*)mem::malloc(EffectHandler.sizeof * self.handler_capacity);
    self.reset_depth = 0;
    self.source_dir_count = 0;
    self.source_dir_capacity = 8;
    self.source_dirs = (char**)mem::malloc(uptr.sizeof * self.source_dir_capacity);
    if (self.source_dirs == null) self.source_dir_capacity = 0;
    self.raise_msg_len = 0;
    self.raise_msg[0] = 0;
    self.raise_payload = null;

    main::stack_pool_init(&self.stack_ctx_pool);
    g_stack_ctx_pool = &self.stack_ctx_pool;
    self.resume_value = null;
    self.yield_value = null;
    self.yield_scope = null;
    self.current_reset_state = null;
}

fn void interp_init_core_symbols(Interp* self) {
    self.sym_lambda = self.symbols.intern("lambda");
    self.sym_if = self.symbols.intern("if");
    self.sym_let = self.symbols.intern("let");
    self.sym_define = self.symbols.intern("define");
    self.sym_quote = self.symbols.intern("quote");
    self.sym_checkpoint = self.symbols.intern("checkpoint");
    self.sym_capture = self.symbols.intern("capture");
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
    self.sym_block = self.symbols.intern("block");
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
    self.sym_struct = self.symbols.intern("struct");
    self.sym_abstract = self.symbols.intern("abstract");
    self.sym_union = self.symbols.intern("union");
    self.sym_alias = self.symbols.intern("alias");
    self.sym_effect = self.symbols.intern("effect");

    self.sym_ffi = self.symbols.intern("ffi");
    self.sym_lib = self.symbols.intern("lib");
    self.sym_Pointer = self.symbols.intern("Pointer");
    self.sym_Ptr = self.symbols.intern("Ptr");
    self.sym_Void = self.symbols.intern("Void");

    self.sym_Integer = self.symbols.intern("Integer");
    self.sym_Int = self.symbols.intern("Int");
    self.sym_Double = self.symbols.intern("Double");
    self.sym_String = self.symbols.intern("String");
    self.sym_Symbol = self.symbols.intern("Symbol");
    self.sym_List = self.symbols.intern("List");
    self.sym_Boolean = self.symbols.intern("Boolean");
    self.sym_Bool = self.symbols.intern("Bool");
    self.sym_Nil = self.symbols.intern("Nil");
    self.sym_Closure = self.symbols.intern("Closure");
    self.sym_Array = self.symbols.intern("Array");
    self.sym_Dictionary = self.symbols.intern("Dictionary");
    self.sym_Dict = self.symbols.intern("Dict");
    self.sym_Set = self.symbols.intern("Set");
    self.sym_Any = self.symbols.intern("Any");
    self.sym_Value = self.symbols.intern("Value");
    self.sym_Number = self.symbols.intern("Number");
    self.sym_Collection = self.symbols.intern("Collection");
    self.sym_Iterator = self.symbols.intern("Iterator");
    self.sym_TimePoint = self.symbols.intern("TimePoint");
    self.sym_Coroutine = self.symbols.intern("Coroutine");
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
    self.sym_as = self.symbols.intern("as");
    self.sym_all = self.symbols.intern("all");
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
}

fn Value* interp_apply_stack_overflow_error(Interp* interp) {
    // Stack-overflow errors must survive deep unwind without per-frame
    // boundary copy/promotion churn; allocate in root scope so callers can
    // reuse the same value through scope-finalization boundaries.
    if (interp == null || interp.root_scope == null) return null;
    main::ScopeRegion* saved_scope = interp.current_scope;
    interp.current_scope = interp.root_scope;
    Value* err = make_error(interp, "stack overflow: insufficient stack headroom");
    interp.current_scope = saved_scope;
    return err;
}

fn Value* interp_apply_frame_enter(Interp* interp) @inline {
    interp.eval_depth++;
    if (!main::stack_has_apply_headroom(main::STACK_APPLY_MIN_HEADROOM)) {
        interp.eval_depth--;
        return interp_apply_stack_overflow_error(interp);
    }
    return null;
}

fn void interp_apply_frame_leave(Interp* interp) @inline {
    assert(interp.eval_depth > 0, "interp eval depth underflow");
    interp.eval_depth--;
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
    e.lifetime_kind = ENV_LIFETIME_LOCAL_SCOPE_FRAME;
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
    e.lifetime_kind = ENV_LIFETIME_LOCAL_SCOPE_FRAME;
    e.scope_gen = self.current_scope.escape_generation;
    return e;
}

fn Expr* Interp.alloc_expr(Interp* self) {
    Expr* expr = (Expr*)ast_arena_alloc(&self.ast_arena, Expr.sizeof);
    assert(expr != null, "alloc_expr: ast arena allocation failed");
    return expr;
}

fn Pattern* Interp.alloc_pattern(Interp* self) {
    Pattern* pattern = (Pattern*)ast_arena_alloc(&self.ast_arena, Pattern.sizeof);
    assert(pattern != null, "alloc_pattern: ast arena allocation failed");
    return pattern;
}

