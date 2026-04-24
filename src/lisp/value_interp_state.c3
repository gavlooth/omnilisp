module lisp;

import std::collections::list;
import std::thread;
import std::atomic::types;
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
    bool jit_enabled           : 0;  // true after init, enables JIT-first execution
    bool jit_tco_bounce        : 1;  // True = return sentinel, jit_eval loop continues
    bool raise_pending         : 2;  // raise_error found handler, pending dispatch
    bool console_to_stderr     : 3;  // reserve stdout for structured transports
}

struct InterpInputState {
    thread::Mutex mu;
    thread::ConditionVariable cv;
    bool mu_ready;
    bool cv_ready;
    char* buf;
    usz len;
    usz cap;
    bool closed;
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

    // Structured REPL server output capture buffer. When active, console
    // primitives append here instead of writing raw process stdio.
    bool console_capture_active;
    char* console_capture_buf;
    usz console_capture_len;
    usz console_capture_cap;
    // Optional structured input source used by the REPL server stdin routing.
    // When null, input primitives read directly from process stdin.
    InterpInputState* input_state;
    int sigint_requested;
    types::Atomic{int}* interrupt_flag;
    void* signal_handles_head;

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
    SymbolId sym_ForeignHandle; // "ForeignHandle" canonical FFI handle annotation type
    SymbolId sym_Void;      // "Void" for ^Void type and runtime Void value/type

    // Built-in type name symbols
    SymbolId sym_Integer;
    SymbolId sym_BigInteger;
    SymbolId sym_BigFloat;
    SymbolId sym_BigComplex;
    SymbolId sym_Complex128;
    SymbolId sym_Complex64;
    SymbolId sym_Float;
    SymbolId sym_Float32;
    SymbolId sym_Double;
    SymbolId sym_String;
    SymbolId sym_Symbol;
    SymbolId sym_List;
    SymbolId sym_Boolean;
    SymbolId sym_Nil;
    SymbolId sym_Closure;
    SymbolId sym_Array;
    SymbolId sym_Dictionary;
    SymbolId sym_Set;
    SymbolId sym_Any;
    SymbolId sym_Value;
    SymbolId sym_Number;
    SymbolId sym_Collection;
    SymbolId sym_Iterator;
    SymbolId sym_TimePoint;
    SymbolId sym_Coroutine;
    SymbolId sym_Tensor;
    SymbolId sym_Kernel;

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
    TypeId tid_BigInteger;
    TypeId tid_BigFloat;
    TypeId tid_BigComplex;
    TypeId tid_Complex128;
    TypeId tid_Complex64;
    TypeId tid_Double;
    TypeId tid_Float32;
    TypeId tid_String;
    TypeId tid_Symbol;
    TypeId tid_List;
    TypeId tid_Boolean;
    TypeId tid_Nil;
    TypeId tid_Void;
    TypeId tid_Closure;
    TypeId tid_Array;
    TypeId tid_Dictionary;
    TypeId tid_Set;
    TypeId tid_Any;
    TypeId tid_Number;
    TypeId tid_Collection;
    TypeId tid_Iterator;
    TypeId tid_TimePoint;
    TypeId tid_Coroutine;
    TypeId tid_Tensor;
    TypeId tid_Kernel;

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
    StableEscapeStore* stable_escape_store; // Interpreter-owned stable escape registry

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
    Continuation* continuation_head;   // Intrusive list of root-owned continuation objects
    Value* resume_value;           // Value passed to (k val) — read by capture on resume
    Value* yield_value;            // Value passed by (yield val) — read by resume
    main::ScopeRegion* yield_scope; // Scope owning yield_value while coroutine is suspended
    void* current_reset_state;     // Pointer to current ResetState (in jit.c3)

}

// ABI/interop guard: interpreter state is allocated and inspected by entry,
// JIT, and runtime bridge code. Update bindings and this assertion together.
$assert(Interp.sizeof == 2296);

fn void Interp.init(Interp* self) {
    runtime_attach_interp(self);
    interp_init_runtime_state(self);
    assert(interp_init_core_symbols(self), "interp core symbol init failed");
    assert(interp_init_type_symbols(self), "interp type symbol init failed");
    assert(interp_init_misc_symbols(self), "interp misc symbol init failed");
    self.types.init();
    interp_init_module_state(self);
    interp_init_runtime_flags(self);
    interp_init_macro_state(self);

    self.global_env = make_env(self, null);
}

