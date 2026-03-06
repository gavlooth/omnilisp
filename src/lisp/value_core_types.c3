module lisp;

import std::collections::list;
import std::io;
import main;
// ============================================================
// Fault types — structured error categories for C3 optionals
// Accessed as lisp::FAULT_NAME (e.g., lisp::UNBOUND_VARIABLE)
// ============================================================

faultdef
    // Parse errors
    UNTERMINATED_STRING,
    UNEXPECTED_TOKEN,
    NESTING_TOO_DEEP,
    UNKNOWN_ATTRIBUTE,
    INVALID_SYNTAX,
    // Eval errors
    UNBOUND_VARIABLE,
    TYPE_MISMATCH,
    ARITY_MISMATCH,
    DIVISION_BY_ZERO,
    STACK_OVERFLOW,
    NOT_A_FUNCTION,
    INDEX_OUT_OF_BOUNDS,
    UNHANDLED_EFFECT,
    // I/O errors
    CONNECTION_REFUSED,
    DNS_FAILED,
    TLS_HANDSHAKE_FAILED,
    READ_FAILED,
    WRITE_FAILED,
    // Type errors
    EXPECTED_STRING,
    EXPECTED_INT,
    EXPECTED_LIST,
    EXPECTED_ARRAY,
    EXPECTED_DICT,
    EXPECTED_CLOSURE,
    EXPECTED_SYMBOL;

// Compile-time debug flag: set to false for release builds (zero-cost elimination).
const bool DEBUG_BUILD = true;

// =============================================================================
// SECTION 1: VALUE TYPES
// =============================================================================

/**
 * ValueTag — Type tag for Lisp values.
 */
enum ValueTag : char {
    NIL,
    INT,
    STRING,         // String value
    SYMBOL,
    CONS,
    CLOSURE,
    CONTINUATION,
    PRIMITIVE,
    PARTIAL_PRIM,   // Partially applied primitive (curried)
    ERROR,          // Error value (carries error message in str_val)
    DOUBLE,         // Floating-point number
    HASHMAP,        // Hash map (mutable dictionary)
    FFI_HANDLE,     // FFI library handle (dlopen result)
    ARRAY,          // Mutable array (dynamic array)
    TYPE_INFO,      // Type descriptor value
    INSTANCE,       // User-defined type instance
    METHOD_TABLE,   // Dispatch table for multiple dispatch
    MODULE,         // First-class module reference
    ITERATOR,       // Lazy iterator (backed by closure thunk)
    COROUTINE,          // User-facing coroutine (wraps StackCtx*)
}

/**
 * Symbol — Interned string identifier.
 */
typedef SymbolId = uint;  // Already distinct (non-inline typedef = no implicit conversion)

// Initial capacities for dynamic tables
const usz MACRO_INITIAL_CAPACITY = 32;
const usz MODULE_INITIAL_CAPACITY = 16;
const usz HANDLER_INITIAL_CAPACITY = 16;
// Invalid symbol ID sentinel (returned on exhaustion; must not alias any valid index)
const SymbolId INVALID_SYMBOL_ID = (SymbolId)0xFFFFFFFF;

// Initial capacities for dynamic symbol table
const usz SYMBOL_INITIAL_CAPACITY = 256;
const usz SYMBOL_HASH_INITIAL_CAPACITY = 512;

// Type system constants
typedef TypeId = uint;
const TypeId INVALID_TYPE_ID = (TypeId)0xFFFFFFFF;
const usz TYPE_INITIAL_CAPACITY = 64;
const usz MAX_TYPE_PARAMS = 32;
const usz MAX_META_ENTRIES = 16;
const usz MAX_TYPE_FIELDS = 64;
/**
 * SymbolEntry — Storage for a single interned symbol name.
 */
struct SymbolEntry {
    char* name;   // heap-allocated, null-terminated
    usz len;
}

/**
 * SymbolTable — Table for interning symbol names (dynamic, growable).
 */
struct SymbolTable {
    SymbolEntry* entries;
    usz count;
    usz capacity;           // entries array capacity
    SymbolId* hash_index;   // Maps hash slot → symbol index (INVALID_SYMBOL_ID = empty)
    usz hash_capacity;      // hash_index array capacity (power of 2)
}

/**
 * Cons — Pair/list cell.
 */
struct Cons {
    Value* car;
    Value* cdr;
}

/**
 * Closure — Lambda with captured environment.
 * Supports variadic lambdas: (lambda (x y .. rest) body)
 */
struct Closure {
    uint      scope_gen;      // Scope generation stamp for O(1) membership check
    main::ScopeRegion* env_scope; // Standalone scope holding env copy (null if root-scope closure)
    SymbolId  param;          // Parameter name (first/only param, or unused if param_count==0)
    SymbolId* params;         // All fixed params (including first), bump-allocated
    usz       param_count;    // Number of fixed params
    bool      has_rest;       // True if variadic (.. rest)
    SymbolId  rest_param;     // Rest parameter name (if has_rest)
    Expr*     body;           // Body expression
    Env*      env;            // Captured environment
    bool      has_param;      // false for zero-arg lambdas: (lambda () body)
    bool      has_typed_params;  // true if any param has type annotations
    MethodSignature* type_sig;   // bump-allocated type signature (null if no types)
    SymbolId  name;           // Bound name (set by define), 0 if anonymous
}

/**
 * Continuation — Delimited continuation backed by a coroutine (StackCtx).
 *
 * The ctx field points to a suspended StackCtx. Multi-shot continuations
 * clone the context on each invocation.
 */
struct Continuation {
    StackCtx* ctx;            // Suspended stack context backing this continuation
    bool  is_coroutine_based; // Always true (kept for resolve dispatch)
    void* handle_state;   // HandleEffectState* (for resolve to reinstall handler)
}

/**
 * Primitive — Built-in function.
 */
alias PrimitiveFn = fn Value*(Value*[] args, Env* env, Interp* interp);

struct Primitive {
    char[64] name;
    PrimitiveFn func;
    int arity;           // -1 for variadic
    int tag;             // Extra data (e.g., TypeId for constructors)
    void* user_data;     // Optional data pointer for closure-like primitives
}

/**
 * PartialPrim — Partially applied primitive (curried).
 */
struct PartialPrim {
    PrimitiveFn func;
    Value*      first_arg;   // First argument stored
    Value*      second_arg;  // Second argument (for 3+ arity primitives)
    int         remaining;   // Arguments still needed
}

/**
 * StringVal — String value storage (heap-allocated, dynamic).
 */
struct StringVal {
    char* chars;    // heap-allocated, null-terminated
    usz len;
    usz capacity;   // allocated size
}

/**
 * HashEntry — Single key-value pair in a hash map.
 */
struct HashEntry {
    Value* key;     // null = empty slot
    Value* value;
}

/**
 * HashMap — Open-addressing hash map with linear probing.
 */
struct HashMap {
    HashEntry* entries;  // contiguous array (region-allocated)
    uint capacity;       // power of 2
    uint count;
    uint mask;           // capacity - 1
}

/**
 * MacroClause — Single clause in a pattern-based macro definition.
 * Pattern matches against args, template is a Value* datum for substitution.
 */
struct MacroClause {
    Pattern* pattern;    // Pattern to match macro args against
    Value*   tmpl;       // Template datum (Value*) for expansion
}

/**
 * CapturedBinding — A definition-time binding captured for macro hygiene.
 * Stores a snapshot of a symbol's value at macro definition time.
 */
struct CapturedBinding {
    SymbolId sym;
    Value*   value;
}

/**
 * MacroDef — Stored macro definition (pattern-based).
 * (define [macro] name (pattern1 template1) (pattern2 template2) ...)
 * captured_bindings stores a SNAPSHOT of template literal values at definition time,
 * preventing expansion-site shadowing from capturing them.
 */
struct MacroDef {
    SymbolId name;
    MacroClause* clauses;  // heap-allocated
    usz clause_count;
    CapturedBinding* captured_bindings;  // heap-allocated snapshot of definition-time values
    usz captured_count;
}

alias FfiHandleFinalizer = fn void(void* handle);

/**
 * FfiHandle — Foreign library handle from dlopen().
 */
struct FfiHandle {
    void*    lib_handle;     // dlopen() result
    char[256] lib_name;      // for display/debugging
    usz      name_len;
    uint     refcount;       // Shared ownership count for handle wrappers
    FfiHandleFinalizer finalizer;  // Optional payload cleanup callback
    bool     free_lib_handle;         // Free lib_handle after finalizer when true
}

/**
 * FastPathEntry — Maps an effect tag to its default (unhandled) primitive.
 * Used by jit_signal_impl to dispatch effects when no handler is installed.
 */
struct FastPathEntry {
    SymbolId tag;        // effect tag (e.g., sym for "io/print")
    Value*   prim;       // raw primitive (e.g., __raw-print)
}

/**
 * Array — Mutable dynamic array. Heap-allocated like HashMap.
 */
struct Array {
    Value** items;     // malloc'd array of Value pointers
    usz length;
    usz capacity;
}

// =============================================================================
// SECTION 1.5: TYPE SYSTEM TYPES
// =============================================================================

/**
 * MetaEntry — Single key-value pair in a metadata dictionary annotation.
 * Keys are quoted symbols, values are either symbol (type bound) or integer.
 */
struct MetaEntry {
    SymbolId key;          // quoted symbol key ('T, 'with, etc.)
    SymbolId sym_value;    // symbol value (Number, Comparable)
    long int_value;        // integer value (arity: 1, 2)
    bool is_int;           // true if value is integer, false if symbol
}

/**
 * TypeAnnotation — Parsed ^Type annotation on a parameter.
 * Three forms: ^Int (simple), ^(List Int) (compound), ^{'T Number} (dict)
 */
struct TypeAnnotation {
    bool has_annotation;
    bool is_compound;          // ^(List Int) form
    bool is_dict;              // ^{...} form
    SymbolId base_type;        // Int, List, Lambda, Val, etc.
    SymbolId* params;          // compound type params (heap-allocated)
    usz param_count;
    MetaEntry* meta;           // dict entries (heap-allocated)
    usz meta_count;
    long val_literal;          // For ^(Val 42) — the literal value
    bool has_val_literal;      // true if this is a Val type with literal
}

/**
 * TypeKind — Kind of user-defined type.
 */
enum TypeKind : char { TK_CONCRETE, TK_ABSTRACT, TK_UNION, TK_ALIAS, TK_BUILTIN, TK_EFFECT }

/**
 * TypeFieldInfo — Field descriptor in a concrete type.
 */
struct TypeFieldInfo { SymbolId name; TypeId field_type; SymbolId annotation_sym; }

/**
 * UnionVariant — Single variant in a union type.
 */
struct UnionVariant {
    SymbolId name;
    SymbolId* fields;          // heap-allocated
    usz field_count;
}

/**
 * TypeInfo — Complete type descriptor stored in the registry.
 */
struct TypeInfo {
    SymbolId name;
    TypeKind kind;
    TypeId parent;           // INVALID_TYPE_ID if no parent
    TypeFieldInfo* fields;   // heap-allocated
    usz field_count;
    SymbolId* type_params;   // heap-allocated
    usz type_param_count;
    MethodConstraint* constraints;  // heap-allocated
    usz constraint_count;
    UnionVariant* variants;  // heap-allocated, for TK_UNION
    usz variant_count;
    TypeId alias_target;     // for TK_ALIAS
}

/**
 * TypeRegistry — Registry of all types (builtin + user-defined).
 */
struct TypeRegistry {
    TypeInfo* types;
    usz type_count;
    usz capacity;
    TypeId* type_hash_index;
    usz hash_capacity;
}

/**
 * Instance — User-defined type instance (scope-owned object).
 *
 * OWNERSHIP RULE:
 * - Each Instance is allocated in a dedicated owner_scope.
 * - Instance field payloads are copied/promoted into owner_scope.
 * - Value wrappers that reference this Instance retain/release owner_scope.
 *
 * This keeps ownership in the scope/region model (no per-type RC drift).
 */
struct Instance {
    main::ScopeRegion* owner_scope;
    TypeId type_id;
    Value* fields;          // Value structs BY VALUE in owner_scope storage
    usz field_count;
    TypeId* type_args;      // owner-scope storage
    usz type_arg_count;
}

/**
 * MethodConstraint — Type bound on a type parameter (e.g., T <: Number).
 */
struct MethodConstraint {
    SymbolId param_sym;   // The type parameter symbol (e.g., 'T)
    TypeId bound_type;    // The upper bound TypeId (e.g., Number)
}

/**
 * MethodSignature — Type signature for dispatch matching.
 */
struct MethodSignature {
    TypeId* param_types;     // length = param_count, INVALID_TYPE_ID = Any
    long* val_literals;      // length = param_count, for Val types
    bool* has_val_literal;   // length = param_count
    usz param_count;
    MethodConstraint* constraints;  // heap/scope-allocated, length = constraint_count
    usz constraint_count;
}

/**
 * MethodEntry — Single dispatch entry in a method table.
 */
struct MethodEntry {
    MethodSignature sig;
    Value* implementation;  // Closure or Primitive
}

/**
 * MethodTable — Dispatch table for multiple dispatch (heap-allocated).
 */
const usz METHOD_INITIAL_CAPACITY = 8;

struct MethodTable {
    SymbolId name;
    MethodEntry* entries;
    usz entry_count;
    usz capacity;
    Value* fallback;  // untyped catch-all
}

/**
 * Value — Tagged union for all Lisp values.
 */
struct Value {
    ValueTag tag;
    uint scope_gen;  // Scope generation stamp — set at allocation, used for O(1) scope checks

    union {
        long          int_val;
        double        double_val;     // Floating-point value
        struct {
            char* str_chars;
            usz   str_len;
        }
        SymbolId      sym_val;
        Cons          cons_val;
        Closure*      closure_val;    // Closure (heap-allocated)
        Continuation* cont_val;      // From our continuation system
        Primitive*    prim_val;       // Primitive (heap-allocated)
        PartialPrim   partial_val;   // Curried primitive
        HashMap*      hashmap_val;   // Hash map
        FfiHandle*    ffi_val;       // FFI library handle (heap-allocated)
        Array*        array_val;     // Mutable array
        TypeInfo*     type_info_val; // Type descriptor
        Instance*     instance_val;  // User-defined type instance
        MethodTable*  method_table_val; // Dispatch table
        Module*       module_val;       // First-class module
        Value*        iterator_val;     // Iterator thunk closure (V_CLOSURE Value*)
        main::StackCtx*   coroutine_val;        // User-facing coroutine (StackCtx*)
    }
}

