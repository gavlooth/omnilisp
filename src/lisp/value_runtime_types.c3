module lisp;

import main;

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

// ABI/interop guard: symbol table snapshots are inspected by runtime helpers.
// Update helper bindings and this assertion together when the layout changes.
$assert(SymbolTable.sizeof == 40);

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

// ABI/interop guard: closure payloads are copied across scope/JIT boundaries.
// Update helper bindings and this assertion together when the layout changes.
$assert(Closure.sizeof == 88);

/**
 * Continuation — Delimited continuation backed by a coroutine (StackCtx).
 *
 * The ctx field points to a suspended StackCtx. Multi-shot continuations
 * clone the context on each invocation.
 */
struct Continuation {
    StackCtx* ctx;            // Suspended stack context backing this continuation
    Interp* owner_interp;     // Owning interpreter for boundary ownership checks
    bool  is_coroutine_based; // Always true (kept for resolve dispatch)
    void* handle_state;   // HandleEffectState* (for resolve to reinstall handler)
    bool  handle_retained; // True once boundary retention increments handle_state refcount
    Continuation* next_owned; // Intrusive list for interpreter teardown invalidation
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

// ABI/interop guard: primitive payloads are wrapped by runtime helper code.
// Update helper bindings and this assertion together when the layout changes.
$assert(Primitive.sizeof == 88);

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
    bool failed;
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

// ABI/interop guard: dictionary payloads cross value-copy and helper boundaries.
// Update helper bindings and this assertion together when the layout changes.
$assert(HashMap.sizeof == 24);

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
 * (define [macro] name (syntax-match (pattern1 (template ...)) (pattern2 (template ...)) ...))
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
    bool     free_lib_handle;         // Free payload when no finalizer owns release
    FfiHandleOwnership ownership;     // Declared handle ownership contract
    ForeignRuntimeKind runtime_kind;  // Common foreign runtime descriptor.
    ForeignHandleKind  handle_kind;   // Library/module/callable/object/buffer/opaque.
    uint     capabilities;            // FOREIGN_CAP_* bitset.
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

// ABI/interop guard: array payloads cross value-copy and helper boundaries.
// Update helper bindings and this assertion together when the layout changes.
$assert(Array.sizeof == 24);

/**
 * TensorDType — Native storage dtype for Tensor payloads.
 */
enum TensorDType : char {
    TENSOR_DTYPE_DOUBLE,
    TENSOR_DTYPE_FLOAT32,
    TENSOR_DTYPE_COMPLEX128,
    TENSOR_DTYPE_COMPLEX64,
    TENSOR_DTYPE_BIG_INTEGER,
    TENSOR_DTYPE_BIG_FLOAT,
    TENSOR_DTYPE_BIG_COMPLEX,
}

/**
 * TensorPayloadKind — Concrete storage or lazy expression node.
 */
enum TensorPayloadKind : char {
    TENSOR_PAYLOAD_CONCRETE,
    TENSOR_PAYLOAD_MAP,
    TENSOR_PAYLOAD_CONTRACT,
    TENSOR_PAYLOAD_VIEW,
}

/**
 * TensorDeviceKind — Physical placement for Tensor storage.
 */
enum TensorDeviceKind : char {
    TENSOR_DEVICE_CPU,
    TENSOR_DEVICE_CUDA,
    TENSOR_DEVICE_VULKAN,
}

alias TensorDeviceFinalizer = fn void(void* handle);

/**
 * TensorVal — Rank-polymorphic numeric aggregate payload.
 *
 * Concrete tensor payloads own native numeric storage. Lazy tensor-expression
 * payloads keep the same dtype/shape metadata but hold Omni Value edges that
 * must be copied/promoted with the surrounding region boundary.
 */
struct TensorVal {
    TensorPayloadKind kind;
    TensorDeviceKind device;
    TensorDType dtype;
    usz rank;
    usz* shape;
    usz* strides;
    usz element_count;
    usz byte_len;
    usz storage_offset;
    usz storage_element_count;
    usz storage_byte_len;
    void* alias_origin;
    void* data;
    void* device_handle;
    TensorDeviceFinalizer device_finalizer;
    bool mutable;
    bool is_view;
    Value* map_func;
    Value* map_left;
    Value* map_right;
    double map_left_scalar;
    double map_right_scalar;
    void* map_left_big_integer_scalar;
    void* map_right_big_integer_scalar;
    void* map_left_big_float_scalar;
    void* map_right_big_float_scalar;
    void* map_left_big_complex_scalar;
    void* map_right_big_complex_scalar;
    Complex128Val map_left_complex128_scalar;
    Complex128Val map_right_complex128_scalar;
    Complex64Val map_left_complex64_scalar;
    Complex64Val map_right_complex64_scalar;
    bool map_is_binary;
    Value* contract_left;
    Value* contract_right;
    usz* contract_left_axes;
    usz* contract_right_axes;
    usz contract_axis_count;
    Value* view_source;
}

// ABI/interop guard: tensor payloads are shared with backend helper contracts.
// Update backend bindings and this assertion together when the layout changes.
$assert(TensorVal.sizeof == 304);
