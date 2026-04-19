module lisp;

import main;
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
 * Three forms: ^Integer (simple), ^(List Integer) (compound), ^{'T Number} (dict)
 */
struct ValueLiteralKey {
    ValueTag tag;         // INT/SYMBOL/STRING
    long int_value;       // INT payload
    SymbolId sym_value;   // SYMBOL payload or interned STRING payload
}

struct TypeAnnotation {
    bool has_annotation;
    bool is_compound;          // ^(List Integer) form
    bool is_dict;              // ^{...} form
    SymbolId base_type;        // Integer, List, Lambda, Value, etc.
    SymbolId* params;          // compound type params (heap-allocated)
    usz param_count;
    MetaEntry* meta;           // dict entries (heap-allocated)
    usz meta_count;
    ValueLiteralKey val_literal; // For ^(Value x) literal matching
    bool has_val_literal;      // true if this is a Value type with literal
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
 * TimePointKind — Kind of temporal value represented by TimePoint.
 */
enum TimePointKind : char {
    TIME_POINT_DATE,
    TIME_POINT_TIME,
    TIME_POINT_DATETIME,
    TIME_POINT_DATETIMETZ
}

/**
 * TimePoint — normalized timestamp components with kind.
 */
struct TimePoint {
    int year;
    int month;
    int day;
    int hour;
    int minute;
    int second;
    int microsecond;
    int tz_offset;     // minutes east of UTC, only valid for TIME_POINT_DATETIMETZ
    TimePointKind kind;
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
    SymbolId* param_type_vars; // length = param_count, non-zero when param is a type variable (e.g., ^T)
    ValueLiteralKey* val_literals; // length = param_count, for Value literal types
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

struct Complex128Val {
    double real;
    double imag;
}

struct Complex64Val {
    float real;
    float imag;
}

/**
 * Value — Tagged union for all Lisp values.
 */
struct Value {
    ValueTag tag;
    uint scope_gen;  // Scope generation stamp — set at allocation, used for O(1) scope checks
    bool continuation_boundary_owned; // True when this wrapper introduced boundary handle retention
    bool string_heap_owned; // True when STRING/ERROR chars are heap-owned and must be freed

    union {
        long          int_val;
        double        double_val;     // Floating-point value
        float         float32_val;    // 32-bit floating-point value
        void*         big_integer_val; // Boost.Multiprecision cpp_int handle
        void*         big_float_val;   // Boost.Multiprecision cpp_dec_float handle
        void*         big_complex_val; // Boost.Multiprecision complex handle
        Complex128Val complex128_val;  // Fixed-width two-Float64 complex value
        Complex64Val  complex64_val;   // Fixed-width two-Float32 complex value
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
        TimePoint     time_point_val;   // Parsed temporal value
        TensorVal*    tensor_val;       // Rank-polymorphic numeric aggregate
    }
}

