module lisp;

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
    BINDING_WRITE_FAILED,
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
const bool DEBUG_BUILD = false;

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
    BIG_INTEGER,    // Arbitrary-precision exact integer
    HASHMAP,        // Hash map (mutable dictionary)
    SET,            // Hash map payload with set semantics
    FFI_HANDLE,     // FFI library handle (dlopen result)
    ARRAY,          // Mutable array (dynamic array)
    TYPE_INFO,      // Type descriptor value
    INSTANCE,       // User-defined type instance
    METHOD_TABLE,   // Dispatch table for multiple dispatch
    MODULE,         // First-class module reference
    ITERATOR,       // Lazy iterator (backed by closure thunk)
    COROUTINE,          // User-facing coroutine (wraps StackCtx*)
    TIME_POINT,         // Temporal value (date/time/datetime/timestamp)
    TENSOR,             // Rank-polymorphic numeric aggregate
    VOID,               // Singleton no-result value
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
