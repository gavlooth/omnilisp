module lisp;

// =============================================================================
// libclang Wrapper — runtime dlopen of libclang for C header parsing.
// Only loaded when --bind is invoked. Defines C3 structs matching libclang ABI
// so struct-returning functions work correctly (hidden pointer ABI on x86-64).
// =============================================================================

// --- libclang struct definitions (must match ABI exactly) ---

struct CXString {
    void* data;
    uint private_flags;
}

struct CXCursor {
    int kind;
    int xdata;
    void*[3] data;
}

struct CXType {
    int kind;
    void*[2] data;
}

// libclang CXCursorKind constants
const int CXCURSOR_FUNCTION_DECL = 8;

// libclang CXChildVisitResult
const int CXCHILDVISIT_CONTINUE = 1;
const int CXCHILDVISIT_RECURSE  = 2;
const int CXCHILDVISIT_BREAK    = 0;

// libclang CXTypeKind constants
const int CXTYPE_VOID      = 2;
const int CXTYPE_INT       = 17;
const int CXTYPE_UINT      = 18;
const int CXTYPE_LONG      = 19;
const int CXTYPE_ULONG     = 20;
const int CXTYPE_FLOAT     = 21;
const int CXTYPE_DOUBLE    = 22;
const int CXTYPE_POINTER   = 101;
const int CXTYPE_TYPEDEF   = 105;
const int CXTYPE_ELABORATED = 119;
// CXType_Enum and others we map to int
const int CXTYPE_ENUM      = 106;

// libclang parse flags
const uint CXTRANSLATIONUNIT_NONE = 0;
const uint CXTRANSLATIONUNIT_SKIP_FUNCTION_BODIES = 0x40;

// --- Function pointer aliases ---

alias ClangCreateIndexFn           = fn void*(int, int);
alias ClangDisposeIndexFn          = fn void(void*);
alias ClangParseTranslationUnitFn  = fn void*(void*, char*, char**, int, void*, uint, uint);
alias ClangDisposeTranslationUnitFn = fn void(void*);

// Struct-returning functions — C3 uses hidden pointer ABI on x86-64
alias ClangGetTranslationUnitCursorFn = fn CXCursor(void*);
alias ClangGetCursorTypeFn            = fn CXType(CXCursor);
alias ClangCursorGetArgumentFn        = fn CXCursor(CXCursor, uint);
alias ClangGetResultTypeFn            = fn CXType(CXType);
alias ClangGetArgTypeFn               = fn CXType(CXType, uint);
alias ClangGetCursorSpellingFn        = fn CXString(CXCursor);
alias ClangGetTypeSpellingFn          = fn CXString(CXType);
alias ClangGetCanonicalTypeFn         = fn CXType(CXType);

// Simple return types
alias ClangGetCStringFn               = fn char*(CXString);
alias ClangDisposeStringFn            = fn void(CXString);
alias ClangGetNumArgTypesFn           = fn int(CXType);
alias ClangIsFunctionTypeVariadicFn   = fn uint(CXType);

// Visitor callback type and visitChildren
alias CXCursorVisitor                 = fn uint(CXCursor, CXCursor, void*);
alias ClangVisitChildrenFn            = fn uint(CXCursor, CXCursorVisitor, void*);

// --- Parsed function representation ---

struct ParsedParam {
    char[64] name;
    char[64] c_type;      // Original C type string from clang
    char[16] ffi_type;    // Mapped: 'int, 'double, 'string, 'ptr, 'void
    char[16] omni_type;   // Mapped: Integer, Float64, String, ForeignHandle (for ptrs)
    BindgenOwnershipTag ownership;
    BindgenNullabilityTag nullability;
    BindgenRoleTag role;
    BindgenBufferDirectionTag buffer_direction;
    int buffer_size_param_index;
    BindgenTeardownTag teardown;
}

enum BindgenOwnershipTag : char {
    BINDGEN_OWNERSHIP_NONE,
    BINDGEN_OWNERSHIP_BORROWED,
    BINDGEN_OWNERSHIP_UNKNOWN,
}

enum BindgenNullabilityTag : char {
    BINDGEN_NULLABILITY_NA,
    BINDGEN_NULLABILITY_UNKNOWN,
}

enum BindgenRoleTag : char {
    BINDGEN_ROLE_VALUE,
    BINDGEN_ROLE_STRING_INPUT,
    BINDGEN_ROLE_STRING_BUFFER,
    BINDGEN_ROLE_STRING_RETURN,
    BINDGEN_ROLE_OPAQUE_POINTER,
    BINDGEN_ROLE_CALLBACK,
}

enum BindgenBufferDirectionTag : char {
    BINDGEN_BUFFER_DIRECTION_NA,
    BINDGEN_BUFFER_DIRECTION_IN,
    BINDGEN_BUFFER_DIRECTION_OUT,
    BINDGEN_BUFFER_DIRECTION_INOUT,
}

enum BindgenTeardownTag : char {
    BINDGEN_TEARDOWN_NONE,
    BINDGEN_TEARDOWN_MANUAL_REVIEW,
    BINDGEN_TEARDOWN_CALLBACK_UNREGISTER,
}

struct ParsedFunc {
    char[128] name;
    char[64] return_c_type;
    char[16] return_ffi_type;
    char[16] return_omni_type;
    ParsedParam* params;
    usz param_count;
    usz param_capacity;
    bool is_variadic;
    bool filtered_out;
    BindgenOwnershipTag return_ownership;
    BindgenNullabilityTag return_nullability;
    BindgenRoleTag return_role;
    BindgenTeardownTag return_teardown;
}

// --- LibclangHandle holds all function pointers ---

struct LibclangHandle {
    void* dl_handle;

    ClangCreateIndexFn           create_index;
    ClangDisposeIndexFn          dispose_index;
    ClangParseTranslationUnitFn  parse_tu;
    ClangDisposeTranslationUnitFn dispose_tu;
    ClangGetTranslationUnitCursorFn get_tu_cursor;
    ClangGetCursorTypeFn         get_cursor_type;
    ClangCursorGetArgumentFn     get_arg_cursor;
    ClangGetResultTypeFn         get_result_type;
    ClangGetArgTypeFn            get_arg_type;
    ClangGetNumArgTypesFn        get_num_arg_types;
    ClangGetCursorSpellingFn     get_cursor_spelling;
    ClangGetTypeSpellingFn       get_type_spelling;
    ClangGetCStringFn            get_cstring;
    ClangDisposeStringFn         dispose_string;
    ClangIsFunctionTypeVariadicFn is_fn_variadic;
    ClangVisitChildrenFn         visit_children;
    ClangGetCanonicalTypeFn      get_canonical_type;
}

/**
 * Initialize libclang via dlopen. Returns false if libclang not found.
 */
fn bool libclang_init(LibclangHandle* h) {
    // Try common libclang paths
    char*[10] paths = {
        "libclang.so",
        "libclang.so.1",
        "/usr/lib/libclang.so",
        "/usr/lib/libclang.so.1",
        "/usr/lib/llvm/lib/libclang.so",
        "/usr/lib/llvm/lib/libclang.so.1",
        "/usr/lib/llvm-18/lib/libclang.so",
        "/usr/lib/llvm-18/lib/libclang.so.1",
        "/usr/lib/llvm-17/lib/libclang.so",
        "/usr/lib/llvm-17/lib/libclang.so.1"
    };

    h.dl_handle = null;
    for (usz i = 0; i < 10; i++) {
        h.dl_handle = dlopen(paths[i], RTLD_LAZY);
        if (h.dl_handle != null) break;
    }
    if (h.dl_handle == null) return false;

    // Load all function pointers
    h.create_index    = (ClangCreateIndexFn)dlsym(h.dl_handle, "clang_createIndex");
    h.dispose_index   = (ClangDisposeIndexFn)dlsym(h.dl_handle, "clang_disposeIndex");
    h.parse_tu        = (ClangParseTranslationUnitFn)dlsym(h.dl_handle, "clang_parseTranslationUnit");
    h.dispose_tu      = (ClangDisposeTranslationUnitFn)dlsym(h.dl_handle, "clang_disposeTranslationUnit");
    h.get_tu_cursor   = (ClangGetTranslationUnitCursorFn)dlsym(h.dl_handle, "clang_getTranslationUnitCursor");
    h.get_cursor_type = (ClangGetCursorTypeFn)dlsym(h.dl_handle, "clang_getCursorType");
    h.get_arg_cursor  = (ClangCursorGetArgumentFn)dlsym(h.dl_handle, "clang_Cursor_getArgument");
    h.get_result_type = (ClangGetResultTypeFn)dlsym(h.dl_handle, "clang_getResultType");
    h.get_arg_type    = (ClangGetArgTypeFn)dlsym(h.dl_handle, "clang_getArgType");
    h.get_num_arg_types = (ClangGetNumArgTypesFn)dlsym(h.dl_handle, "clang_getNumArgTypes");
    h.get_cursor_spelling = (ClangGetCursorSpellingFn)dlsym(h.dl_handle, "clang_getCursorSpelling");
    h.get_type_spelling = (ClangGetTypeSpellingFn)dlsym(h.dl_handle, "clang_getTypeSpelling");
    h.get_cstring     = (ClangGetCStringFn)dlsym(h.dl_handle, "clang_getCString");
    h.dispose_string  = (ClangDisposeStringFn)dlsym(h.dl_handle, "clang_disposeString");
    h.is_fn_variadic  = (ClangIsFunctionTypeVariadicFn)dlsym(h.dl_handle, "clang_isFunctionTypeVariadic");
    h.visit_children  = (ClangVisitChildrenFn)dlsym(h.dl_handle, "clang_visitChildren");
    h.get_canonical_type = (ClangGetCanonicalTypeFn)dlsym(h.dl_handle, "clang_getCanonicalType");

    // Verify critical functions loaded
    if (h.create_index == null || h.parse_tu == null || h.visit_children == null) {
        dlclose(h.dl_handle);
        h.dl_handle = null;
        return false;
    }

    return true;
}

/**
 * Release libclang handle.
 */
fn void libclang_destroy(LibclangHandle* h) {
    if (h.dl_handle != null) {
        dlclose(h.dl_handle);
        h.dl_handle = null;
    }
}
