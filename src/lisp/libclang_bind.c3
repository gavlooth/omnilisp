module lisp;

import std::io;

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
    char[16] omni_type;   // Mapped: Int, Double, String, Int (for ptrs)
}

struct ParsedFunc {
    char[128] name;
    char[16] return_ffi_type;
    char[16] return_omni_type;
    ParsedParam[16] params;
    usz param_count;
    bool is_variadic;
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
    char*[4] paths = {
        "libclang.so",
        "libclang.so.1",
        "/usr/lib/libclang.so",
        "/usr/lib/llvm/lib/libclang.so"
    };

    h.dl_handle = null;
    for (usz i = 0; i < 4; i++) {
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

// --- Visitor state passed via client_data ---

struct VisitorState {
    LibclangHandle* h;
    ParsedFunc*     funcs;
    usz             count;
    usz             max;
    char[64]*       filter;     // Optional function name filter
    usz             filter_count;
    bool            use_filter;
}

/**
 * Map a CXType to FFI type string and Omni type annotation.
 */
fn void map_c_type(LibclangHandle* h, CXType ty, char* ffi_out, char* omni_out, usz buf_size) {
    // Get canonical type to resolve typedefs
    CXType canon = h.get_canonical_type(ty);
    int kind = canon.kind;

    if (kind == CXTYPE_VOID) {
        copy_str("'void", ffi_out, buf_size);
        copy_str("", omni_out, buf_size);
    } else if (kind == CXTYPE_INT || kind == CXTYPE_UINT ||
               kind == CXTYPE_LONG || kind == CXTYPE_ULONG ||
               kind == CXTYPE_ENUM) {
        copy_str("'int", ffi_out, buf_size);
        copy_str("Int", omni_out, buf_size);
    } else if (kind == CXTYPE_FLOAT || kind == CXTYPE_DOUBLE) {
        copy_str("'double", ffi_out, buf_size);
        copy_str("Double", omni_out, buf_size);
    } else if (kind == CXTYPE_POINTER) {
        // Check if char* / const char*
        CXString spelling = h.get_type_spelling(canon);
        char* cstr = h.get_cstring(spelling);
        bool is_char_ptr = false;
        if (cstr != null) {
            // Check for "char *" or "const char *"
            is_char_ptr = cstr_contains(cstr, "char");
        }
        h.dispose_string(spelling);

        if (is_char_ptr) {
            copy_str("'string", ffi_out, buf_size);
            copy_str("String", omni_out, buf_size);
        } else {
            copy_str("'ptr", ffi_out, buf_size);
            copy_str("Int", omni_out, buf_size);
        }
    } else {
        // Unknown type — treat as int (best effort)
        copy_str("'int", ffi_out, buf_size);
        copy_str("Int", omni_out, buf_size);
    }
}

/**
 * Visitor callback for clang_visitChildren.
 * Collects top-level function declarations.
 */
fn uint clang_visitor(CXCursor cursor, CXCursor parent, void* client_data) {
    VisitorState* state = (VisitorState*)client_data;
    LibclangHandle* h = state.h;

    // Only interested in function declarations
    if (cursor.kind != CXCURSOR_FUNCTION_DECL) return CXCHILDVISIT_CONTINUE;
    if (state.count >= state.max) return CXCHILDVISIT_BREAK;

    // Get function name
    CXString name_str = h.get_cursor_spelling(cursor);
    char* name_cstr = h.get_cstring(name_str);
    if (name_cstr == null) {
        h.dispose_string(name_str);
        return CXCHILDVISIT_CONTINUE;
    }

    // Apply filter if specified
    if (state.use_filter) {
        bool found = false;
        for (usz i = 0; i < state.filter_count; i++) {
            if (cstr_eq(name_cstr, &state.filter[i])) {
                found = true;
                break;
            }
        }
        if (!found) {
            h.dispose_string(name_str);
            return CXCHILDVISIT_CONTINUE;
        }
    }

    ParsedFunc* func = &state.funcs[state.count];

    // Copy name
    cstr_copy(name_cstr, &func.name, 127);
    h.dispose_string(name_str);

    // Get function type
    CXType fn_type = h.get_cursor_type(cursor);

    // Check if variadic
    func.is_variadic = h.is_fn_variadic(fn_type) != 0;

    // Return type
    CXType ret_type = h.get_result_type(fn_type);
    map_c_type(h, ret_type, &func.return_ffi_type, &func.return_omni_type, 16);

    // Parameters
    int num_args = h.get_num_arg_types(fn_type);
    if (num_args < 0) num_args = 0;
    if (num_args > 16) num_args = 16;
    func.param_count = (usz)num_args;

    for (int i = 0; i < num_args; i++) {
        ParsedParam* param = &func.params[i];

        // Get arg type
        CXType arg_type = h.get_arg_type(fn_type, (uint)i);
        map_c_type(h, arg_type, &param.ffi_type, &param.omni_type, 16);

        // Get type spelling for c_type field
        CXString type_str = h.get_type_spelling(arg_type);
        char* type_cstr = h.get_cstring(type_str);
        if (type_cstr != null) {
            cstr_copy(type_cstr, &param.c_type, 63);
        } else {
            param.c_type[0] = 0;
        }
        h.dispose_string(type_str);

        // Generate param name (arg0, arg1, ...)
        param.name[0] = 'a'; param.name[1] = 'r'; param.name[2] = 'g';
        if (i < 10) {
            param.name[3] = (char)('0' + i);
            param.name[4] = 0;
        } else {
            param.name[3] = (char)('0' + i / 10);
            param.name[4] = (char)('0' + i % 10);
            param.name[5] = 0;
        }
    }

    state.count++;
    return CXCHILDVISIT_CONTINUE;
}

/**
 * Parse a C header file, extracting function declarations.
 * Returns number of functions found.
 */
fn usz parse_header(LibclangHandle* h, char[] header_path,
                    ParsedFunc* out, usz max,
                    void* filter, usz filter_count, bool use_filter) {
    // Null-terminate header path
    char[512] path_buf;
    usz path_len = header_path.len;
    if (path_len > 511) path_len = 511;
    for (usz i = 0; i < path_len; i++) path_buf[i] = header_path[i];
    path_buf[path_len] = 0;

    // Create index
    void* index = h.create_index(0, 0);
    if (index == null) return 0;

    // Parse translation unit
    void* tu = h.parse_tu(index, &path_buf[0], null, 0, null, 0,
                           CXTRANSLATIONUNIT_SKIP_FUNCTION_BODIES);
    if (tu == null) {
        h.dispose_index(index);
        return 0;
    }

    // Set up visitor state
    VisitorState state;
    state.h = h;
    state.funcs = out;
    state.count = 0;
    state.max = max;
    state.filter = (char[64]*)filter;
    state.filter_count = filter_count;
    state.use_filter = use_filter;

    // Get root cursor and visit children
    CXCursor root = h.get_tu_cursor(tu);
    h.visit_children(root, &clang_visitor, (void*)&state);

    // Cleanup
    h.dispose_tu(tu);
    h.dispose_index(index);

    return state.count;
}

// --- String helpers ---

fn void copy_str(char[] src, void* dest_ptr, usz max_len) {
    char* dest = (char*)dest_ptr;
    usz len = src.len;
    if (len > max_len) len = max_len;
    for (usz i = 0; i < len; i++) dest[i] = src[i];
    dest[len] = 0;
}

fn void cstr_copy(char* src, void* dest_ptr, usz max_len) {
    char* dest = (char*)dest_ptr;
    usz i = 0;
    while (src[i] != 0 && i < max_len) {
        dest[i] = src[i];
        i++;
    }
    dest[i] = 0;
}

fn bool cstr_eq(char* a, void* b_ptr) {
    char* b = (char*)b_ptr;
    usz i = 0;
    while (a[i] != 0 && b[i] != 0) {
        if (a[i] != b[i]) return false;
        i++;
    }
    return a[i] == b[i]; // both null
}

fn bool cstr_contains(char* haystack, char[] needle) {
    usz i = 0;
    while (haystack[i] != 0) {
        bool match = true;
        for (usz j = 0; j < needle.len; j++) {
            if (haystack[i + j] == 0 || haystack[i + j] != needle[j]) {
                match = false;
                break;
            }
        }
        if (match) return true;
        i++;
    }
    return false;
}
