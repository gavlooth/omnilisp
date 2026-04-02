module lisp;

import std::core::mem;

// --- Visitor state passed via client_data ---

bool g_bind_force_param_alloc_fail = false;

struct VisitorState {
    LibclangHandle* h;
    ParsedFunc*     funcs;
    usz             count;
    usz             max;
    char[64]*       filter;     // Optional function name filter
    usz             filter_count;
    bool            use_filter;
    bool            output_limit_hit;
    bool            param_allocation_failed;
    bool            unsupported_type;
    char[64]        unsupported_type_name;
}

struct ParseHeaderResult {
    usz count;
    bool path_too_long;
    bool output_limit_hit;
    bool param_allocation_failed;
    bool unsupported_type;
    bool parse_failed;
    char[64] unsupported_type_name;
}

/**
 * Map a CXType to FFI type string and Omni type annotation.
 */
fn void set_unsupported_c_type_name(LibclangHandle* h, CXType ty, char* out_buf, usz out_cap) {
    if (out_buf == null || out_cap == 0) return;
    out_buf[0] = 0;

    CXType canon = h.get_canonical_type(ty);
    CXString spelling = h.get_type_spelling(canon);
    char* cstr = h.get_cstring(spelling);
    if (cstr != null && cstr[0] != 0) {
        cstr_copy(cstr, out_buf, out_cap - 1);
    } else {
        copy_str("unknown", out_buf, out_cap - 1);
    }
    h.dispose_string(spelling);
}

fn bool map_c_type(LibclangHandle* h, CXType ty, char* ffi_out, char* omni_out, usz buf_size,
    char* unsupported_type_out = null, usz unsupported_type_cap = 0) {
    // Get canonical type to resolve typedefs
    CXType canon = h.get_canonical_type(ty);
    int kind = canon.kind;

    if (kind == CXTYPE_VOID) {
        copy_str("'void", ffi_out, buf_size);
        copy_str("Void", omni_out, buf_size);
        return true;
    }
    if (kind == CXTYPE_INT || kind == CXTYPE_UINT ||
        kind == CXTYPE_LONG || kind == CXTYPE_ULONG ||
        kind == CXTYPE_ENUM) {
        copy_str("'int", ffi_out, buf_size);
        copy_str("Integer", omni_out, buf_size);
        return true;
    }
    if (kind == CXTYPE_FLOAT || kind == CXTYPE_DOUBLE) {
        copy_str("'double", ffi_out, buf_size);
        copy_str("Double", omni_out, buf_size);
        return true;
    }
    if (kind == CXTYPE_POINTER) {
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
            copy_str("Pointer", omni_out, buf_size);
        }
        return true;
    }

    set_unsupported_c_type_name(h, ty, unsupported_type_out, unsupported_type_cap);
    return false;
}

fn bool visitor_matches_filter(VisitorState* state, char* name_cstr) {
    if (!state.use_filter) return true;
    for (usz i = 0; i < state.filter_count; i++) {
        if (cstr_eq(name_cstr, &state.filter[i])) return true;
    }
    return false;
}

fn void visitor_set_param_name(ParsedParam* param, int index) {
    // Generate param name (arg0, arg1, ...)
    param.name[0] = 'a'; param.name[1] = 'r'; param.name[2] = 'g';
    if (index < 10) {
        param.name[3] = (char)('0' + index);
        param.name[4] = 0;
        return;
    }
    param.name[3] = (char)('0' + index / 10);
    param.name[4] = (char)('0' + index % 10);
    param.name[5] = 0;
}

fn bool visitor_set_param_types(LibclangHandle* h, ParsedParam* param, CXType arg_type,
    char* unsupported_type_out, usz unsupported_type_cap) {
    bool mapped = map_c_type(h, arg_type, &param.ffi_type, &param.omni_type, 16,
        unsupported_type_out, unsupported_type_cap);

    // Get type spelling for c_type field
    CXString type_str = h.get_type_spelling(arg_type);
    char* type_cstr = h.get_cstring(type_str);
    if (type_cstr != null) {
        cstr_copy(type_cstr, &param.c_type, 63);
    } else {
        param.c_type[0] = 0;
    }
    h.dispose_string(type_str);
    return mapped;
}

fn bool parsed_func_ensure_param_capacity(ParsedFunc* func, usz needed) {
    if (needed <= func.param_capacity) return true;
    if (g_bind_force_param_alloc_fail) return false;
    usz new_cap = func.param_capacity > 0 ? func.param_capacity : 8;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }
    ParsedParam* new_params = (ParsedParam*)mem::realloc(func.params, ParsedParam.sizeof * new_cap);
    if (new_params == null) return false;
    func.params = new_params;
    func.param_capacity = new_cap;
    return true;
}

fn bool visitor_set_params(LibclangHandle* h, CXType fn_type, ParsedFunc* func, VisitorState* state) {
    // Parameters
    int num_args = h.get_num_arg_types(fn_type);
    if (num_args < 0) num_args = 0;
    if (!parsed_func_ensure_param_capacity(func, (usz)num_args)) {
        state.param_allocation_failed = true;
        func.param_count = 0;
        return false;
    }
    func.param_count = (usz)num_args;

    for (int i = 0; i < num_args; i++) {
        ParsedParam* param = &func.params[i];
        CXType arg_type = h.get_arg_type(fn_type, (uint)i);
        if (!visitor_set_param_types(h, param, arg_type, &state.unsupported_type_name, state.unsupported_type_name.len)) {
            state.unsupported_type = true;
            func.param_count = 0;
            return false;
        }
        visitor_set_param_name(param, i);
    }
    return true;
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
    if (state.count >= state.max) {
        state.output_limit_hit = true;
        return CXCHILDVISIT_BREAK;
    }

    // Get function name
    CXString name_str = h.get_cursor_spelling(cursor);
    char* name_cstr = h.get_cstring(name_str);
    if (name_cstr == null) {
        h.dispose_string(name_str);
        return CXCHILDVISIT_CONTINUE;
    }

    if (!visitor_matches_filter(state, name_cstr)) {
        h.dispose_string(name_str);
        return CXCHILDVISIT_CONTINUE;
    }

    ParsedFunc* func = &state.funcs[state.count];
    if (func.params != null) {
        mem::free(func.params);
        func.params = null;
    }
    func.param_count = 0;
    func.param_capacity = 0;

    // Copy name
    cstr_copy(name_cstr, &func.name, 127);
    h.dispose_string(name_str);

    // Get function type
    CXType fn_type = h.get_cursor_type(cursor);

    // Check if variadic
    func.is_variadic = h.is_fn_variadic(fn_type) != 0;

    // Return type
    CXType ret_type = h.get_result_type(fn_type);
    if (!map_c_type(h, ret_type, &func.return_ffi_type, &func.return_omni_type, 16,
        &state.unsupported_type_name, state.unsupported_type_name.len)) {
        state.unsupported_type = true;
        return CXCHILDVISIT_BREAK;
    }

    if (!visitor_set_params(h, fn_type, func, state)) {
        if (func.params != null) {
            mem::free(func.params);
            func.params = null;
        }
        func.param_count = 0;
        func.param_capacity = 0;
        return CXCHILDVISIT_BREAK;
    }

    state.count++;
    return CXCHILDVISIT_CONTINUE;
}

/**
 * Parse a C header file, extracting function declarations.
 * Returns number of functions found.
 */
fn ParseHeaderResult parse_header_checked(LibclangHandle* h, char[] header_path,
    ParsedFunc* out, usz max,
    void* filter, usz filter_count, bool use_filter) {
    ParseHeaderResult result = {};

    // Null-terminate header path
    char[512] path_buf;
    usz path_len = header_path.len;
    if (path_len > 511) {
        result.path_too_long = true;
        return result;
    }
    for (usz i = 0; i < path_len; i++) path_buf[i] = header_path[i];
    path_buf[path_len] = 0;

    // Create index
    void* index = h.create_index(0, 0);
    if (index == null) {
        result.parse_failed = true;
        return result;
    }

    // Parse translation unit
    void* tu = h.parse_tu(index, &path_buf[0], null, 0, null, 0,
        CXTRANSLATIONUNIT_SKIP_FUNCTION_BODIES);
    if (tu == null) {
        h.dispose_index(index);
        result.parse_failed = true;
        return result;
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
    state.output_limit_hit = false;
    state.param_allocation_failed = false;
    state.unsupported_type = false;
    state.unsupported_type_name[0] = 0;

    // Get root cursor and visit children
    CXCursor root = h.get_tu_cursor(tu);
    h.visit_children(root, &clang_visitor, (void*)&state);

    // Cleanup
    h.dispose_tu(tu);
    h.dispose_index(index);

    result.count = state.count;
    result.output_limit_hit = state.output_limit_hit;
    result.param_allocation_failed = state.param_allocation_failed;
    result.unsupported_type = state.unsupported_type;
    cstr_copy(&state.unsupported_type_name, &result.unsupported_type_name, result.unsupported_type_name.len - 1);
    return result;
}

fn usz parse_header(LibclangHandle* h, char[] header_path,
                    ParsedFunc* out, usz max,
                    void* filter, usz filter_count, bool use_filter) {
    return parse_header_checked(h, header_path, out, max, filter, filter_count, use_filter).count;
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
