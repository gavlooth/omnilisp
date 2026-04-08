module lisp;

import std::core::mem;

// --- Visitor state passed via client_data ---

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
    if (!map_c_type(h, ret_type, true, &func.return_ffi_type, &func.return_omni_type, 16,
        &state.unsupported_type_name, state.unsupported_type_name.len)) {
        state.unsupported_type = true;
        return CXCHILDVISIT_BREAK;
    }
    CXString ret_type_str = h.get_type_spelling(ret_type);
    char* ret_type_cstr = h.get_cstring(ret_type_str);
    if (ret_type_cstr != null) {
        cstr_copy(ret_type_cstr, &func.return_c_type, 63);
    } else {
        func.return_c_type[0] = 0;
    }
    h.dispose_string(ret_type_str);
    bindgen_set_return_metadata(func);

    if (!visitor_set_params(h, cursor, fn_type, func, state)) {
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
