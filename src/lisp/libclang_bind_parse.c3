module lisp;

bool g_bind_force_param_alloc_fail = false;

/**
 * Parse a C header file, extracting function declarations.
 * Returns number of functions found.
 */
fn ParseHeaderResult parse_header_checked(LibclangHandle* h, char[] header_path,
    ParsedFunc* out, usz max,
    void* filter, usz filter_count, bool use_filter,
    void* exclude_filter, usz exclude_filter_count, bool use_exclude_filter) {
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
    state.exclude_filter = (char[64]*)exclude_filter;
    state.exclude_filter_count = exclude_filter_count;
    state.use_exclude_filter = use_exclude_filter;
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
    return parse_header_checked(h, header_path, out, max, filter, filter_count, use_filter, null, 0, false).count;
}
