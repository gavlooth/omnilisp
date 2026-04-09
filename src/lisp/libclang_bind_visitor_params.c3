module lisp;

import std::core::mem;

fn bool visitor_matches_filter(VisitorState* state, char* name_cstr) {
    if (!state.use_filter) return true;
    for (usz i = 0; i < state.filter_count; i++) {
        if (cstr_eq(name_cstr, &state.filter[i])) return true;
    }
    return false;
}

fn void visitor_set_param_name(ParsedParam* param, int index) {
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

fn void visitor_copy_param_name(LibclangHandle* h, CXCursor fn_cursor, ParsedParam* param, int index) {
    if (h.get_arg_cursor == null) {
        visitor_set_param_name(param, index);
        return;
    }

    CXCursor arg_cursor = h.get_arg_cursor(fn_cursor, (uint)index);
    CXString name_str = h.get_cursor_spelling(arg_cursor);
    char* name_cstr = h.get_cstring(name_str);
    if (name_cstr != null && name_cstr[0] != 0) {
        cstr_copy(name_cstr, &param.name, param.name.len - 1);
        h.dispose_string(name_str);
        return;
    }

    h.dispose_string(name_str);
    visitor_set_param_name(param, index);
}

fn bool visitor_set_param_types(LibclangHandle* h, ParsedParam* param, CXType arg_type,
    char* unsupported_type_out, usz unsupported_type_cap) {
    bool mapped = map_c_type(h, arg_type, false, &param.ffi_type, &param.omni_type, 16,
        unsupported_type_out, unsupported_type_cap);

    CXString type_str = h.get_type_spelling(arg_type);
    char* type_cstr = h.get_cstring(type_str);
    if (type_cstr != null) {
        cstr_copy(type_cstr, &param.c_type, 63);
    } else {
        param.c_type[0] = 0;
    }
    h.dispose_string(type_str);
    bindgen_infer_metadata_for_type(
        &param.c_type,
        &param.omni_type,
        false,
        &param.ownership,
        &param.nullability,
        &param.role,
        &param.teardown
    );
    param.buffer_direction = BINDGEN_BUFFER_DIRECTION_NA;
    param.buffer_size_param_index = -1;
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

fn bool visitor_set_params(LibclangHandle* h, CXCursor fn_cursor, CXType fn_type, ParsedFunc* func, VisitorState* state) {
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
        visitor_copy_param_name(h, fn_cursor, param, i);
    }
    bindgen_finalize_param_metadata(func);
    return true;
}
