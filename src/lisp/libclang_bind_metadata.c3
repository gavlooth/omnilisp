module lisp;

fn bool bindgen_type_is_callback(char* c_type) {
    return cstr_contains(c_type, "(*");
}

fn bool bindgen_type_is_const_qualified(char* c_type) {
    return cstr_contains(c_type, "const");
}

fn bool bindgen_type_is_char_pointer(char* c_type) {
    return cstr_contains(c_type, "char");
}

fn void bindgen_set_scalar_metadata(BindgenOwnershipTag* ownership, BindgenNullabilityTag* nullability,
    BindgenRoleTag* role, BindgenTeardownTag* teardown) {
    *ownership = BINDGEN_OWNERSHIP_NONE;
    *nullability = BINDGEN_NULLABILITY_NA;
    *role = BINDGEN_ROLE_VALUE;
    *teardown = BINDGEN_TEARDOWN_NONE;
}

fn void bindgen_infer_metadata_for_type(char* c_type, char* omni_type, bool is_return,
    BindgenOwnershipTag* ownership, BindgenNullabilityTag* nullability,
    BindgenRoleTag* role, BindgenTeardownTag* teardown) {
    bindgen_set_scalar_metadata(ownership, nullability, role, teardown);

    if (!is_return && bindgen_type_is_char_pointer(c_type) && !bindgen_type_is_const_qualified(c_type)) {
        *ownership = BINDGEN_OWNERSHIP_BORROWED;
        *nullability = BINDGEN_NULLABILITY_UNKNOWN;
        *role = BINDGEN_ROLE_STRING_BUFFER;
        *teardown = BINDGEN_TEARDOWN_MANUAL_REVIEW;
        return;
    }

    if (cstr_eq(omni_type, "ForeignHandle")) {
        *ownership = is_return ? BINDGEN_OWNERSHIP_UNKNOWN : BINDGEN_OWNERSHIP_BORROWED;
        *nullability = BINDGEN_NULLABILITY_UNKNOWN;
        *role = bindgen_type_is_callback(c_type) ? BINDGEN_ROLE_CALLBACK : BINDGEN_ROLE_OPAQUE_POINTER;
        *teardown = bindgen_type_is_callback(c_type)
            ? BINDGEN_TEARDOWN_CALLBACK_UNREGISTER
            : (is_return ? BINDGEN_TEARDOWN_MANUAL_REVIEW : BINDGEN_TEARDOWN_NONE);
        return;
    }

    if (cstr_eq(omni_type, "String")) {
        *nullability = BINDGEN_NULLABILITY_UNKNOWN;
        if (is_return) {
            *ownership = bindgen_type_is_const_qualified(c_type) ? BINDGEN_OWNERSHIP_BORROWED : BINDGEN_OWNERSHIP_UNKNOWN;
            *role = BINDGEN_ROLE_STRING_RETURN;
            *teardown = BINDGEN_TEARDOWN_MANUAL_REVIEW;
            return;
        }

        *ownership = BINDGEN_OWNERSHIP_BORROWED;
        *role = bindgen_type_is_const_qualified(c_type) ? BINDGEN_ROLE_STRING_INPUT : BINDGEN_ROLE_STRING_BUFFER;
        *teardown = *role == BINDGEN_ROLE_STRING_BUFFER ? BINDGEN_TEARDOWN_MANUAL_REVIEW : BINDGEN_TEARDOWN_NONE;
    }
}

fn void bindgen_set_return_metadata(ParsedFunc* func) {
    bindgen_infer_metadata_for_type(
        &func.return_c_type,
        &func.return_omni_type,
        true,
        &func.return_ownership,
        &func.return_nullability,
        &func.return_role,
        &func.return_teardown
    );
}

fn bool cstr_contains_cstr(char* haystack, char* needle) {
    if (needle == null || needle[0] == 0) return false;
    usz i = 0;
    while (haystack[i] != 0) {
        usz j = 0;
        while (needle[j] != 0 && haystack[i + j] != 0 && haystack[i + j] == needle[j]) j++;
        if (needle[j] == 0) return true;
        i++;
    }
    return false;
}

fn bool bindgen_param_is_integer_size_candidate(ParsedParam* p) {
    return cstr_eq(&p.omni_type, "Integer");
}

fn bool bindgen_param_name_has_size_hint(char* name) {
    return cstr_contains(name, "len") ||
        cstr_contains(name, "size") ||
        cstr_contains(name, "count") ||
        cstr_contains(name, "capacity") ||
        cstr_contains(name, "cap") ||
        cstr_contains(name, "bytes");
}

fn BindgenBufferDirectionTag bindgen_infer_string_buffer_direction(char* name) {
    if (cstr_contains(name, "out") || cstr_contains(name, "output") || cstr_contains(name, "dst")) {
        return BINDGEN_BUFFER_DIRECTION_OUT;
    }
    if (cstr_contains(name, "inout")) {
        return BINDGEN_BUFFER_DIRECTION_INOUT;
    }
    if (cstr_contains(name, "input") || cstr_contains(name, "src")) {
        return BINDGEN_BUFFER_DIRECTION_IN;
    }
    return BINDGEN_BUFFER_DIRECTION_INOUT;
}

fn int bindgen_find_string_buffer_size_param_index(ParsedFunc* func, int buffer_index) {
    if (func == null || buffer_index < 0 || (usz)buffer_index >= func.param_count) return -1;
    char* buffer_name = &func.params[(usz)buffer_index].name;
    int best_index = -1;
    int best_score = -1;
    for (usz i = 0; i < func.param_count; i++) {
        if ((int)i == buffer_index) continue;
        ParsedParam* candidate = &func.params[i];
        if (!bindgen_param_is_integer_size_candidate(candidate)) continue;
        if (!bindgen_param_name_has_size_hint(&candidate.name)) continue;

        int score = 0;
        if (cstr_contains_cstr(&candidate.name, buffer_name)) score += 10;
        if (cstr_contains(&candidate.name, "size")) score += 4;
        if (cstr_contains(&candidate.name, "len")) score += 4;
        if (cstr_contains(&candidate.name, "capacity")) score += 3;
        if (cstr_contains(&candidate.name, "count")) score += 2;

        int distance = (int)i - buffer_index;
        if (distance < 0) distance = -distance;
        if (distance <= 3) score += 4 - distance;

        if (score > best_score) {
            best_score = score;
            best_index = (int)i;
        }
    }
    return best_index;
}

fn void bindgen_finalize_param_metadata(ParsedFunc* func) {
    if (func == null || func.param_count == 0) return;
    for (usz i = 0; i < func.param_count; i++) {
        ParsedParam* p = &func.params[i];
        p.buffer_direction = BINDGEN_BUFFER_DIRECTION_NA;
        p.buffer_size_param_index = -1;
        if (p.role != BINDGEN_ROLE_STRING_BUFFER) continue;
        p.buffer_direction = bindgen_infer_string_buffer_direction(&p.name);
        p.buffer_size_param_index = bindgen_find_string_buffer_size_param_index(func, (int)i);
    }
}
