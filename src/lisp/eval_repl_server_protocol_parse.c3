module lisp;

fn bool repl_server_parse_required_string_field(YyjsonVal* root, char[] field_name, usz field_name_len, char[] missing_error, char[] type_error, char[] oom_error, char[]* out, bool* has_out, ReplServerRequest* request) {
    YyjsonVal* field_val = omni_yyjson_obj_getn(root, field_name.ptr, field_name_len);
    if (field_val == null) {
        request.error_message = missing_error;
        return false;
    }

    char[] parsed = "";
    if (!json_repl_val_string(field_val, &parsed)) {
        request.error_message = type_error;
        return false;
    }

    char[] copied = json_repl_dup_string(parsed);
    if (copied.ptr == null) {
        request.error_message = oom_error;
        return false;
    }
    *out = copied;
    *has_out = true;
    return true;
}

fn bool repl_server_parse_optional_string_field(YyjsonVal* root, char[] field_name, usz field_name_len, char[] type_error, char[] oom_error, char[]* out, bool* has_out, ReplServerRequest* request) {
    YyjsonVal* field_val = omni_yyjson_obj_getn(root, field_name.ptr, field_name_len);
    if (field_val == null) return true;

    char[] parsed = "";
    if (!json_repl_val_string(field_val, &parsed)) {
        request.error_message = type_error;
        return false;
    }

    char[] copied = json_repl_dup_string(parsed);
    if (copied.ptr == null) {
        request.error_message = oom_error;
        return false;
    }
    *out = copied;
    *has_out = true;
    return true;
}

fn ReplServerRequest repl_server_parse_request(char[] line) {
    ReplServerRequest request = {
        .ok = false,
        .error_message = "invalid request",
    };

    YyjsonDoc* doc = omni_yyjson_read(line.ptr, line.len, 0);
    if (doc == null) {
        request.error_message = "invalid JSON request";
        return request;
    }
    defer omni_yyjson_doc_free(doc);

    YyjsonVal* root = omni_yyjson_doc_get_root(doc);
    if (root == null || omni_yyjson_is_obj(root) == 0) {
        request.error_message = "request must be a JSON object";
        return request;
    }

    if (!repl_server_parse_required_string_field(root, "id", 2, "request field 'id' is required", "request field 'id' must be a string", "out of memory while copying request field 'id'", &request.id, &request.has_id, &request)) {
        return request;
    }
    if (!repl_server_parse_required_string_field(root, "op", 2, "request field 'op' is required", "request field 'op' must be a string", "out of memory while copying request field 'op'", &request.op, &request.has_op, &request)) {
        return request;
    }

    if (!repl_server_parse_optional_string_field(root, "auth", 4, "request field 'auth' must be a string", "out of memory while copying request field 'auth'", &request.auth, &request.has_auth, &request)) {
        return request;
    }
    if (!repl_server_parse_optional_string_field(root, "session", 7, "request field 'session' must be a string", "out of memory while copying request field 'session'", &request.session, &request.has_session, &request)) {
        return request;
    }
    if (!repl_server_parse_optional_string_field(root, "target", 6, "request field 'target' must be a string", "out of memory while copying request field 'target'", &request.target, &request.has_target, &request)) {
        return request;
    }
    if (!repl_server_parse_optional_string_field(root, "code", 4, "request field 'code' must be a string", "out of memory while copying request field 'code'", &request.code, &request.has_code, &request)) {
        return request;
    }
    if (!repl_server_parse_optional_string_field(root, "path", 4, "request field 'path' must be a string", "out of memory while copying request field 'path'", &request.path, &request.has_path, &request)) {
        return request;
    }
    if (!repl_server_parse_optional_string_field(root, "prefix", 6, "request field 'prefix' must be a string", "out of memory while copying request field 'prefix'", &request.prefix, &request.has_prefix, &request)) {
        return request;
    }
    if (!repl_server_parse_optional_string_field(root, "data", 4, "request field 'data' must be a string", "out of memory while copying request field 'data'", &request.data, &request.has_data, &request)) {
        return request;
    }

    YyjsonVal* eof_val = omni_yyjson_obj_getn(root, "eof".ptr, 3);
    if (eof_val != null) {
        if (omni_yyjson_is_true(eof_val) == 0 && omni_yyjson_is_false(eof_val) == 0) {
            request.error_message = "request field 'eof' must be a boolean";
            return request;
        }
        request.eof = omni_yyjson_get_bool(eof_val) != 0;
        request.has_eof = true;
    }

    YyjsonVal* mode_val = omni_yyjson_obj_getn(root, "mode".ptr, 4);
    if (mode_val != null) {
        char[] mode = "";
        if (!json_repl_val_string(mode_val, &mode)) {
            request.error_message = "request field 'mode' must be a string";
            return request;
        }
        if (json_repl_str_eq(mode, "expr")) {
            request.program_mode = false;
        } else if (json_repl_str_eq(mode, "program")) {
            request.program_mode = true;
        } else {
            request.error_message = "request field 'mode' must be 'expr' or 'program'";
            return request;
        }
    }

    request.ok = true;
    return request;
}
