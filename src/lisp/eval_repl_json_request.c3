module lisp;

import std::core::mem;

struct JsonReplRequest {
    bool ok;
    bool has_id;
    bool has_input;
    bool program_mode;
    char[] id;
    char[] input;
    char[] error_message;
}

fn char[] json_repl_dup_string(char[] src) {
    char* out = (char*)mem::malloc(src.len + 1);
    if (out == null) return {};
    for (usz i = 0; i < src.len; i++) out[i] = src[i];
    out[src.len] = 0;
    return out[:src.len];
}

fn void json_repl_request_free(JsonReplRequest* request) {
    if (request == null) return;
    if (request.has_id && request.id.ptr != null) {
        mem::free(request.id.ptr);
        request.id = "";
        request.has_id = false;
    }
    if (request.has_input && request.input.ptr != null) {
        mem::free(request.input.ptr);
        request.input = "";
        request.has_input = false;
    }
}

fn bool json_repl_str_eq(char[] left, char[] right) @inline {
    if (left.len != right.len) return false;
    for (usz i = 0; i < left.len; i++) {
        if (left[i] != right[i]) return false;
    }
    return true;
}

fn bool json_repl_val_string(YyjsonVal* val, char[]* out) {
    if (omni_yyjson_is_str(val) == 0) return false;
    char* chars = omni_yyjson_get_str(val);
    usz len = omni_yyjson_get_len(val);
    *out = chars[:len];
    return true;
}

fn JsonReplRequest json_repl_parse_request(char[] line) {
    JsonReplRequest request = {
        .ok = false,
        .has_id = false,
        .has_input = false,
        .program_mode = false,
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

    YyjsonVal* id_val = omni_yyjson_obj_getn(root, "id".ptr, 2);
    if (id_val == null) {
        request.error_message = "request field 'id' is required";
        return request;
    }
    char[] parsed_id = "";
    if (!json_repl_val_string(id_val, &parsed_id)) {
        request.error_message = "request field 'id' must be a string";
        return request;
    }
    request.id = json_repl_dup_string(parsed_id);
    if (request.id.ptr == null) {
        request.error_message = "out of memory while copying request field 'id'";
        return request;
    }
    request.has_id = true;

    YyjsonVal* input_val = omni_yyjson_obj_getn(root, "input".ptr, 5);
    if (input_val == null) {
        request.error_message = "request field 'input' is required";
        return request;
    }
    char[] parsed_input = "";
    if (!json_repl_val_string(input_val, &parsed_input)) {
        request.error_message = "request field 'input' must be a string";
        return request;
    }
    request.input = json_repl_dup_string(parsed_input);
    if (request.input.ptr == null) {
        request.error_message = "out of memory while copying request field 'input'";
        return request;
    }
    request.has_input = true;

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
