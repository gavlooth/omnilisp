module lisp;

fn char[] repl_server_completion_kind(Value* value) {
    if (value == null) return "binding";
    switch (value.tag) {
        case PRIMITIVE:
        case CLOSURE:
        case PARTIAL_PRIM:
        case METHOD_TABLE:
            return "function";
        case MODULE:
            return "module";
        default:
            return "binding";
    }
}

fn bool repl_server_completion_name_less(char[] left, char[] right) {
    usz limit = left.len < right.len ? left.len : right.len;
    for (usz i = 0; i < limit; i++) {
        if (left[i] < right[i]) return true;
        if (left[i] > right[i]) return false;
    }
    return left.len < right.len;
}

fn bool repl_server_send_interrupted_event(ReplServerOutput* output, char[] id, char[] session) {
    return repl_server_send_simple_event(output, id, session, "interrupted");
}

fn bool repl_server_send_complete_event(ReplServerOutput* output, char[] id, char[] session, Env* env, SymbolTable* symbols, char[] prefix) {
    YyjsonMutDoc* doc = omni_yyjson_mut_doc_new();
    if (doc == null) return false;
    defer omni_yyjson_mut_doc_free(doc);

    YyjsonMutVal* root = omni_yyjson_mut_obj(doc);
    omni_yyjson_mut_doc_set_root(doc, root);
    repl_server_obj_add_string(doc, root, "id", id);
    repl_server_obj_add_string(doc, root, "session", session);
    repl_server_obj_add_string(doc, root, "event", "complete");

    YyjsonMutVal* items = omni_yyjson_mut_arr(doc);
    if (env != null && env.binding_count > 0) {
        usz* matches = (usz*)mem::malloc(usz.sizeof * env.binding_count);
        if (matches == null) return false;
        defer mem::free(matches);

        usz match_count = 0;
        for (usz i = 0; i < env.binding_count; i++) {
            char[] name = symbols.get_name(env.bindings[i].name);
            if (!repl_completion_matches_prefix(name, prefix.ptr, prefix.len)) continue;
            matches[match_count] = i;
            match_count++;
        }

        for (usz i = 1; i < match_count; i++) {
            usz current = matches[i];
            char[] current_name = symbols.get_name(env.bindings[current].name);
            usz j = i;
            while (j > 0) {
                char[] prev_name = symbols.get_name(env.bindings[matches[j - 1]].name);
                if (!repl_server_completion_name_less(current_name, prev_name)) break;
                matches[j] = matches[j - 1];
                j--;
            }
            matches[j] = current;
        }

        for (usz i = 0; i < match_count; i++) {
            Binding binding = env.bindings[matches[i]];
            char[] name = symbols.get_name(binding.name);
            YyjsonMutVal* item = omni_yyjson_mut_obj(doc);
            repl_server_obj_add_string(doc, item, "candidate", name);
            repl_server_obj_add_string(doc, item, "kind", repl_server_completion_kind(binding.value));
            omni_yyjson_mut_arr_append(items, item);
        }
    }

    repl_server_obj_add_val(doc, root, "items", items);
    return repl_server_output_write_json_doc(output, doc);
}

fn bool repl_server_send_describe(ReplServerOutput* output, char[] id, char[] transport_name) {
    YyjsonMutDoc* doc = omni_yyjson_mut_doc_new();
    if (doc == null) return false;
    defer omni_yyjson_mut_doc_free(doc);

    YyjsonMutVal* root = omni_yyjson_mut_obj(doc);
    omni_yyjson_mut_doc_set_root(doc, root);
    repl_server_obj_add_string(doc, root, "id", id);
    repl_server_obj_add_string(doc, root, "event", "describe");
    repl_server_obj_add_string(doc, root, "protocol", "omni-repl/1");

    YyjsonMutVal* server = omni_yyjson_mut_obj(doc);
    repl_server_obj_add_string(doc, server, "implementation", "omni");
    repl_server_obj_add_string(doc, server, "version", "0.2.0");
    repl_server_obj_add_val(doc, root, "server", server);

    YyjsonMutVal* transports = omni_yyjson_mut_arr(doc);
    omni_yyjson_mut_arr_append(transports, omni_yyjson_mut_strn(doc, transport_name.ptr, transport_name.len));
    repl_server_obj_add_val(doc, root, "transports", transports);

    YyjsonMutVal* ops = omni_yyjson_mut_obj(doc);
    repl_server_obj_add_val(doc, ops, "clone", omni_yyjson_mut_obj(doc));
    repl_server_obj_add_val(doc, ops, "close", omni_yyjson_mut_obj(doc));
    repl_server_obj_add_val(doc, ops, "describe", omni_yyjson_mut_obj(doc));
    repl_server_obj_add_val(doc, ops, "complete", omni_yyjson_mut_obj(doc));
    repl_server_obj_add_val(doc, ops, "interrupt", omni_yyjson_mut_obj(doc));
    repl_server_obj_add_val(doc, ops, "stdin", omni_yyjson_mut_obj(doc));
    YyjsonMutVal* eval = omni_yyjson_mut_obj(doc);
    YyjsonMutVal* modes = omni_yyjson_mut_arr(doc);
    omni_yyjson_mut_arr_append(modes, omni_yyjson_mut_strn(doc, "expr".ptr, 4));
    omni_yyjson_mut_arr_append(modes, omni_yyjson_mut_strn(doc, "program".ptr, 7));
    repl_server_obj_add_val(doc, eval, "modes", modes);
    repl_server_obj_add_val(doc, ops, "eval", eval);
    repl_server_obj_add_val(doc, ops, "load-file", omni_yyjson_mut_obj(doc));
    repl_server_obj_add_val(doc, root, "ops", ops);

    return repl_server_output_write_json_doc(output, doc);
}

fn void repl_server_output_init(ReplServerOutput* output, int write_fd, bool socket_write) {
    *output = {};
    output.write_fd = write_fd;
    output.socket_write = socket_write;
    if (catch err = output.write_mu.init()) {
        output.write_mu_ready = false;
    } else {
        output.write_mu_ready = true;
    }
}

fn void repl_server_output_destroy(ReplServerOutput* output) {
    if (output == null) return;
    *output = {};
}
