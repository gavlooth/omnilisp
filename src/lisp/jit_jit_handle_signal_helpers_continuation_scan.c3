module lisp;

fn void continuation_retain_handle_state_for_boundary(Continuation* k) {
    if (k == null || k.handle_state == null || k.handle_retained) return;
    HandleEffectState* hstate = (HandleEffectState*)k.handle_state;
    hstate.continuation_refcount++;
    k.handle_retained = true;
}

const usz CONTINUATION_SCAN_MAX_VALUES = 1024;
const usz CONTINUATION_SCAN_MAX_ENVS = 256;

struct ContinuationEscapeScan {
    Continuation* target;
    Value*[CONTINUATION_SCAN_MAX_VALUES] value_stack;
    usz value_stack_len;
    Value*[CONTINUATION_SCAN_MAX_VALUES] value_seen;
    usz value_seen_len;
    Env*[CONTINUATION_SCAN_MAX_ENVS] env_stack;
    usz env_stack_len;
    Env*[CONTINUATION_SCAN_MAX_ENVS] env_seen;
    usz env_seen_len;
}

fn bool continuation_scan_push_value(ContinuationEscapeScan* scan, Value* v) {
    if (scan == null || v == null) return false;
    for (usz i = 0; i < scan.value_seen_len; i++) {
        if (scan.value_seen[i] == v) return false;
    }
    if (scan.value_seen_len >= CONTINUATION_SCAN_MAX_VALUES) return true;
    scan.value_seen[scan.value_seen_len] = v;
    scan.value_seen_len++;
    if (scan.value_stack_len >= CONTINUATION_SCAN_MAX_VALUES) return true;
    scan.value_stack[scan.value_stack_len] = v;
    scan.value_stack_len++;
    return false;
}

fn bool continuation_scan_push_env(ContinuationEscapeScan* scan, Env* env) {
    if (scan == null || env == null) return false;
    for (usz i = 0; i < scan.env_seen_len; i++) {
        if (scan.env_seen[i] == env) return false;
    }
    if (scan.env_seen_len >= CONTINUATION_SCAN_MAX_ENVS) return true;
    scan.env_seen[scan.env_seen_len] = env;
    scan.env_seen_len++;
    if (scan.env_stack_len >= CONTINUATION_SCAN_MAX_ENVS) return true;
    scan.env_stack[scan.env_stack_len] = env;
    scan.env_stack_len++;
    return false;
}

fn bool continuation_result_references_target(Value* root, Continuation* target) {
    if (root == null || target == null) return false;

    ContinuationEscapeScan scan = {};
    scan.target = target;
    if (continuation_scan_push_value(&scan, root)) return true;

    while (scan.value_stack_len > 0 || scan.env_stack_len > 0) {
        while (scan.value_stack_len > 0) {
            scan.value_stack_len--;
            Value* v = scan.value_stack[scan.value_stack_len];
            if (v == null) continue;
            if (v.tag == CONTINUATION && v.cont_val == scan.target) {
                return true;
            }

            switch (v.tag) {
                case CONS:
                    if (continuation_scan_push_value(&scan, v.cons_val.car)) return true;
                    if (continuation_scan_push_value(&scan, v.cons_val.cdr)) return true;
                case PARTIAL_PRIM:
                    if (continuation_scan_push_value(&scan, v.partial_val.first_arg)) return true;
                    if (continuation_scan_push_value(&scan, v.partial_val.second_arg)) return true;
                case ARRAY:
                    if (v.array_val != null && v.array_val.items != null) {
                        for (usz i = 0; i < v.array_val.length; i++) {
                            if (continuation_scan_push_value(&scan, v.array_val.items[i])) return true;
                        }
                    }
                case HASHMAP:
                case SET:
                    if (v.hashmap_val != null && v.hashmap_val.entries != null) {
                        for (usz i = 0; i < v.hashmap_val.capacity; i++) {
                            HashEntry* entry = &v.hashmap_val.entries[i];
                            if (entry.key != null) {
                                if (continuation_scan_push_value(&scan, entry.key)) return true;
                                if (continuation_scan_push_value(&scan, entry.value)) return true;
                            }
                        }
                    }
                case METHOD_TABLE:
                    if (v.method_table_val != null) {
                        if (continuation_scan_push_value(&scan, v.method_table_val.fallback)) return true;
                        if (v.method_table_val.entries != null) {
                            for (usz i = 0; i < v.method_table_val.entry_count; i++) {
                                if (continuation_scan_push_value(&scan, v.method_table_val.entries[i].implementation)) return true;
                            }
                        }
                    }
                case CLOSURE:
                    if (v.closure_val != null) {
                        if (continuation_scan_push_env(&scan, v.closure_val.env)) return true;
                    }
                case MODULE:
                    if (v.module_val != null) {
                        if (continuation_scan_push_env(&scan, v.module_val.env)) return true;
                    }
                case ITERATOR:
                    if (continuation_scan_push_value(&scan, v.iterator_val)) return true;
                case INSTANCE:
                    if (v.instance_val != null && v.instance_val.fields != null) {
                        for (usz i = 0; i < v.instance_val.field_count; i++) {
                            Value* field_ptr = &v.instance_val.fields[i];
                            if (continuation_scan_push_value(&scan, field_ptr)) return true;
                        }
                    }
                default:
                    break;
            }
        }

        while (scan.env_stack_len > 0) {
            scan.env_stack_len--;
            Env* env = scan.env_stack[scan.env_stack_len];
            if (env == null) continue;
            if (continuation_scan_push_env(&scan, env.parent)) return true;
            for (usz i = 0; i < env.binding_count; i++) {
                if (continuation_scan_push_value(&scan, env.bindings[i].value)) return true;
            }
        }
    }

    return false;
}

fn void continuation_retain_if_referenced_by_result(Value* result, Continuation* k) {
    if (k == null || result == null) return;
    if (continuation_result_references_target(result, k)) {
        continuation_retain_handle_state_for_boundary(k);
    }
}
