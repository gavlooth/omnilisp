module lisp;

import std::io;
import main;

const usz STABLE_ESCAPE_CAPACITY_WIDE_CHILDREN = 257;
const usz STABLE_ESCAPE_CAPACITY_DICT_ENTRIES = 129;

fn bool stable_escape_capacity_prepare_matches(
    Interp* interp,
    Value* root,
    ValueTag root_tag,
    usz expected_node_count,
    usz expected_child_count
) {
    if (interp == null || root == null || root.tag != root_tag || expected_child_count == 0) return false;
    StableEscapeHandle handle = {};
    bool prepared = stable_escape_store_prepare_value(interp, root, &handle);
    usz node_count = prepared ? stable_escape_store_prepared_node_count(interp, &handle) : 0;
    usz root_index = prepared ? stable_escape_store_prepared_root_index(interp, &handle) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    usz child_count = prepared ? stable_escape_store_prepared_node_child_count(interp, &handle, root_index) : 0;
    usz first_child = prepared ? stable_escape_store_prepared_node_child(interp, &handle, root_index, 0) : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    usz last_child = prepared
        ? stable_escape_store_prepared_node_child(interp, &handle, root_index, (uint)(expected_child_count - 1))
        : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    usz beyond_child = prepared
        ? stable_escape_store_prepared_node_child(interp, &handle, root_index, (uint)expected_child_count)
        : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    StableGraphPassport passport = {};
    bool passport_ok = prepared && stable_escape_store_passport_snapshot(interp, &handle, &passport);
    bool shape_ok = prepared &&
                    passport_ok &&
                    node_count == expected_node_count &&
                    passport.prepared_node_count == expected_node_count &&
                    passport.prepared_edge_count == expected_child_count &&
                    root_index != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                    stable_escape_store_prepared_node_tag(interp, &handle, root_index) == root_tag &&
                    child_count == expected_child_count &&
                    first_child != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                    last_child != STABLE_ESCAPE_PREPARED_INVALID_INDEX &&
                    beyond_child == STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    bool retire_ok = prepared && stable_escape_store_retire(interp, &handle);
    return shape_ok && retire_ok;
}

fn Value* stable_escape_capacity_make_array_root(Interp* interp, usz count) {
    Value* arr = make_array_checked(interp, count, "test: stable escape capacity array allocation failed");
    if (arr == null || arr.tag != ARRAY || arr.array_val == null) return null;
    for (usz i = 0; i < count; i++) {
        arr.array_val.items[i] = make_int(interp, (long)(10000 + i));
        if (arr.array_val.items[i] == null) return null;
        arr.array_val.length++;
    }
    return arr;
}

fn Value* stable_escape_capacity_make_dict_root(Interp* interp, usz count) {
    Value* map = make_hashmap_checked(interp, 512, "test: stable escape capacity dict allocation failed");
    if (map == null || map.tag != HASHMAP || map.hashmap_val == null) return null;
    for (usz i = 0; i < count; i++) {
        Value* key = make_int(interp, (long)(20000 + i));
        Value* val = make_int(interp, (long)(30000 + i));
        if (key == null || val == null || !hashmap_set_checked(map.hashmap_val, key, val, interp)) return null;
    }
    return map;
}

fn Value* stable_escape_capacity_make_set_root(Interp* interp, usz count) {
    Value* set = make_set_checked(interp, 512, "test: stable escape capacity set allocation failed");
    Value* true_val = interp != null ? make_symbol(interp, interp.sym_true) : null;
    if (set == null || set.tag != SET || set.hashmap_val == null || true_val == null) return null;
    for (usz i = 0; i < count; i++) {
        Value* member = make_int(interp, (long)(40000 + i));
        if (member == null || !hashmap_set_checked(set.hashmap_val, member, true_val, interp)) return null;
    }
    return set;
}

fn Value* stable_escape_capacity_make_closure_root(Interp* interp, usz count) {
    if (interp == null) return null;
    Env* captured_env = make_env(interp, interp.global_env);
    if (captured_env == null) return null;
    for (usz i = 0; i < count; i++) {
        char[64] name_buf;
        if (try rendered = io::bprintf(&name_buf, "stable-cap-env-%d", (long)i)) {
            SymbolId name = interp.symbols.intern(rendered);
            if (name == INVALID_SYMBOL_ID ||
                !env_define_with_barrier_checked(captured_env, name, make_int(interp, (long)(50000 + i)), interp)) {
                return null;
            }
        } else {
            return null;
        }
    }
    return make_closure_no_param(interp, null, captured_env);
}

fn void run_memory_lifetime_stable_escape_capacity_tests(Interp* interp, int* pass, int* fail) {
    main::ScopeRegion* saved_scope = interp != null ? interp.current_scope : null;
    bool setup_ok = interp != null && interp.root_scope != null;
    if (!setup_ok) {
        io::printn("[FAIL] lifetime: stable escape prepared graph dynamic capacity setup");
        (*fail)++;
        return;
    }
    interp.current_scope = interp.root_scope;
    defer interp.current_scope = saved_scope;

    Value* array_root = stable_escape_capacity_make_array_root(interp, STABLE_ESCAPE_CAPACITY_WIDE_CHILDREN);
    bool array_ok = stable_escape_capacity_prepare_matches(
        interp,
        array_root,
        ARRAY,
        STABLE_ESCAPE_CAPACITY_WIDE_CHILDREN + 1,
        STABLE_ESCAPE_CAPACITY_WIDE_CHILDREN
    );

    Value* dict_root = stable_escape_capacity_make_dict_root(interp, STABLE_ESCAPE_CAPACITY_DICT_ENTRIES);
    bool dict_ok = stable_escape_capacity_prepare_matches(
        interp,
        dict_root,
        HASHMAP,
        STABLE_ESCAPE_CAPACITY_DICT_ENTRIES * 2 + 1,
        STABLE_ESCAPE_CAPACITY_DICT_ENTRIES * 2
    );

    Value* set_root = stable_escape_capacity_make_set_root(interp, STABLE_ESCAPE_CAPACITY_WIDE_CHILDREN);
    bool set_ok = stable_escape_capacity_prepare_matches(
        interp,
        set_root,
        SET,
        STABLE_ESCAPE_CAPACITY_WIDE_CHILDREN + 1,
        STABLE_ESCAPE_CAPACITY_WIDE_CHILDREN
    );

    Value* closure_root = stable_escape_capacity_make_closure_root(interp, STABLE_ESCAPE_CAPACITY_WIDE_CHILDREN);
    bool closure_ok = stable_escape_capacity_prepare_matches(
        interp,
        closure_root,
        CLOSURE,
        STABLE_ESCAPE_CAPACITY_WIDE_CHILDREN + 1,
        STABLE_ESCAPE_CAPACITY_WIDE_CHILDREN
    );

    if (array_ok && dict_ok && set_ok && closure_ok) {
        emit_pass_literal("[PASS] lifetime: stable escape prepared graph grows beyond fixed scratch capacity");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] lifetime: stable escape prepared graph grows beyond fixed scratch capacity (array=%s dict=%s set=%s closure=%s)",
            array_ok ? "yes" : "no",
            dict_ok ? "yes" : "no",
            set_ok ? "yes" : "no",
            closure_ok ? "yes" : "no"
        );
        (*fail)++;
    }
}
