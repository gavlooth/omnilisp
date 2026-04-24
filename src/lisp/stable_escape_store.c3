module lisp;

import std::atomic::types;
import std::core::mem;
import std::thread;
import main;

const usz STABLE_ESCAPE_STORE_CAPACITY = 512;
const usz STABLE_ESCAPE_PREPARED_MAX_NODES = 64;
const usz STABLE_ESCAPE_PREPARED_MAX_EDGES = 256;
const usz STABLE_ESCAPE_PREPARED_INVALID_INDEX = usz.max;
const uint STABLE_GRAPH_PASSPORT_POLICY_OWNER_SCOPE_CHAIN = 0x1;
const uint STABLE_GRAPH_PASSPORT_POLICY_PREPARED_GRAPH = 0x2;
const uint STABLE_GRAPH_PASSPORT_RISK_MUTABLE_GRAPH = 0x1;
const uint STABLE_GRAPH_PASSPORT_RISK_ENV_CAPTURE = 0x2;
const uint STABLE_GRAPH_PASSPORT_RISK_SHARED_REFERENCE = 0x4;

enum StableEscapeHandleKind : char {
    STABLE_ESCAPE_HANDLE_ROOT_VALUE
}
enum StableGraphPassportInvalidationReason : char {
    STABLE_GRAPH_PASSPORT_VALID,
    STABLE_GRAPH_PASSPORT_OWNER_SCOPE_INVALID,
    STABLE_GRAPH_PASSPORT_OWNER_ESCAPE_CHANGED,
    STABLE_GRAPH_PASSPORT_ROOT_NOT_IN_OWNER_CHAIN,
    STABLE_GRAPH_PASSPORT_PREPARED_GRAPH_STALE,
}

struct StableEscapePreparedNode {
    ValueTag tag;
    Value* source;
    uint mutation_epoch;
    usz child_offset;
    usz child_count;
}

struct StableEscapePreparedGraphScratch {
    StableEscapePreparedNode[STABLE_ESCAPE_PREPARED_MAX_NODES] nodes;
    Value*[STABLE_ESCAPE_PREPARED_MAX_NODES] seen_values;
    usz[STABLE_ESCAPE_PREPARED_MAX_NODES] seen_node_indices;
    usz[STABLE_ESCAPE_PREPARED_MAX_EDGES] edges;
    Env* global_env;
    uint risk_flags;
    usz seen_count;
    usz node_count;
    usz edge_count;
}

struct StableGraphPassport {
    uint owner_scope_generation;
    uint owner_escape_generation;
    usz prepared_node_count;
    usz prepared_edge_count;
    usz prepared_root_index;
    uint mutation_epoch;
    uint policy_flags;
    uint risk_flags;
    StableGraphPassportInvalidationReason invalidation_reason;
}

struct StableEscapeHandle {
    StableEscapeHandleKind kind;
    usz slot;
    uint generation;
    Value* root;
}

struct StableEscapeStoreEntry {
    bool active;
    StableEscapeHandleKind kind;
    uint generation;
    Value* root;
    main::ScopeRegion* owner_scope;
    uint owner_escape_generation;
    StableEscapePreparedNode* prepared_nodes;
    usz prepared_node_count;
    usz* prepared_edges;
    usz prepared_edge_count;
    usz prepared_root_index;
    Env* global_env;
    StableGraphPassport passport;
}

struct StableEscapeStore {
    StableEscapeStoreEntry[STABLE_ESCAPE_STORE_CAPACITY] entries;
    usz next_slot;
    thread::Mutex mu;
    bool initialized;
}

types::Atomic{usz} g_stable_escape_store_publication_hook_count;
types::Atomic{usz} g_stable_escape_store_prepared_publication_hook_count;
types::Atomic{usz} g_stable_escape_store_raw_publication_hook_count;
types::Atomic{usz} g_stable_escape_store_prepare_failure_fallback_count;
types::Atomic{usz} g_stable_escape_store_alias_unsafe_skip_count;
types::Atomic{usz} g_stable_escape_store_resolve_failure_count;

fn uint stable_escape_store_next_generation(uint current) @inline {
    if (current == uint.max) return 1;
    return current + 1;
}

fn StableEscapePreparedNode stable_escape_prepared_scalar_node(Value* value) @inline {
    return {
        .tag = value.tag,
        .source = value,
        .child_offset = STABLE_ESCAPE_PREPARED_INVALID_INDEX,
        .child_count = 0
    };
}

fn bool stable_escape_prepare_graph_append_edge(
    StableEscapePreparedGraphScratch* scratch,
    usz node_index
) @inline {
    if (scratch == null) return false;
    if (scratch.edge_count >= STABLE_ESCAPE_PREPARED_MAX_EDGES) return false;
    scratch.edges[scratch.edge_count++] = node_index;
    return true;
}

fn usz stable_escape_prepared_find_seen_node(
    StableEscapePreparedGraphScratch* scratch,
    Value* value
) @inline {
    if (scratch == null || value == null) return STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    foreach (i, seen_value : scratch.seen_values[0:scratch.seen_count]) {
        if (seen_value == value) return scratch.seen_node_indices[i];
    }
    return STABLE_ESCAPE_PREPARED_INVALID_INDEX;
}

fn bool stable_escape_prepared_register_seen(
    StableEscapePreparedGraphScratch* scratch,
    Value* value,
    usz node_index
) @inline {
    if (scratch == null || value == null) return false;
    if (scratch.seen_count >= STABLE_ESCAPE_PREPARED_MAX_NODES) return false;
    scratch.seen_values[scratch.seen_count] = value;
    scratch.seen_node_indices[scratch.seen_count] = node_index;
    scratch.seen_count++;
    return true;
}

fn bool stable_escape_env_binding_value_count(Env* env, Env* stop_at, usz* out_count) {
    if (out_count == null) return false;
    usz count = 0;
    usz depth = 0;
    Env* cur = env;
    while (cur != null && cur != stop_at) {
        if (depth >= BOUNDARY_ALIAS_MAX_DEPTH) return false;
        depth++;
        if (cur.bindings == null && cur.binding_count > 0) return false;
        for (usz i = 0; i < cur.binding_count; i++) {
            if (cur.bindings[i].value == null) continue;
            if (count == usz.max) return false;
            count++;
        }
        cur = cur.parent;
    }
    *out_count = count;
    return true;
}

fn bool stable_escape_prepare_env_binding_values(
    StableEscapePreparedGraphScratch* scratch,
    Env* env,
    usz depth,
    usz* out_offset,
    usz* out_count
) {
    if (scratch == null || out_offset == null || out_count == null) return false;
    usz* child_indices = (usz*)mem::malloc(usz.sizeof * STABLE_ESCAPE_PREPARED_MAX_EDGES);
    if (child_indices == null) return false;
    defer mem::free(child_indices);
    usz child_count = 0;
    usz env_depth = 0;
    Env* cur = env;
    while (cur != null && cur != scratch.global_env) {
        if (depth + env_depth >= BOUNDARY_ALIAS_MAX_DEPTH) return false;
        env_depth++;
        if (cur.bindings == null && cur.binding_count > 0) return false;
        for (usz i = 0; i < cur.binding_count; i++) {
            Value* binding_value = cur.bindings[i].value;
            if (binding_value == null) continue;
            if (child_count >= STABLE_ESCAPE_PREPARED_MAX_EDGES) return false;
            usz child_index = STABLE_ESCAPE_PREPARED_INVALID_INDEX;
            if (!stable_escape_prepare_graph_node(scratch, binding_value, &child_index, depth + env_depth)) return false;
            child_indices[child_count++] = child_index;
        }
        cur = cur.parent;
    }

    usz child_offset = scratch.edge_count;
    for (usz i = 0; i < child_count; i++) {
        if (!stable_escape_prepare_graph_append_edge(scratch, child_indices[i])) return false;
    }
    *out_offset = child_count > 0 ? child_offset : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    *out_count = child_count;
    return true;
}

fn bool stable_escape_prepare_graph_node(
    StableEscapePreparedGraphScratch* scratch,
    Value* value,
    usz* out_index,
    usz depth = 0
) {
    if (scratch == null || value == null || out_index == null) return false;
    if (depth >= BOUNDARY_ALIAS_MAX_DEPTH) return false;

    usz seen_index = stable_escape_prepared_find_seen_node(scratch, value);
    if (seen_index != STABLE_ESCAPE_PREPARED_INVALID_INDEX) {
        scratch.risk_flags |= STABLE_GRAPH_PASSPORT_RISK_SHARED_REFERENCE;
        *out_index = seen_index;
        return true;
    }

    if (scratch.node_count >= STABLE_ESCAPE_PREPARED_MAX_NODES) return false;

    switch (value.tag) {
        case CLOSURE:
            if (value.closure_val == null) return false;
            scratch.risk_flags |= STABLE_GRAPH_PASSPORT_RISK_MUTABLE_GRAPH | STABLE_GRAPH_PASSPORT_RISK_ENV_CAPTURE;
            usz closure_index = scratch.node_count++;
            scratch.nodes[closure_index] = {
                .tag = CLOSURE,
                .source = value,
                .child_offset = STABLE_ESCAPE_PREPARED_INVALID_INDEX,
                .child_count = 0
            };
            scratch.nodes[closure_index].mutation_epoch = stable_graph_value_mutation_epoch(value);
            if (!stable_escape_prepared_register_seen(scratch, value, closure_index)) return false;

            usz env_child_offset = STABLE_ESCAPE_PREPARED_INVALID_INDEX;
            usz env_child_count = 0;
            if (!stable_escape_prepare_env_binding_values(
                scratch,
                value.closure_val.env,
                depth + 1,
                &env_child_offset,
                &env_child_count
            )) return false;
            scratch.nodes[closure_index].child_offset = env_child_offset;
            scratch.nodes[closure_index].child_count = env_child_count;
            *out_index = closure_index;
            return true;
        case CONS:
            scratch.risk_flags |= STABLE_GRAPH_PASSPORT_RISK_MUTABLE_GRAPH;
            usz node_index = scratch.node_count++;
            scratch.nodes[node_index] = {
                .tag = CONS,
                .source = value,
                .child_offset = STABLE_ESCAPE_PREPARED_INVALID_INDEX,
                .child_count = 0
            };
            scratch.nodes[node_index].mutation_epoch = stable_graph_value_mutation_epoch(value);
            if (!stable_escape_prepared_register_seen(scratch, value, node_index)) return false;

            usz car_index = STABLE_ESCAPE_PREPARED_INVALID_INDEX;
            usz cdr_index = STABLE_ESCAPE_PREPARED_INVALID_INDEX;
            if (!stable_escape_prepare_graph_node(scratch, value.cons_val.car, &car_index, depth + 1)) return false;
            if (!stable_escape_prepare_graph_node(scratch, value.cons_val.cdr, &cdr_index, depth + 1)) return false;
            usz child_offset = scratch.edge_count;
            if (!stable_escape_prepare_graph_append_edge(scratch, car_index)) return false;
            if (!stable_escape_prepare_graph_append_edge(scratch, cdr_index)) return false;
            scratch.nodes[node_index].child_offset = child_offset;
            scratch.nodes[node_index].child_count = 2;
            *out_index = node_index;
            return true;
        case ARRAY:
            if (value.array_val == null) return false;
            scratch.risk_flags |= STABLE_GRAPH_PASSPORT_RISK_MUTABLE_GRAPH;
            usz array_index = scratch.node_count++;
            scratch.nodes[array_index] = {
                .tag = ARRAY,
                .source = value,
                .child_offset = STABLE_ESCAPE_PREPARED_INVALID_INDEX,
                .child_count = 0
            };
            scratch.nodes[array_index].mutation_epoch = stable_graph_value_mutation_epoch(value);
            if (!stable_escape_prepared_register_seen(scratch, value, array_index)) return false;

            if (value.array_val.length > STABLE_ESCAPE_PREPARED_MAX_EDGES) return false;
            usz* array_child_indices = (usz*)mem::malloc(usz.sizeof * STABLE_ESCAPE_PREPARED_MAX_EDGES);
            if (array_child_indices == null) return false;
            defer mem::free(array_child_indices);
            for (usz i = 0; i < value.array_val.length; i++) {
                usz child_index = STABLE_ESCAPE_PREPARED_INVALID_INDEX;
                if (!stable_escape_prepare_graph_node(scratch, value.array_val.items[i], &child_index, depth + 1)) {
                    return false;
                }
                array_child_indices[i] = child_index;
            }
            usz child_offset = scratch.edge_count;
            for (usz i = 0; i < value.array_val.length; i++) {
                if (!stable_escape_prepare_graph_append_edge(scratch, array_child_indices[i])) return false;
            }
            scratch.nodes[array_index].child_offset =
                value.array_val.length > 0 ? child_offset : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
            scratch.nodes[array_index].child_count = value.array_val.length;
            *out_index = array_index;
            return true;
        case HASHMAP:
            if (value.hashmap_val == null || value.hashmap_val.entries == null) return false;
            scratch.risk_flags |= STABLE_GRAPH_PASSPORT_RISK_MUTABLE_GRAPH;
            usz hashmap_index = scratch.node_count++;
            scratch.nodes[hashmap_index] = {
                .tag = HASHMAP,
                .source = value,
                .child_offset = STABLE_ESCAPE_PREPARED_INVALID_INDEX,
                .child_count = 0
            };
            scratch.nodes[hashmap_index].mutation_epoch = stable_graph_value_mutation_epoch(value);
            if (!stable_escape_prepared_register_seen(scratch, value, hashmap_index)) return false;

            HashMap* map = value.hashmap_val;
            usz* pair_child_indices = (usz*)mem::malloc(usz.sizeof * STABLE_ESCAPE_PREPARED_MAX_EDGES);
            if (pair_child_indices == null) return false;
            defer mem::free(pair_child_indices);
            usz pair_child_count = 0;
            for (uint i = 0; i < map.capacity; i++) {
                HashEntry* entry = &map.entries[i];
                if (entry.key == null) continue;
                if (pair_child_count > STABLE_ESCAPE_PREPARED_MAX_EDGES - (usz)2) return false;
                usz key_index = STABLE_ESCAPE_PREPARED_INVALID_INDEX;
                usz value_index = STABLE_ESCAPE_PREPARED_INVALID_INDEX;
                if (!stable_escape_prepare_graph_node(scratch, entry.key, &key_index, depth + 1)) return false;
                if (!stable_escape_prepare_graph_node(scratch, entry.value, &value_index, depth + 1)) return false;
                pair_child_indices[pair_child_count++] = key_index;
                pair_child_indices[pair_child_count++] = value_index;
            }
            usz pair_child_offset = scratch.edge_count;
            for (usz i = 0; i < pair_child_count; i++) {
                if (!stable_escape_prepare_graph_append_edge(scratch, pair_child_indices[i])) return false;
            }
            scratch.nodes[hashmap_index].child_offset =
                pair_child_count > 0 ? pair_child_offset : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
            scratch.nodes[hashmap_index].child_count = pair_child_count;
            *out_index = hashmap_index;
            return true;
        case SET:
            if (value.hashmap_val == null || value.hashmap_val.entries == null) return false;
            scratch.risk_flags |= STABLE_GRAPH_PASSPORT_RISK_MUTABLE_GRAPH;
            usz set_index = scratch.node_count++;
            scratch.nodes[set_index] = {
                .tag = SET,
                .source = value,
                .child_offset = STABLE_ESCAPE_PREPARED_INVALID_INDEX,
                .child_count = 0
            };
            scratch.nodes[set_index].mutation_epoch = stable_graph_value_mutation_epoch(value);
            if (!stable_escape_prepared_register_seen(scratch, value, set_index)) return false;

            HashMap* set_map = value.hashmap_val;
            usz* member_child_indices = (usz*)mem::malloc(usz.sizeof * STABLE_ESCAPE_PREPARED_MAX_EDGES);
            if (member_child_indices == null) return false;
            defer mem::free(member_child_indices);
            usz member_child_count = 0;
            for (uint i = 0; i < set_map.capacity; i++) {
                HashEntry* entry = &set_map.entries[i];
                if (entry.key == null) continue;
                if (member_child_count >= STABLE_ESCAPE_PREPARED_MAX_EDGES) return false;
                usz member_index = STABLE_ESCAPE_PREPARED_INVALID_INDEX;
                if (!stable_escape_prepare_graph_node(scratch, entry.key, &member_index, depth + 1)) return false;
                member_child_indices[member_child_count++] = member_index;
            }
            usz member_child_offset = scratch.edge_count;
            for (usz i = 0; i < member_child_count; i++) {
                if (!stable_escape_prepare_graph_append_edge(scratch, member_child_indices[i])) return false;
            }
            scratch.nodes[set_index].child_offset =
                member_child_count > 0 ? member_child_offset : STABLE_ESCAPE_PREPARED_INVALID_INDEX;
            scratch.nodes[set_index].child_count = member_child_count;
            *out_index = set_index;
            return true;
        default:
            if (boundary_alias_value_has_child_payload(value)) return false;
            usz scalar_index = scratch.node_count++;
            scratch.nodes[scalar_index] = stable_escape_prepared_scalar_node(value);
            if (!stable_escape_prepared_register_seen(scratch, value, scalar_index)) return false;
            *out_index = scalar_index;
            return true;
    }
}

fn StableEscapePreparedNode* stable_escape_prepare_graph_heap_copy(
    StableEscapePreparedGraphScratch* scratch
) {
    if (scratch == null || scratch.node_count == 0) return null;
    if (scratch.node_count > usz.max / StableEscapePreparedNode.sizeof) return null;

    StableEscapePreparedNode* prepared_nodes =
        (StableEscapePreparedNode*)mem::malloc(StableEscapePreparedNode.sizeof * scratch.node_count);
    if (prepared_nodes == null) return null;
    foreach (i, node : scratch.nodes[0:scratch.node_count]) {
        prepared_nodes[i] = node;
    }
    return prepared_nodes;
}

fn usz* stable_escape_prepare_graph_edges_heap_copy(
    StableEscapePreparedGraphScratch* scratch
) {
    if (scratch == null || scratch.edge_count == 0) return null;
    if (scratch.edge_count > usz.max / usz.sizeof) return null;

    usz* prepared_edges = (usz*)mem::malloc(usz.sizeof * scratch.edge_count);
    if (prepared_edges == null) return null;
    foreach (i, edge : scratch.edges[0:scratch.edge_count]) {
        prepared_edges[i] = edge;
    }
    return prepared_edges;
}

fn StableEscapeStore* stable_escape_store_from_interp(Interp* interp) @inline {
    if (interp == null) return null;
    return interp.stable_escape_store;
}

fn bool stable_escape_store_root_publishable_in_owner_scope(Value* root, main::ScopeRegion* owner_scope) @inline {
    if (root == null || owner_scope == null) return false;
    main::ScopeRegion* cur = owner_scope;
    while (cur != null) {
        if ((root.scope_gen == cur.generation || root.scope_gen == cur.escape_generation) &&
            main::scope_contains(cur, (void*)root)) {
            return true;
        }
        cur = cur.parent;
    }
    return false;
}

fn usz stable_escape_entry_node_index_for_source(StableEscapeStoreEntry* entry, Value* source) @inline {
    if (entry == null || source == null || entry.prepared_nodes == null) return STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    foreach (i, node : entry.prepared_nodes[0:entry.prepared_node_count]) {
        if (node.source == source) return i;
    }
    return STABLE_ESCAPE_PREPARED_INVALID_INDEX;
}

fn bool stable_escape_entry_edge_matches(
    StableEscapeStoreEntry* entry,
    StableEscapePreparedNode* node,
    usz ordinal,
    Value* source
) @inline {
    if (entry == null || node == null || source == null) return false;
    if (ordinal >= node.child_count) return false;
    if (node.child_offset == STABLE_ESCAPE_PREPARED_INVALID_INDEX) return false;
    if (node.child_offset + ordinal >= entry.prepared_edge_count) return false;
    usz expected_index = stable_escape_entry_node_index_for_source(entry, source);
    if (expected_index == STABLE_ESCAPE_PREPARED_INVALID_INDEX) return false;
    return entry.prepared_edges[node.child_offset + ordinal] == expected_index;
}

fn bool stable_escape_entry_env_edges_match(
    StableEscapeStoreEntry* entry,
    StableEscapePreparedNode* node,
    Env* env
) {
    if (entry == null || node == null) return false;
    usz ordinal = 0;
    usz depth = 0;
    Env* cur = env;
    while (cur != null && cur != entry.global_env) {
        if (depth >= BOUNDARY_ALIAS_MAX_DEPTH) return false;
        depth++;
        if (cur.bindings == null && cur.binding_count > 0) return false;
        for (usz i = 0; i < cur.binding_count; i++) {
            Value* binding_value = cur.bindings[i].value;
            if (binding_value == null) continue;
            if (!stable_escape_entry_edge_matches(entry, node, ordinal, binding_value)) return false;
            ordinal++;
        }
        cur = cur.parent;
    }
    return ordinal == node.child_count;
}

fn bool stable_escape_store_prepared_nodes_alive(
    StableEscapeStoreEntry* entry
) @inline {
    if (entry == null) return false;
    if (entry.prepared_node_count == 0) return true;
    if (entry.prepared_nodes == null) return false;
    if (entry.prepared_edge_count > 0 && entry.prepared_edges == null) return false;
    if (entry.prepared_root_index >= entry.prepared_node_count) return false;

    foreach (node : entry.prepared_nodes[0:entry.prepared_node_count]) {
        if (node.source == null) return false;
        if (node.source.tag != node.tag) return false;
        if (node.mutation_epoch != stable_graph_value_mutation_epoch(node.source)) return false;
        if (!boundary_ptr_in_scope_chain_scan_unchecked((void*)node.source, entry.owner_scope)) {
            return false;
        }

        if (node.child_count == 0) {
            if (node.child_offset != STABLE_ESCAPE_PREPARED_INVALID_INDEX) return false;
        } else {
            if (node.child_offset == STABLE_ESCAPE_PREPARED_INVALID_INDEX) return false;
            if (node.child_offset >= entry.prepared_edge_count) return false;
            if (node.child_count > entry.prepared_edge_count - node.child_offset) return false;
        }

        switch (node.tag) {
            case CLOSURE:
                if (node.source.closure_val == null) return false;
                usz closure_child_count = 0;
                if (!stable_escape_env_binding_value_count(
                    node.source.closure_val.env,
                    entry.global_env,
                    &closure_child_count
                )) return false;
                if (node.child_count != closure_child_count) return false;
                if (!stable_escape_entry_env_edges_match(entry, &node, node.source.closure_val.env)) return false;
                break;
            case CONS:
                if (node.child_count != 2) return false;
                if (!stable_escape_entry_edge_matches(entry, &node, 0, node.source.cons_val.car)) return false;
                if (!stable_escape_entry_edge_matches(entry, &node, 1, node.source.cons_val.cdr)) return false;
                break;
            case ARRAY:
                if (node.source.array_val == null) return false;
                if (node.child_count != node.source.array_val.length) return false;
                for (usz i = 0; i < node.source.array_val.length; i++) {
                    if (!stable_escape_entry_edge_matches(entry, &node, i, node.source.array_val.items[i])) return false;
                }
                break;
            case HASHMAP:
                if (node.source.hashmap_val == null || node.source.hashmap_val.entries == null) return false;
                if ((usz)node.source.hashmap_val.count > usz.max / (usz)2) return false;
                if (node.child_count != (usz)node.source.hashmap_val.count * (usz)2) return false;
                usz pair_ordinal = 0;
                for (uint i = 0; i < node.source.hashmap_val.capacity; i++) {
                    HashEntry* map_entry = &node.source.hashmap_val.entries[i];
                    if (map_entry.key == null) continue;
                    if (!stable_escape_entry_edge_matches(entry, &node, pair_ordinal, map_entry.key)) return false;
                    pair_ordinal++;
                    if (!stable_escape_entry_edge_matches(entry, &node, pair_ordinal, map_entry.value)) return false;
                    pair_ordinal++;
                }
                if (pair_ordinal != node.child_count) return false;
                break;
            case SET:
                if (node.source.hashmap_val == null || node.source.hashmap_val.entries == null) return false;
                if (node.child_count != (usz)node.source.hashmap_val.count) return false;
                usz member_ordinal = 0;
                for (uint i = 0; i < node.source.hashmap_val.capacity; i++) {
                    HashEntry* set_entry = &node.source.hashmap_val.entries[i];
                    if (set_entry.key == null) continue;
                    if (!stable_escape_entry_edge_matches(entry, &node, member_ordinal, set_entry.key)) return false;
                    member_ordinal++;
                }
                if (member_ordinal != node.child_count) return false;
                break;
            default:
                if (node.child_count != 0) return false;
                break;
        }

        if (node.child_count > 0) {
            foreach (edge : entry.prepared_edges[node.child_offset : node.child_count]) {
                if (edge >= entry.prepared_node_count) return false;
            }
        }
    }

    return true;
}

fn bool stable_escape_store_ensure_ready(Interp* interp) {
    StableEscapeStore* store = stable_escape_store_from_interp(interp);
    if (store == null) return false;
    if (store.initialized) return true;
    if (catch err = store.mu.init()) return false;
    store.initialized = true;
    store.next_slot = 0;
    return true;
}

fn void stable_escape_store_reset_publication_hook_count() @inline {
    g_stable_escape_store_publication_hook_count.store(0);
}

fn usz stable_escape_store_publication_hook_count_snapshot() @inline {
    return g_stable_escape_store_publication_hook_count.load();
}

fn void stable_escape_store_reset_prepared_publication_hook_count() @inline {
    g_stable_escape_store_prepared_publication_hook_count.store(0);
}

fn usz stable_escape_store_prepared_publication_hook_count_snapshot() @inline {
    return g_stable_escape_store_prepared_publication_hook_count.load();
}

fn void stable_escape_store_reset_publication_outcome_counts() @inline {
    g_stable_escape_store_raw_publication_hook_count.store(0);
    g_stable_escape_store_prepare_failure_fallback_count.store(0);
    g_stable_escape_store_alias_unsafe_skip_count.store(0);
    g_stable_escape_store_resolve_failure_count.store(0);
}

fn usz stable_escape_store_raw_publication_hook_count_snapshot() @inline {
    return g_stable_escape_store_raw_publication_hook_count.load();
}

fn usz stable_escape_store_prepare_failure_fallback_count_snapshot() @inline {
    return g_stable_escape_store_prepare_failure_fallback_count.load();
}

fn usz stable_escape_store_alias_unsafe_skip_count_snapshot() @inline {
    return g_stable_escape_store_alias_unsafe_skip_count.load();
}

fn usz stable_escape_store_resolve_failure_count_snapshot() @inline {
    return g_stable_escape_store_resolve_failure_count.load();
}

fn void stable_escape_store_reset(Interp* interp) {
    StableEscapeStore* store = stable_escape_store_from_interp(interp);
    if (store == null || !store.initialized) return;

    main::ScopeRegion*[STABLE_ESCAPE_STORE_CAPACITY] release_scopes;
    StableEscapePreparedNode*[STABLE_ESCAPE_STORE_CAPACITY] free_prepared_nodes;
    usz*[STABLE_ESCAPE_STORE_CAPACITY] free_prepared_edges;
    usz release_count = 0;
    usz free_node_count = 0;
    usz free_edge_count = 0;

    store.mu.lock();
    for (usz i = 0; i < STABLE_ESCAPE_STORE_CAPACITY; i++) {
        StableEscapeStoreEntry* entry = &store.entries[i];
        if (entry.active && entry.owner_scope != null) {
            release_scopes[release_count++] = entry.owner_scope;
        }
        if (entry.prepared_nodes != null) {
            free_prepared_nodes[free_node_count++] = entry.prepared_nodes;
        }
        if (entry.prepared_edges != null) {
            free_prepared_edges[free_edge_count++] = entry.prepared_edges;
        }
        *entry = {};
    }
    store.next_slot = 0;
    store.mu.unlock();

    for (usz i = 0; i < release_count; i++) {
        main::scope_release(release_scopes[i]);
    }
    for (usz i = 0; i < free_node_count; i++) {
        mem::free(free_prepared_nodes[i]);
    }
    for (usz i = 0; i < free_edge_count; i++) {
        mem::free(free_prepared_edges[i]);
    }
}

fn bool stable_escape_store_publish_entry(
    Interp* interp,
    Value* root,
    StableEscapePreparedNode* prepared_nodes,
    usz prepared_node_count,
    usz* prepared_edges,
    usz prepared_edge_count,
    usz prepared_root_index,
    StableGraphPassport passport,
    StableEscapeHandle* out_handle
) {
    if (interp == null || root == null || out_handle == null) return false;
    main::ScopeRegion* owner_scope = interp.current_scope;
    if (owner_scope == null) return false;
    if (!stable_escape_store_root_publishable_in_owner_scope(root, owner_scope)) return false;
    if (!stable_escape_store_ensure_ready(interp)) return false;

    StableEscapeStore* store = stable_escape_store_from_interp(interp);
    if (store == null) return false;
    store.mu.lock();
    defer store.mu.unlock();

    for (usz i = 0; i < STABLE_ESCAPE_STORE_CAPACITY; i++) {
        usz slot = (store.next_slot + i) % STABLE_ESCAPE_STORE_CAPACITY;
        StableEscapeStoreEntry* entry = &store.entries[slot];
        if (entry.active) continue;

        uint generation = entry.generation;
        if (generation == 0) generation = 1;

        entry.active = true;
        entry.kind = STABLE_ESCAPE_HANDLE_ROOT_VALUE;
        entry.generation = generation;
        entry.root = root;
        entry.owner_scope = owner_scope;
        entry.owner_escape_generation = owner_scope.escape_generation;
        entry.prepared_nodes = prepared_nodes;
        entry.prepared_node_count = prepared_node_count;
        entry.prepared_edges = prepared_edges;
        entry.prepared_edge_count = prepared_edge_count;
        entry.prepared_root_index = prepared_root_index;
        entry.global_env = interp.global_env;
        entry.passport = passport;
        main::scope_retain(owner_scope);
        store.next_slot = (slot + 1) % STABLE_ESCAPE_STORE_CAPACITY;

        *out_handle = {
            .kind = entry.kind,
            .slot = slot,
            .generation = generation,
            .root = root
        };
        return true;
    }

    return false;
}

fn bool stable_escape_store_publish_value(
    Interp* interp,
    Value* root,
    StableEscapeHandle* out_handle
) {
    return stable_escape_store_publish_entry(
        interp,
        root,
        null,
        0,
        null,
        0,
        STABLE_ESCAPE_PREPARED_INVALID_INDEX,
        {},
        out_handle
    );
}

fn bool stable_escape_store_prepare_value(
    Interp* interp,
    Value* root,
    StableEscapeHandle* out_handle
) {
    if (interp == null || root == null || out_handle == null) return false;

    StableEscapePreparedGraphScratch scratch = {};
    scratch.global_env = interp.global_env;
    usz root_index = STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    if (!stable_escape_prepare_graph_node(&scratch, root, &root_index)) return false;

    StableEscapePreparedNode* prepared_nodes = stable_escape_prepare_graph_heap_copy(&scratch);
    if (prepared_nodes == null) return false;
    usz* prepared_edges = stable_escape_prepare_graph_edges_heap_copy(&scratch);
    if (scratch.edge_count > 0 && prepared_edges == null) {
        mem::free(prepared_nodes);
        return false;
    }
    if (root_index >= scratch.node_count) {
        if (prepared_edges != null) mem::free(prepared_edges);
        mem::free(prepared_nodes);
        return false;
    }

    if (!stable_escape_store_publish_entry(
            interp,
            root,
            prepared_nodes,
            scratch.node_count,
            prepared_edges,
            scratch.edge_count,
            root_index,
            {
                .owner_scope_generation = interp.current_scope != null ? interp.current_scope.generation : 0,
                .owner_escape_generation = interp.current_scope != null ? interp.current_scope.escape_generation : 0,
                .prepared_node_count = scratch.node_count,
                .prepared_edge_count = scratch.edge_count,
                .prepared_root_index = root_index,
                .mutation_epoch = stable_graph_mutation_epoch_snapshot(),
                .policy_flags = STABLE_GRAPH_PASSPORT_POLICY_OWNER_SCOPE_CHAIN | STABLE_GRAPH_PASSPORT_POLICY_PREPARED_GRAPH,
                .risk_flags = scratch.risk_flags,
                .invalidation_reason = STABLE_GRAPH_PASSPORT_VALID
            },
            out_handle
        )) {
        if (prepared_edges != null) mem::free(prepared_edges);
        mem::free(prepared_nodes);
        return false;
    }
    return true;
}

fn bool stable_escape_store_handle_is_alive(Interp* interp, StableEscapeHandle* handle) {
    if (handle == null || handle.kind != STABLE_ESCAPE_HANDLE_ROOT_VALUE) return false;
    StableEscapeStore* store = stable_escape_store_from_interp(interp);
    if (store == null || !store.initialized || handle.slot >= STABLE_ESCAPE_STORE_CAPACITY) return false;

    store.mu.lock();
    defer store.mu.unlock();

    StableEscapeStoreEntry* entry = &store.entries[handle.slot];
    if (!entry.active) return false;
    if (entry.kind != handle.kind) return false;
    if (entry.generation != handle.generation) return false;
    if (entry.root != handle.root) return false;
    if (entry.owner_scope == null) return false;
    if (entry.owner_scope.generation == 0) return false;
    if (entry.owner_scope.escape_generation != entry.owner_escape_generation) return false;
    if (!stable_escape_store_root_publishable_in_owner_scope(entry.root, entry.owner_scope)) return false;
    if (!stable_escape_store_prepared_nodes_alive(entry)) return false;
    return true;
}

fn StableEscapeStoreEntry* stable_escape_store_acquire_entry(StableEscapeStore* store, StableEscapeHandle* handle) {
    if (handle == null || handle.kind != STABLE_ESCAPE_HANDLE_ROOT_VALUE) return null;
    if (store == null || !store.initialized || handle.slot >= STABLE_ESCAPE_STORE_CAPACITY) return null;

    store.mu.lock();
    StableEscapeStoreEntry* entry = &store.entries[handle.slot];
    if (!entry.active ||
        entry.kind != handle.kind ||
        entry.generation != handle.generation ||
        entry.root != handle.root ||
        entry.owner_scope == null ||
        entry.owner_scope.generation == 0 ||
        entry.owner_scope.escape_generation != entry.owner_escape_generation ||
        !stable_escape_store_root_publishable_in_owner_scope(entry.root, entry.owner_scope) ||
        !stable_escape_store_prepared_nodes_alive(entry)) {
        store.mu.unlock();
        return null;
    }
    return entry;
}

fn void stable_escape_store_release_entry(StableEscapeStore* store) {
    if (store != null) store.mu.unlock();
}

fn Value* stable_escape_store_resolve_value(Interp* interp, StableEscapeHandle* handle) {
    StableEscapeStore* store = stable_escape_store_from_interp(interp);
    StableEscapeStoreEntry* entry = stable_escape_store_acquire_entry(store, handle);
    if (entry == null) return null;
    defer stable_escape_store_release_entry(store);
    return entry.root;
}

fn usz stable_escape_store_prepared_node_count(Interp* interp, StableEscapeHandle* handle) {
    StableEscapeStore* store = stable_escape_store_from_interp(interp);
    StableEscapeStoreEntry* entry = stable_escape_store_acquire_entry(store, handle);
    if (entry == null) return 0;
    defer stable_escape_store_release_entry(store);
    return entry.prepared_node_count;
}

fn usz stable_escape_store_prepared_root_index(Interp* interp, StableEscapeHandle* handle) {
    StableEscapeStore* store = stable_escape_store_from_interp(interp);
    StableEscapeStoreEntry* entry = stable_escape_store_acquire_entry(store, handle);
    if (entry == null) return STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    defer stable_escape_store_release_entry(store);
    return entry.prepared_root_index;
}

fn usz stable_escape_store_prepared_node_child_count(
    Interp* interp,
    StableEscapeHandle* handle,
    usz node_index
) {
    StableEscapeStore* store = stable_escape_store_from_interp(interp);
    StableEscapeStoreEntry* entry = stable_escape_store_acquire_entry(store, handle);
    if (entry == null) return 0;
    defer stable_escape_store_release_entry(store);
    if (entry.prepared_nodes == null || node_index >= entry.prepared_node_count) return 0;
    return entry.prepared_nodes[node_index].child_count;
}

fn ValueTag stable_escape_store_prepared_node_tag(
    Interp* interp,
    StableEscapeHandle* handle,
    usz node_index
) {
    StableEscapeStore* store = stable_escape_store_from_interp(interp);
    StableEscapeStoreEntry* entry = stable_escape_store_acquire_entry(store, handle);
    if (entry == null) return VOID;
    defer stable_escape_store_release_entry(store);
    if (entry.prepared_nodes == null || node_index >= entry.prepared_node_count) return VOID;
    return entry.prepared_nodes[node_index].tag;
}

fn usz stable_escape_store_prepared_node_child(
    Interp* interp,
    StableEscapeHandle* handle,
    usz node_index,
    uint child_ordinal
) {
    StableEscapeStore* store = stable_escape_store_from_interp(interp);
    StableEscapeStoreEntry* entry = stable_escape_store_acquire_entry(store, handle);
    if (entry == null) return STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    defer stable_escape_store_release_entry(store);
    if (entry.prepared_nodes == null || node_index >= entry.prepared_node_count) {
        return STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    }

    StableEscapePreparedNode* node = &entry.prepared_nodes[node_index];
    if (child_ordinal >= node.child_count || node.child_offset == STABLE_ESCAPE_PREPARED_INVALID_INDEX) {
        return STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    }
    if (node.child_offset + child_ordinal >= entry.prepared_edge_count) return STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    return entry.prepared_edges[node.child_offset + child_ordinal];
}

fn Value* stable_escape_store_publish_and_resolve_value(Interp* interp, Value* root) {
    if (interp == null || root == null) return root;
    StableEscapeHandle handle = {};
    if (!stable_escape_store_publish_value(interp, root, &handle)) return root;
    Value* resolved = stable_escape_store_resolve_value(interp, &handle);
    if (resolved == null) {
        g_stable_escape_store_resolve_failure_count.add(1);
        (void)stable_escape_store_retire(interp, &handle);
        return root;
    }
    g_stable_escape_store_raw_publication_hook_count.add(1);
    g_stable_escape_store_publication_hook_count.add(1);
    (void)stable_escape_store_retire(interp, &handle);
    return resolved;
}

fn Value* stable_escape_prepare_and_publish_value(Interp* interp, Value* root) {
    if (interp == null || root == null) return root;
    if (boundary_graph_alias_unsafe_for_reuse(root, interp)) {
        g_stable_escape_store_alias_unsafe_skip_count.add(1);
        return root;
    }
    StableEscapeHandle handle = {};
    if (!stable_escape_store_prepare_value(interp, root, &handle)) {
        g_stable_escape_store_prepare_failure_fallback_count.add(1);
        return stable_escape_store_publish_and_resolve_value(interp, root);
    }

    Value* resolved = stable_escape_store_resolve_value(interp, &handle);
    if (resolved == null) {
        g_stable_escape_store_resolve_failure_count.add(1);
        (void)stable_escape_store_retire(interp, &handle);
        return root;
    }
    g_stable_escape_store_prepared_publication_hook_count.add(1);
    g_stable_escape_store_publication_hook_count.add(1);
    (void)stable_escape_store_retire(interp, &handle);
    return resolved;
}

fn bool stable_escape_store_retire(Interp* interp, StableEscapeHandle* handle) {
    if (handle == null || handle.kind != STABLE_ESCAPE_HANDLE_ROOT_VALUE) return false;
    StableEscapeStore* store = stable_escape_store_from_interp(interp);
    if (store == null || !store.initialized || handle.slot >= STABLE_ESCAPE_STORE_CAPACITY) return false;

    main::ScopeRegion* owner_scope = null;
    StableEscapePreparedNode* prepared_nodes = null;
    usz* prepared_edges = null;
    store.mu.lock();
    StableEscapeStoreEntry* entry = &store.entries[handle.slot];
    if (!entry.active ||
        entry.kind != handle.kind ||
        entry.generation != handle.generation ||
        entry.root != handle.root) {
        store.mu.unlock();
        return false;
    }

    owner_scope = entry.owner_scope;
    prepared_nodes = entry.prepared_nodes;
    prepared_edges = entry.prepared_edges;
    entry.active = false;
    entry.kind = STABLE_ESCAPE_HANDLE_ROOT_VALUE;
    entry.generation = stable_escape_store_next_generation(entry.generation);
    entry.root = null;
    entry.owner_scope = null;
    entry.owner_escape_generation = 0;
    entry.prepared_nodes = null;
    entry.prepared_node_count = 0;
    entry.prepared_edges = null;
    entry.prepared_edge_count = 0;
    entry.prepared_root_index = STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    entry.global_env = null;
    entry.passport = {};
    store.mu.unlock();
    if (owner_scope != null) {
        main::scope_release(owner_scope);
    }
    if (prepared_nodes != null) {
        mem::free(prepared_nodes);
    }
    if (prepared_edges != null) {
        mem::free(prepared_edges);
    }
    return true;
}
