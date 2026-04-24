module lisp;

import std::core::mem;

fn StableEscapePreparedNode stable_escape_prepared_scalar_node(Value* value) @inline {
    return {
        .tag = value.tag,
        .source = value,
        .child_offset = STABLE_ESCAPE_PREPARED_INVALID_INDEX,
        .child_count = 0
    };
}

fn bool stable_escape_prepare_graph_grow_usz_buffer(usz** items, usz* capacity, usz required, usz initial_capacity) {
    if (items == null || capacity == null) return false;
    if (required <= *capacity) return true;
    usz new_capacity = *capacity > 0 ? *capacity : initial_capacity;
    if (new_capacity == 0) new_capacity = 1;
    while (new_capacity < required) {
        if (new_capacity > usz.max / (usz)2) return false;
        new_capacity *= 2;
    }
    if (new_capacity > usz.max / usz.sizeof) return false;
    usz* grown = (usz*)mem::realloc(*items, usz.sizeof * new_capacity);
    if (grown == null) return false;
    *items = grown;
    *capacity = new_capacity;
    return true;
}

fn bool stable_escape_prepare_graph_append_index(usz** items, usz* count, usz* capacity, usz node_index) @inline {
    if (count == null || *count == usz.max) return false;
    if (!stable_escape_prepare_graph_grow_usz_buffer(items, capacity, *count + 1, 8)) return false;
    (*items)[(*count)++] = node_index;
    return true;
}

fn bool stable_escape_prepare_graph_ensure_node_capacity(StableEscapePreparedGraphScratch* scratch, usz required) {
    if (scratch == null) return false;
    if (required <= scratch.node_capacity) return true;
    usz new_capacity = scratch.node_capacity > 0 ? scratch.node_capacity : STABLE_ESCAPE_PREPARED_INITIAL_NODES;
    while (new_capacity < required) {
        if (new_capacity > usz.max / (usz)2) return false;
        new_capacity *= 2;
    }
    if (new_capacity > usz.max / StableEscapePreparedNode.sizeof ||
        new_capacity > usz.max / Value*.sizeof ||
        new_capacity > usz.max / usz.sizeof) {
        return false;
    }
    StableEscapePreparedNode* grown_nodes =
        (StableEscapePreparedNode*)mem::realloc(scratch.nodes, StableEscapePreparedNode.sizeof * new_capacity);
    if (grown_nodes == null) return false;
    scratch.nodes = grown_nodes;
    Value** grown_seen_values = (Value**)mem::realloc(scratch.seen_values, Value*.sizeof * new_capacity);
    if (grown_seen_values == null) return false;
    scratch.seen_values = grown_seen_values;
    usz* grown_seen_indices = (usz*)mem::realloc(scratch.seen_node_indices, usz.sizeof * new_capacity);
    if (grown_seen_indices == null) return false;
    scratch.seen_node_indices = grown_seen_indices;
    scratch.node_capacity = new_capacity;
    return true;
}

fn bool stable_escape_prepare_graph_reserve_node(StableEscapePreparedGraphScratch* scratch, usz* out_index) @inline {
    if (scratch == null || out_index == null || scratch.node_count == usz.max) return false;
    if (!stable_escape_prepare_graph_ensure_node_capacity(scratch, scratch.node_count + 1)) return false;
    *out_index = scratch.node_count++;
    return true;
}

fn void stable_escape_prepare_graph_scratch_free(StableEscapePreparedGraphScratch* scratch) {
    if (scratch == null) return;
    if (scratch.nodes != null) mem::free(scratch.nodes);
    if (scratch.seen_values != null) mem::free(scratch.seen_values);
    if (scratch.seen_node_indices != null) mem::free(scratch.seen_node_indices);
    if (scratch.edges != null) mem::free(scratch.edges);
    *scratch = {};
}

fn bool stable_escape_prepare_graph_append_edge(
    StableEscapePreparedGraphScratch* scratch,
    usz node_index
) @inline {
    if (scratch == null) return false;
    if (scratch.edge_count == usz.max) return false;
    if (!stable_escape_prepare_graph_grow_usz_buffer(
        &scratch.edges,
        &scratch.edge_capacity,
        scratch.edge_count + 1,
        STABLE_ESCAPE_PREPARED_INITIAL_EDGES
    )) return false;
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
    if (scratch.seen_count >= scratch.node_capacity) return false;
    scratch.seen_values[scratch.seen_count] = value;
    scratch.seen_node_indices[scratch.seen_count] = node_index;
    scratch.seen_count++;
    return true;
}
