module lisp;

import std::atomic::types;

const usz STABLE_GRAPH_EPOCH_TABLE_CAPACITY = 2048;

struct StableGraphEpochEntry {
    void* ptr;
    uint epoch;
}

types::Atomic{uint} g_stable_graph_mutation_epoch;
types::Atomic{uint} g_stable_graph_epoch_overflow;
StableGraphEpochEntry[STABLE_GRAPH_EPOCH_TABLE_CAPACITY] g_stable_graph_epoch_table;

fn uint stable_graph_mutation_epoch_snapshot() @inline {
    return g_stable_graph_mutation_epoch.load();
}

fn uint stable_graph_next_mutation_epoch() @inline {
    g_stable_graph_mutation_epoch.add(1);
    if (g_stable_graph_mutation_epoch.load() == 0) g_stable_graph_mutation_epoch.store(1);
    return g_stable_graph_mutation_epoch.load();
}

fn usz stable_graph_epoch_slot(void* ptr) @inline {
    return ((usz)ptr >> 4) % STABLE_GRAPH_EPOCH_TABLE_CAPACITY;
}

fn void stable_graph_note_mutation_ptr(void* ptr) {
    uint epoch = stable_graph_next_mutation_epoch();
    if (ptr == null) return;
    usz start = stable_graph_epoch_slot(ptr);
    for (usz probe = 0; probe < STABLE_GRAPH_EPOCH_TABLE_CAPACITY; probe++) {
        usz slot = (start + probe) % STABLE_GRAPH_EPOCH_TABLE_CAPACITY;
        if (g_stable_graph_epoch_table[slot].ptr == null || g_stable_graph_epoch_table[slot].ptr == ptr) {
            g_stable_graph_epoch_table[slot].ptr = ptr;
            g_stable_graph_epoch_table[slot].epoch = epoch;
            return;
        }
    }
    g_stable_graph_epoch_overflow.store(epoch);
}

fn void stable_graph_note_mutation() @inline {
    (void)stable_graph_next_mutation_epoch();
}

fn uint stable_graph_pointer_epoch(void* ptr) {
    uint overflow_epoch = g_stable_graph_epoch_overflow.load();
    if (ptr == null || overflow_epoch != 0) return overflow_epoch;
    usz start = stable_graph_epoch_slot(ptr);
    for (usz probe = 0; probe < STABLE_GRAPH_EPOCH_TABLE_CAPACITY; probe++) {
        usz slot = (start + probe) % STABLE_GRAPH_EPOCH_TABLE_CAPACITY;
        if (g_stable_graph_epoch_table[slot].ptr == ptr) return g_stable_graph_epoch_table[slot].epoch;
        if (g_stable_graph_epoch_table[slot].ptr == null) return 0;
    }
    return overflow_epoch;
}

fn uint stable_graph_value_mutation_epoch(Value* value) @inline {
    if (value == null) return 0;
    switch (value.tag) {
        case CLOSURE:
            return value.closure_val != null ? stable_graph_pointer_epoch((void*)value.closure_val.env) : 0;
        case CONS:
            return stable_graph_pointer_epoch((void*)value);
        case ARRAY:
            return stable_graph_pointer_epoch((void*)value.array_val);
        case HASHMAP:
        case SET:
            return stable_graph_pointer_epoch((void*)value.hashmap_val);
        default:
            return 0;
    }
}
