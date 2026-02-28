/**
 * =============================================================================
 * REGION MEMORY SYSTEM v3 — C3 Implementation (MINIMAL)
 * =============================================================================
 *
 * This is a minimal version of the region memory system, preserved only for
 * Expr and Pattern allocation in the compiler.
 *
 * All production memory management has been migrated to scope_region.c3.
 */
module main;

import std::collections::list;
import std::io;

// =============================================================================
// SECTION 1: PRIMITIVE TYPE ALIASES
// =============================================================================

typedef RegionId   = uint;
typedef SlotId     = uint;
typedef PoolId     = uint;
typedef PoolIndex  = uint;
typedef Generation = uint;
typedef RefCount   = uint;

// =============================================================================
// SECTION 2: HANDLE TYPES
// =============================================================================

struct ObjectHandle {
    RegionId   region_id;
    SlotId     slot_id;
    Generation generation;
}

struct RegionHandle {
    RegionId   region_id;
    Generation generation;
}

const ObjectHandle INVALID_OBJECT_HANDLE = { 0, 0, 0 };
const RegionHandle INVALID_REGION_HANDLE = { 0, 0 };

// =============================================================================
// SECTION 3: SPARSE SET
// =============================================================================

struct SparseSet {
    List{uint} key_position_index;
    List{uint} packed_keys;
    uint live_count;
}

fn bool SparseSet.contains(SparseSet* self, uint key) {
    if (key >= (uint)self.key_position_index.len()) return false;
    uint pos = self.key_position_index[(usz)key];
    return pos < self.live_count && self.packed_keys[(usz)pos] == key;
}

fn void SparseSet.insert(SparseSet* self, uint key) {
    if (self.contains(key)) return;
    while (self.key_position_index.len() <= (usz)key) {
        self.key_position_index.push(0);
    }
    if (self.packed_keys.len() <= (usz)self.live_count) {
        self.packed_keys.push(key);
    } else {
        self.packed_keys[(usz)self.live_count] = key;
    }
    self.key_position_index[(usz)key] = self.live_count;
    self.live_count += 1;
}

fn void SparseSet.remove(SparseSet* self, uint key) {
    if (!self.contains(key)) return;
    uint pos = self.key_position_index[(usz)key];
    uint last_key = self.packed_keys[(usz)(self.live_count - 1)];
    self.packed_keys[(usz)pos] = last_key;
    self.key_position_index[(usz)last_key] = pos;
    self.live_count -= 1;
}

fn void SparseSet.release(SparseSet* self) {
    self.key_position_index.free();
    self.packed_keys.free();
}

// =============================================================================
// SECTION 4: POOL (Object Storage)
// =============================================================================

const usz INLINE_THRESHOLD = 16;
const usz ARENA_SIZE = 64 * 1024;

struct PoolSlot {
    usz    size;
    typeid type_id;
    PoolId owner_id;
    bool   is_inline;
    void*  heap_ptr;
    char[16] inline_data @align(16);
}

struct ArenaBlock {
    char* data;
    usz   capacity;
    usz   used;
}

struct FreeListEntry {
    usz offset;
    usz size;
    usz arena_idx;
}

struct Pool {
    List{PoolSlot} packed_slots;
    uint slot_count;
    List{PoolIndex} pool_id_position_index;
    List{PoolId}    position_owner_index;
    List{PoolId} recycled_pool_ids;
    PoolId       next_pool_id;
    List{ArenaBlock} arenas;
    List{FreeListEntry} free_list;
}

fn void* Pool.arena_alloc(Pool* self, usz size, usz alignment = 8) {
    if (self.arenas.len() == 0) {
        char* data = (char*)mem::malloc(ARENA_SIZE);
        self.arenas.push({ .data = data, .capacity = ARENA_SIZE, .used = 0 });
    }
    ArenaBlock* current = &self.arenas[self.arenas.len() - 1];
    usz aligned_offset = (current.used + alignment - 1) & ~(alignment - 1);
    if (aligned_offset + size > current.capacity) {
        char* data = (char*)mem::malloc(ARENA_SIZE);
        self.arenas.push({ .data = data, .capacity = ARENA_SIZE, .used = 0 });
        current = &self.arenas[self.arenas.len() - 1];
        aligned_offset = (current.used + alignment - 1) & ~(alignment - 1);
    }
    void* ptr = current.data + aligned_offset;
    current.used = aligned_offset + size;
    return ptr;
}

fn PoolId Pool.allocate(Pool* self, void* source_data, usz size, typeid type_id) {
    PoolId pid;
    if (self.recycled_pool_ids.len() > 0) {
        pid = self.recycled_pool_ids.pop()!!;
    } else {
        pid = self.next_pool_id;
        self.next_pool_id = (PoolId)((uint)self.next_pool_id + 1);
        while (self.pool_id_position_index.len() <= (usz)(uint)pid) {
            self.pool_id_position_index.push((PoolIndex)0);
        }
    }
    PoolSlot slot = { .size = size, .type_id = type_id, .owner_id = pid };
    if (size <= INLINE_THRESHOLD) {
        slot.is_inline = true;
        mem::copy(&slot.inline_data[0], source_data, size);
    } else {
        slot.is_inline = false;
        void* arena_ptr = self.arena_alloc(size);
        mem::copy(arena_ptr, source_data, size);
        slot.heap_ptr = arena_ptr;
    }
    if (self.packed_slots.len() <= (usz)self.slot_count) {
        self.packed_slots.push(slot);
        self.position_owner_index.push(pid);
    } else {
        self.packed_slots[(usz)self.slot_count] = slot;
        self.position_owner_index[(usz)self.slot_count] = pid;
    }
    self.pool_id_position_index[(usz)(uint)pid] = (PoolIndex)self.slot_count;
    self.slot_count += 1;
    return pid;
}

fn void Pool.destroy_all(Pool* self) {
    foreach (&arena : self.arenas) mem::free(arena.data);
    self.packed_slots.free();
    self.pool_id_position_index.free();
    self.position_owner_index.free();
    self.recycled_pool_ids.free();
    self.arenas.free();
    self.free_list.free();
}

// =============================================================================
// SECTION 5: SLOT TABLE
// =============================================================================

struct ObjectLiveInfo {
    PoolId pool_id;
    usz    size;
    typeid type_id;
}

struct ObjectRecord {
    Generation generation;
    bool escaped;
    bool forwarded;
    union {
        ObjectLiveInfo live_info;
        ObjectHandle   forward_target;
    }
}

struct SlotTable {
    List{ObjectRecord} object_records;
    List{SlotId}       recycled_slot_ids;
    SparseSet          live_slot_tracker;
}

fn SlotAllocationResult SlotTable.allocate(SlotTable* self) {
    SlotId sid;
    if (self.recycled_slot_ids.len() > 0) {
        sid = self.recycled_slot_ids.pop()!!;
    } else {
        sid = (SlotId)self.object_records.len();
        self.object_records.push({});
    }
    usz idx = (usz)(uint)sid;
    self.object_records[idx].generation = (Generation)((uint)self.object_records[idx].generation + 1);
    self.object_records[idx].escaped = false;
    self.object_records[idx].forwarded = false;
    self.live_slot_tracker.insert((uint)sid);
    return { .slot_id = sid, .generation = self.object_records[idx].generation };
}

struct SlotAllocationResult {
    SlotId     slot_id;
    Generation generation;
}

fn void SlotTable.destroy_all(SlotTable* self) {
    self.object_records.free();
    self.recycled_slot_ids.free();
    self.live_slot_tracker.release();
}

// =============================================================================
// SECTION 7: REGION
// =============================================================================

struct Region {
    RegionId   id;
    Generation generation;
    Pool      pool;
    SlotTable slot_table;
}

macro ObjectHandle Region.allocate_typed(&self, $Type, value) {
    void* source_data = (void*)&value;
    usz size = $Type.sizeof;
    typeid tid = $Type.typeid;
    PoolId pid = self.pool.allocate(source_data, size, tid);
    SlotAllocationResult slot_result = self.slot_table.allocate();
    ObjectRecord* record = &self.slot_table.object_records[(usz)(uint)slot_result.slot_id];
    record.live_info.pool_id = pid;
    record.live_info.size = size;
    record.live_info.type_id = tid;
    return { .region_id = self.id, .slot_id = slot_result.slot_id, .generation = slot_result.generation };
}

fn void Region.destroy_internals(Region* self) {
    self.pool.destroy_all();
    self.slot_table.destroy_all();
}

// =============================================================================
// SECTION 9: REGION REGISTRY
// =============================================================================

struct RegionRegistry {
    List{Region} region_storage;
    List{bool}   region_alive_flags;
    List{Generation} region_generations;
    RegionId root_id;
}

fn void RegionRegistry.init(RegionRegistry* self) {
    self.region_storage.push({ .id = 0, .generation = 1 });
    self.region_alive_flags.push(true);
    self.region_generations.push(1);
    self.root_id = 0;
}

fn void* RegionRegistry.dereference(RegionRegistry* self, ObjectHandle handle) {
    usz idx = (usz)(uint)handle.region_id;
    Region* region = &self.region_storage[idx];
    ObjectRecord* record = &region.slot_table.object_records[(usz)(uint)handle.slot_id];
    assert((uint)record.generation == (uint)handle.generation, "stale handle");
    if (record.forwarded) return self.dereference(record.forward_target);
    PoolSlot* slot = &region.pool.packed_slots[(usz)(uint)region.pool.pool_id_position_index[(usz)(uint)record.live_info.pool_id]];
    return slot.is_inline ? (void*)&slot.inline_data[0] : slot.heap_ptr;
}

macro RegionRegistry.dereference_as(&self, $Type, handle) {
    return ($Type*)self.dereference(handle);
}

// =============================================================================
// SECTION 13: THREAD-LOCAL REGISTRY
// =============================================================================

tlocal RegionRegistry* g_thread_registry;

fn void thread_registry_init() {
    g_thread_registry = (RegionRegistry*)mem::malloc(RegionRegistry.sizeof);
    *g_thread_registry = {};
    g_thread_registry.init();
}

fn void thread_registry_shutdown() {
    if (g_thread_registry == null) return;
    for (usz i = 0; i < g_thread_registry.region_storage.len(); i++) {
        if (g_thread_registry.region_alive_flags[i]) g_thread_registry.region_storage[i].destroy_internals();
    }
    g_thread_registry.region_storage.free();
    g_thread_registry.region_alive_flags.free();
    g_thread_registry.region_generations.free();
    mem::free(g_thread_registry);
    g_thread_registry = null;
}

fn RegionRegistry* thread_registry() @inline {
    return g_thread_registry;
}

fn void* dereference(ObjectHandle handle) @inline {
    return thread_registry().dereference(handle);
}

macro dereference_as($Type, handle) {
    return thread_registry().dereference_as($Type, handle);
}

macro ObjectHandle allocate_in(region_handle, $Type, value) {
    Region* region = &thread_registry().region_storage[(usz)(uint)region_handle.region_id];
    return region.allocate_typed($Type, value);
}

fn RegionHandle thread_root_region() @inline {
    return { .region_id = thread_registry().root_id, .generation = thread_registry().region_generations[(usz)(uint)thread_registry().root_id] };
}

fn bool str_eq(char* s, char[] target) {
    for (usz i = 0; i < target.len; i++) if (s[i] != target[i]) return false;
    return s[target.len] == 0;
}
