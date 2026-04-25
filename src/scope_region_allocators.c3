module main;

fn void* ScopeRegion.alloc_slow(ScopeRegion* self, usz aligned_size) {
    scope_guard_owner(self, "scope_alloc_slow");
    // Determine new chunk capacity: double the previous, or at least fit the request
    usz prev_cap = SCOPE_CHUNK_INITIAL;
    if (self.chunks != null) {
        prev_cap = self.chunks.capacity;
    }
    if (prev_cap > usz.max / 2) return null;
    usz new_cap = prev_cap * 2;
    if (new_cap > SCOPE_CHUNK_MAX) new_cap = SCOPE_CHUNK_MAX;
    if (new_cap < aligned_size) new_cap = aligned_size;

    bool size_eligible = fiber_temp_alloc_size_eligible(aligned_size);
    bool shape_eligible = fiber_temp_scope_shape_eligible(self);
    bool use_fiber_temp = self.fiber_temp_eligible && size_eligible && shape_eligible;
    if (use_fiber_temp) {
        g_fiber_temp_pool_stats.eligible_slow_allocs++;
    } else if (self.fiber_temp_eligible) {
        if (!size_eligible) {
            g_fiber_temp_pool_stats.bypass_large_allocs++;
        } else if (!shape_eligible) {
            g_fiber_temp_pool_stats.bypass_escape_activity_allocs++;
        }
    }

    ScopeChunk* chunk = use_fiber_temp
        ? scope_chunk_alloc_temp(new_cap)
        : scope_chunk_alloc_raw(new_cap);
    if (chunk == null) return null;
    scope_telemetry_note_temp_slow_sequence_close(self);
    scope_telemetry_note_temp_slow_alloc_request(aligned_size, chunk.capacity);
    scope_telemetry_note_temp_slow_sequence_start(self, aligned_size, chunk.capacity);
    scope_telemetry_note_temp_chunk_alloc(chunk);
    chunk.next = self.chunks;
    self.chunks = chunk;

    char* data = scope_chunk_data(chunk);
    self.bump = data + aligned_size;
    self.limit = data + chunk.capacity;
    self.alloc_bytes += aligned_size;
    self.alloc_count++;
    scope_telemetry_note_temp_alloc(aligned_size);
    return (void*)data;
}

/**
 * Escape slow path: allocate a new ESCAPE chunk when current escape chunk is full.
 */
fn void* ScopeRegion.alloc_escape_slow(ScopeRegion* self, usz aligned_size) {
    scope_guard_owner(self, "scope_alloc_escape_slow");
    scope_assert_escape_tail_consistency(self);
    usz prev_cap = SCOPE_CHUNK_INITIAL;
    if (self.escape_chunks != null) {
        prev_cap = self.escape_chunks.capacity;
    }
    if (prev_cap > usz.max / 2) return null;
    usz new_cap = prev_cap * 2;
    if (new_cap > SCOPE_CHUNK_MAX) new_cap = SCOPE_CHUNK_MAX;
    if (new_cap < aligned_size) new_cap = aligned_size;

    ScopeChunk* chunk = scope_chunk_alloc_raw(new_cap);
    if (chunk == null) return null;
    ScopeEscapeAllocSource source = self.escape_alloc_source == SCOPE_ESCAPE_ALLOC_UNKNOWN
        ? SCOPE_ESCAPE_ALLOC_DIRECT
        : self.escape_alloc_source;
    scope_telemetry_note_escape_slow_sequence_close(self);
    scope_telemetry_note_escape_slow_alloc_request(aligned_size, chunk.capacity);
    scope_telemetry_note_escape_slow_sequence_start(self, aligned_size, chunk.capacity, source);
    scope_telemetry_note_escape_chunk_alloc(chunk);
    ScopeChunk* previous_head = self.escape_chunks;
    chunk.next = previous_head;
    self.escape_chunks = chunk;
    if (previous_head == null) {
        self.escape_chunks_tail = chunk;
    }

    char* data = scope_chunk_data(chunk);
    self.escape_bump = data + aligned_size;
    self.escape_limit = data + chunk.capacity;
    self.alloc_bytes += aligned_size;
    self.alloc_count++;
    scope_telemetry_note_escape_alloc(aligned_size);
    scope_assert_escape_tail_consistency(self);
    return (void*)data;
}

/**
 * Bump-allocate `size` bytes from this scope. 8-byte aligned.
 * Fast path: ~3 instructions (load bump, add, compare).
 */
fn void* ScopeRegion.alloc(ScopeRegion* self, usz size) @inline {
    scope_guard_owner(self, "scope_alloc");
    usz mask = 7;
    if (size > usz.max - mask) return null;
    usz aligned = (size + mask) & ~mask;
    char* result = self.bump;
    usz result_addr = (usz)result;
    usz limit_addr = (usz)self.limit;
    if (result != null && result_addr <= limit_addr && aligned <= limit_addr - result_addr) {
        char* new_bump = result + aligned;
        self.bump = new_bump;
            self.alloc_bytes += aligned;
            self.alloc_count++;
            scope_telemetry_note_temp_sequence_followup(self, aligned);
            scope_telemetry_note_temp_alloc(aligned);
            return (void*)result;
    }
    return self.alloc_slow(aligned);
}

/**
 * Bump-allocate `size` bytes from ESCAPE lane. 8-byte aligned.
 */
fn void* ScopeRegion.alloc_escape(ScopeRegion* self, usz size) @inline {
    scope_guard_owner(self, "scope_alloc_escape");
    usz mask = 7;
    if (size > usz.max - mask) return null;
    usz aligned = (size + mask) & ~mask;
    if (self.escape_bump != null) {
        char* result = self.escape_bump;
        usz result_addr = (usz)result;
        usz limit_addr = (usz)self.escape_limit;
        if (result_addr <= limit_addr && aligned <= limit_addr - result_addr) {
            char* new_bump = result + aligned;
            self.escape_bump = new_bump;
            self.alloc_bytes += aligned;
            self.alloc_count++;
            scope_telemetry_note_escape_sequence_followup(self, aligned);
            scope_telemetry_note_escape_alloc(aligned);
            return (void*)result;
        }
    }
    return self.alloc_escape_slow(aligned);
}

fn void* ScopeRegion.alloc_escape_attributed(ScopeRegion* self, usz size, ScopeEscapeAllocSource source) @inline {
    scope_guard_owner(self, "scope_alloc_escape_attributed");
    ScopeEscapeAllocSource previous = self.escape_alloc_source;
    self.escape_alloc_source = source;
    void* result = self.alloc_escape(size);
    self.escape_alloc_source = previous;
    return result;
}
