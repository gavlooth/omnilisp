module lisp;

const usz AST_ARENA_ALIGNMENT = 8;
const usz AST_ARENA_INITIAL_CHUNK_BYTES = 4096;

// AstArena is the interpreter-owned, chunked bump arena for AST nodes only.
// It owns Expr/Pattern storage for the full interp lifetime and only supports
// whole-arena teardown via ast_arena_destroy().
struct AstArenaChunk {
    AstArenaChunk* next;
    usz used;
    usz capacity;
}

struct AstArena {
    AstArenaChunk* chunks;
    usz count;
    usz capacity;
}

fn usz ast_arena_aligned_size(usz size) {
    usz mask = AST_ARENA_ALIGNMENT - 1;
    return (size + mask) & ~mask;
}

fn usz ast_arena_next_chunk_capacity(AstArena* arena, usz required) {
    if (arena == null) return required;

    usz capacity = AST_ARENA_INITIAL_CHUNK_BYTES;
    if (arena.chunks != null) {
        if (arena.chunks.capacity >= (usz.max / 2)) {
            capacity = required;
        } else {
            capacity = arena.chunks.capacity * 2;
        }
    }

    if (capacity < required) {
        capacity = required;
    }
    return capacity;
}

fn AstArenaChunk* ast_arena_chunk_alloc(usz capacity) {
    if (capacity == 0) return null;

    usz total = AstArenaChunk.sizeof + capacity;
    AstArenaChunk* chunk = (AstArenaChunk*)mem::malloc(total);
    if (chunk == null) return null;

    chunk.next = null;
    chunk.used = 0;
    chunk.capacity = capacity;
    return chunk;
}

fn char* ast_arena_chunk_data(AstArenaChunk* chunk) @inline {
    return (char*)chunk + AstArenaChunk.sizeof;
}

fn void ast_arena_init(AstArena* arena) {
    if (arena == null) return;
    arena.chunks = null;
    arena.count = 0;
    arena.capacity = 0;
}

fn void ast_arena_destroy(AstArena* arena) {
    if (arena == null) return;

    AstArenaChunk* current = arena.chunks;
    while (current != null) {
        AstArenaChunk* next = current.next;
        mem::free(current);
        current = next;
    }

    arena.chunks = null;
    arena.count = 0;
    arena.capacity = 0;
}

<*
@require arena != null : "ast_arena_validate: null arena"
*>
fn bool ast_arena_validate(AstArena* arena) {
    if (arena == null) return false;

    if (arena.chunks == null) {
        return arena.count == 0 && arena.capacity == 0;
    }

    if (arena.count == 0) return false;

    usz seen = 0;
    for (AstArenaChunk* chunk = arena.chunks; chunk != null; chunk = chunk.next) {
        if (chunk == null) return false;
        if (chunk.capacity < chunk.used) return false;
        seen++;
        if (chunk.next == chunk) return false;
        if (seen > arena.count) return false;
    }

    if (seen != arena.count) return false;
    return true;
}

fn bool ast_arena_contains(AstArena* arena, void* ptr) @inline {
    if (arena == null || ptr == null) return false;

    char* target = (char*)ptr;
    for (AstArenaChunk* chunk = arena.chunks; chunk != null; chunk = chunk.next) {
        if (chunk.used == 0) continue;

        char* start = ast_arena_chunk_data(chunk);
        char* end = start + chunk.used;
        if (start <= target && target < end) {
            return true;
        }
    }

    return false;
}

fn void ast_arena_zero(void* ptr, usz size) @inline {
    if (ptr == null || size == 0) return;
    char* bytes = (char*)ptr;
    for (usz i = 0; i < size; i++) {
        bytes[i] = 0;
    }
}

fn void* ast_arena_alloc(AstArena* arena, usz size) {
    if (arena == null || size == 0) return null;

    usz aligned = ast_arena_aligned_size(size);

    AstArenaChunk* current = arena.chunks;
    if (current != null && current.used + aligned <= current.capacity) {
        char* data = ast_arena_chunk_data(current);
        void* result = data + current.used;
        current.used += aligned;
        ast_arena_zero(result, size);
        return result;
    }

    usz chunk_capacity = ast_arena_next_chunk_capacity(arena, aligned);
    AstArenaChunk* chunk = ast_arena_chunk_alloc(chunk_capacity);
    if (chunk == null) return null;

    chunk.used = aligned;
    arena.capacity += chunk_capacity;
    arena.count++;
    chunk.next = arena.chunks;
    arena.chunks = chunk;

    void* result = ast_arena_chunk_data(chunk);
    ast_arena_zero(result, size);
    return result;
}
