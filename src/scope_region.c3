/**
 * =============================================================================
 * SCOPE REGION — Lightweight bump-allocating scopes with reference counting
 * =============================================================================
 *
 * Replaces the heavyweight Region system for Value/Env allocation with
 * fine-grained scopes at let/loop/lambda boundaries.
 *
 * Key properties:
 * - Bump allocation: ~3 instructions per alloc (load, add, compare)
 * - Reference counting: scopes freed deterministically at known program points
 * - No cycles: closures form a lexical tree (child→parent, never reverse)
 * - Freelist recycling: ScopeRegion structs reused across scope lifetimes
 * - Destructor list: heap-backed values (strings, arrays, etc.) cleaned up on scope death
 *
 * Memory layout:
 *   ScopeRegion → ScopeChunk (linked list of bump chunks)
 *                 ScopeDtor  (linked list of destructors, bump-allocated in chunks)
 */
module main;

extern fn ZString scope_c_getenv(ZString name) @extern("getenv");

// =============================================================================
// Types
// =============================================================================

alias ScopeDestructorFn = fn void(void* ptr);

// =============================================================================
// Structs
// =============================================================================

/**
 * ScopeChunk — A contiguous block of memory for bump allocation.
 * Chunks form a singly-linked list (newest first). Data follows inline
 * after the struct fields.
 */
struct ScopeChunk {
    ScopeChunk* next;       // Previous chunk (linked list, newest→oldest)
    usz         capacity;   // Usable bytes in this chunk (after header)
}

/**
 * ScopeDtor — Destructor entry for heap-backed values.
 * Allocated within the scope's own chunks (freed automatically).
 */
struct ScopeDtor {
    ScopeDtor*       next;  // Linked list (LIFO — last registered runs first)
    void*            ptr;   // Object to destroy
    ScopeDestructorFn func; // Destructor function
}

/**
 * ScopeRegion — A lightweight, bump-allocating memory scope.
 *
 * Typical size: ~80 bytes (vs hundreds for the old Region).
 * Fast path allocation: load bump, add size, compare limit.
 */
struct ScopeRegion {
    // Identity & lifetime
    uint         generation;    // Incremented on recycle (for handle validation)
    uint         refcount;      // +1 per external reference (owner + captures)

    // Hierarchy
    ScopeRegion* parent;        // Lexical parent scope (retained via RC)

    // Bump allocator
    char*        bump;          // Current allocation pointer
    char*        limit;         // End of current chunk's data area
    ScopeChunk*  chunks;        // Linked list of chunks (head = current)

    // Escape lane bump allocator (return values / boundary survivors)
    char*        escape_bump;   // Current escape allocation pointer
    char*        escape_limit;  // End of current escape chunk's data area
    ScopeChunk*  escape_chunks; // Linked list of escape chunks (head = current)
    ScopeChunk*  escape_chunks_tail; // Tail of escape_chunks (oldest, next=null)

    // Cleanup
    ScopeDtor*   dtors;         // Destructor list (LIFO)
    ScopeDtor*   escape_dtors;  // Escape-lane destructors (LIFO)
    ScopeDtor*   escape_dtors_tail; // Tail of escape_dtors (oldest, next=null)
    usz          alloc_bytes;   // Total bytes allocated (for split decisions)
    usz          alloc_count;   // Object count (for stats)
    uint         escape_generation; // Unique stamp for ESCAPE-lane membership checks
    void*        owner_thread_token; // Thread-affinity token for runtime safety checks
    bool         fiber_temp_eligible; // True when scope may use fiber-temp TEMP backing

    // Recycling
    ScopeRegion* pool_next;     // Freelist link (only used when in freelist)
}

// ABI/interop guard: scope regions are retained and inspected across runtime
// boundary helpers. Update bindings and this assertion together on layout change.
$assert(ScopeRegion.sizeof == 144);

// =============================================================================
// Constants
// =============================================================================

const usz SCOPE_CHUNK_INITIAL   = 512;      // First chunk size (bytes)
const usz SCOPE_CHUNK_MAX       = 65536;    // Maximum chunk size (64KB)

fn bool scope_escape_chunk_tail_consistent(ScopeRegion* scope) @inline {
    if (scope == null) return true;
    if (scope.escape_chunks == null) return scope.escape_chunks_tail == null;
    if (scope.escape_chunks_tail == null) return false;
    return scope.escape_chunks_tail.next == null;
}

fn bool scope_escape_dtor_tail_consistent(ScopeRegion* scope) @inline {
    if (scope == null) return true;
    if (scope.escape_dtors == null) return scope.escape_dtors_tail == null;
    if (scope.escape_dtors_tail == null) return false;
    return scope.escape_dtors_tail.next == null;
}

fn void scope_assert_escape_tail_consistency(ScopeRegion* scope) @inline {
    if (scope == null) return;
    assert(scope_escape_chunk_tail_consistent(scope), "scope: escape chunk tail inconsistency");
    assert(scope_escape_dtor_tail_consistent(scope), "scope: escape dtor tail inconsistency");
}

// =============================================================================
// Unit tests
// =============================================================================

