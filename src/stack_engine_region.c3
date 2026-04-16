module main;

// =============================================================================
// SECTION 5: STACK REGION (mmap + guard page)
// =============================================================================

struct StackRegion {
    void* base;        // mmap'd region start (includes guard page at bottom)
    usz   total_size;  // Total mmap'd size (guard + usable)
    usz   usable_size; // Usable stack size (total - guard page)
    void* stack_top;   // Top of usable stack (highest address, initial RSP)
}

// Higher-order stdlib and lazy Tensor materialization inside handle/reset stack
// contexts exceed the original coroutine-only budget. Keep normal runtime
// stacks large enough for handled lazy map realization without tripping the
// guard before recoverable errors can unwind.
const usz DEFAULT_STACK_SIZE = 262144; // 256KB usable stack (normal builds)
// Sanitizer-instrumented recursive eval paths burn substantially more stack per
// frame than normal builds. Keep the ASAN coroutine/checkpoint stack budget
// large enough to validate deep recursion contracts without tripping on
// sanitizer frame inflation first.
const usz ASAN_STACK_SIZE = 524288;   // 512KB usable stack (ASAN-instrumented builds)
const usz GUARD_SIZE = PAGE_SIZE;     // 4KB guard page

const usz STACK_APPLY_MIN_HEADROOM = 24 * 1024;

fn bool stack_runtime_asan_enabled() @inline {
    $if env::ADDRESS_SANITIZER:
        return true;
    $else
        return stack_asan_enabled() != 0;
    $endif
}

fn usz stack_current_available_bytes() {
    void* sp = stack_current_sp();
    if (sp == null) return 0;

    if (g_current_stack_ctx != null && stack_region_contains(&g_current_stack_ctx.stack, sp)) {
        usz lo = (usz)g_current_stack_ctx.stack.base + GUARD_SIZE;
        usz cur = (usz)sp;
        return cur > lo ? cur - lo : 0;
    }

    return stack_current_thread_available_bytes_from_sp(sp);
}

fn bool stack_has_apply_headroom(usz required) @inline {
    return stack_current_available_bytes() > required;
}

/**
 * Allocate a stack region: guard page (PROT_NONE) + usable stack (RW).
 *
 * Memory layout (low → high addresses):
 *   [guard page: PROT_NONE, 4KB] [usable stack: RW, stack_size bytes]
 *                                                                    ^ stack_top
 *
 * Stack grows downward, so stack_top is the initial RSP.
 */
fn StackRegion stack_region_alloc(usz stack_size) {
    StackRegion r = {};
    usz total = stack_size + GUARD_SIZE;

    void* mem = mmap(null, total, PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    if ((usz)mem == MAP_FAILED_VAL) {
        r.base = null;
        r.total_size = 0;
        r.usable_size = 0;
        r.stack_top = null;
        return r;
    }

    // Guard page at lowest address — crashes on stack overflow.
    if (mprotect(mem, GUARD_SIZE, PROT_NONE) < 0) {
        munmap(mem, total);
        return r;
    }

    r.base = mem;
    r.total_size = total;
    r.usable_size = stack_size;
    r.stack_top = (void*)((usz)mem + total);
    return r;
}

/**
 * Free a stack region.
 */
fn void stack_region_free(StackRegion* r) {
    if (r.base != null) {
        munmap(r.base, r.total_size);
        r.base = null;
    }
}

/**
 * Check if a pointer falls within this stack region's usable area.
 */
fn bool stack_region_contains(StackRegion* r, void* ptr) {
    usz p = (usz)ptr;
    usz lo = (usz)r.base + GUARD_SIZE;
    usz hi = (usz)r.stack_top;
    return p >= lo && p < hi;
}
