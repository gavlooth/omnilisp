/**
 * =============================================================================
 * STACK ENGINE — Separate-stack execution contexts for delimited continuations
 * =============================================================================
 *
 * Provides the low-level primitives for running code on independently allocated
 * stacks and switching between them. This is the implementation mechanism for:
 *
 *   - Delimited continuations (reset/shift)
 *   - Algebraic effects (handle/signal/resolve)
 *   - User-facing coroutines (fiber/yield/resume)
 *
 * Semantic layering (what users see):
 *   continuations (reset/shift) ← the primitive
 *     ├── effects (handle/signal/resolve) ← tagged continuations
 *     └── coroutines (fiber/yield/resume) ← a usage pattern
 *
 * Implementation layering (this file):
 *   StackContext + stack_context_switch ← x86_64 assembly
 *   StackRegion (mmap + guard page)    ← stack allocation
 *   StackPool (free-list)              ← recycling
 *   Coro (coroutine handle)            ← lifecycle management
 *
 * Platform: x86_64 Linux only.
 */
module main;

import std::io;

// =============================================================================
// SECTION 1: PLATFORM CONSTANTS (Linux x86_64)
// =============================================================================

extern fn void* mmap(void* addr, usz length, int prot, int flags,
                     int fd, long offset) @extern("mmap");
extern fn int munmap(void* addr, usz length) @extern("munmap");
extern fn int mprotect(void* addr, usz length, int prot) @extern("mprotect");

const int PROT_NONE  = 0x0;
const int PROT_READ  = 0x1;
const int PROT_WRITE = 0x2;
const int MAP_PRIVATE   = 0x02;
const int MAP_ANONYMOUS = 0x20;
const usz PAGE_SIZE = 4096;

// mmap returns (void*)-1 on failure
const usz MAP_FAILED_VAL = ~(usz)0;

// =============================================================================
// C HELPERS (csrc/stack_helpers.c)
// =============================================================================
// D1: FPU state save/restore (stmxcsr/ldmxcsr/fnstcw/fldcw)
extern fn void fpu_save(uint* mxcsr, uint* x87cw) @extern("fpu_save");
extern fn void fpu_restore(uint mxcsr, uint x87cw) @extern("fpu_restore");
// D2: Stack overflow detection via SIGSEGV + sigaltstack
extern fn int stack_guard_init() @extern("stack_guard_init");
extern fn void stack_guard_shutdown() @extern("stack_guard_shutdown");
extern fn void stack_guard_register(void* base, usz size) @extern("stack_guard_register");
extern fn void stack_guard_unregister(void* base) @extern("stack_guard_unregister");
extern fn int stack_guard_protected_switch(void* old_ctx, void* new_ctx) @extern("stack_guard_protected_switch");

// =============================================================================
// SECTION 2: STACK CONTEXT (72 bytes — one cache line on most CPUs)
// =============================================================================
//
// Minimal register save area for cooperative context switching.
// Only callee-saved registers per the x86_64 System V ABI, plus RSP/RIP.
//

struct StackContext {
    ulong rbx;       // offset  0
    ulong rbp;       // offset  8
    ulong r12;       // offset 16
    ulong r13;       // offset 24
    ulong r14;       // offset 32
    ulong r15;       // offset 40
    ulong rsp;       // offset 48
    ulong rip;       // offset 56
    uint  mxcsr;     // offset 64: SSE control/status (callee-saved bits)
    uint  x87cw;     // offset 68: x87 FPU control word (callee-saved bits)
}
// Total: 72 bytes, 8-byte aligned

// =============================================================================
// SECTION 3: CONTEXT SWITCH — x86_64 assembly (~20 instructions)
// =============================================================================
//
// Symmetric context switch: saves current state into old_ctx, loads from new_ctx.
// This is the single most critical function in the stack engine.
//

/**
 * Switch execution context.
 *
 * Saves the caller's registers into old_ctx, then restores registers from
 * new_ctx and jumps to new_ctx.rip. From the caller's perspective, this
 * function "returns" when someone else switches back to old_ctx.
 *
 * @param old_ctx  Where to save current state (RDI)
 * @param new_ctx  State to restore and jump to (RSI)
 */
fn void stack_context_switch(StackContext* old_ctx, StackContext* new_ctx) @naked @extern("omni_context_switch") {
    asm {
        // ---- Save current context into old_ctx (RDI) ----

        // Callee-saved GPRs
        movq [$rdi],      $rbx;         // offset 0:  rbx
        movq [$rdi + 8],  $rbp;         // offset 8:  rbp
        movq [$rdi + 16], $r12;         // offset 16: r12
        movq [$rdi + 24], $r13;         // offset 24: r13
        movq [$rdi + 32], $r14;         // offset 32: r14
        movq [$rdi + 40], $r15;         // offset 40: r15

        // RSP: caller's stack pointer = current RSP + 8 (past return address)
        movq $rax, $rsp;
        addq $rax, 8;
        movq [$rdi + 48], $rax;         // offset 48: rsp

        // RIP: return address is at [RSP]
        movq $rax, [$rsp];
        movq [$rdi + 56], $rax;         // offset 56: rip

        // MXCSR/x87CW saved via fpu_save() in wrapper functions
        // (coro_switch_to, coro_resume, coro_suspend) before calling switch.

        // ---- Restore from new_ctx (RSI) ----

        // Callee-saved GPRs
        movq $rbx, [$rsi];              // rbx
        movq $rbp, [$rsi + 8];          // rbp
        movq $r12, [$rsi + 16];         // r12
        movq $r13, [$rsi + 24];         // r13
        movq $r14, [$rsi + 32];         // r14
        movq $r15, [$rsi + 40];         // r15

        // RSP
        movq $rsp, [$rsi + 48];

        // Jump to saved RIP via push+ret (compatible with existing context.c3 pattern)
        movq $rcx, [$rsi + 56];
        pushq $rcx;
        ret;
    }
}

// =============================================================================
// SECTION 4: COROUTINE STATUS
// =============================================================================

enum CoroStatus : char {
    CORO_READY,       // Created, not yet started
    CORO_RUNNING,     // Currently executing
    CORO_SUSPENDED,   // Yielded/shifted, can be resumed
    CORO_COMPLETED,   // Returned normally
    CORO_DEAD         // Error or invalidated
}

/**
 * Entry function signature for coroutines.
 */
alias CoroEntryFn = fn void(void*);

/**
 * Bootstrap state — tells the trampoline what function to call.
 */
struct CoroBootstrap {
    CoroEntryFn entry;
    void*       arg;
    Coro*       self;
}

// =============================================================================
// SECTION 5: STACK REGION (mmap + guard page)
// =============================================================================

struct StackRegion {
    void* base;        // mmap'd region start (includes guard page at bottom)
    usz   total_size;  // Total mmap'd size (guard + usable)
    usz   usable_size; // Usable stack size (total - guard page)
    void* stack_top;   // Top of usable stack (highest address, initial RSP)
}

const usz DEFAULT_STACK_SIZE = 65536;  // 64KB usable stack
const usz GUARD_SIZE = PAGE_SIZE;      // 4KB guard page

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
    StackRegion r;
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

    // Guard page at lowest address — crashes on stack overflow
    mprotect(mem, GUARD_SIZE, PROT_NONE);

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

// =============================================================================
// SECTION 6: COROUTINE HANDLE
// =============================================================================

/**
 * Coro — A coroutine: an execution context with its own stack.
 *
 * This is the runtime unit that backs delimited continuations, effect handlers,
 * and user-facing coroutines. The name "Coro" is internal — users see
 * "continuation" or "fiber" depending on the abstraction layer.
 */
struct Coro {
    StackContext    ctx;         // Saved register state
    StackRegion    stack;       // Owned stack memory
    CoroStatus     status;
    void*          result;      // Return value when COMPLETED
    StackContext*   parent_ctx;  // Where to switch back to on yield/complete
    uint           id;          // Unique identifier
    Coro*          pool_next;   // Free-list link for StackPool
    CoroBootstrap  boot;        // Bootstrap info for first switch
    void*          user_data;   // Application-level data (e.g. FiberThunkState*)
}

// =============================================================================
// SECTION 7: STACK POOL (free-list of recycled stacks)
// =============================================================================

struct StackPool {
    Coro*  free_list;   // Singly-linked list of available coros
    usz    pool_size;   // Current count in free list
    usz    max_pool;    // Max cached before freeing
    uint   next_id;     // Monotonic ID counter
    usz    stack_size;  // Default stack size for this pool
}

const usz STACK_POOL_MAX = 64;

fn void stack_pool_init(StackPool* pool) {
    pool.free_list = null;
    pool.pool_size = 0;
    pool.max_pool = STACK_POOL_MAX;
    pool.next_id = 1;
    pool.stack_size = DEFAULT_STACK_SIZE;
    stack_guard_init();
}

fn void stack_pool_shutdown(StackPool* pool) {
    // Free all cached coros and unregister their guard pages
    Coro* c = pool.free_list;
    while (c != null) {
        Coro* next = c.pool_next;
        stack_guard_unregister(c.stack.base);
        stack_region_free(&c.stack);
        mem::free(c);
        c = next;
    }
    pool.free_list = null;
    pool.pool_size = 0;
    stack_guard_shutdown();
}

// =============================================================================
// SECTION 8: COROUTINE LIFECYCLE
// =============================================================================

/**
 * Create a coroutine. Pops from pool if available, else allocates fresh.
 */
fn Coro* coro_create(StackPool* pool) {
    Coro* c;

    if (pool.free_list != null) {
        // Recycle from pool
        c = pool.free_list;
        pool.free_list = c.pool_next;
        pool.pool_size--;
    } else {
        // Fresh allocation
        c = (Coro*)mem::malloc(Coro.sizeof);
        c.stack = stack_region_alloc(pool.stack_size);
        if (c.stack.base == null) {
            mem::free(c);
            return null;  // mmap failed
        }
        // Register guard page for stack overflow detection
        stack_guard_register(c.stack.base, GUARD_SIZE);
    }

    // Initialize state
    StackContext empty_ctx;
    for (usz i = 0; i < StackContext.sizeof; i++) {
        ((char*)&empty_ctx)[i] = 0;
    }
    c.ctx = empty_ctx;
    c.status = CORO_READY;
    c.result = null;
    c.parent_ctx = null;
    c.pool_next = null;
    c.id = pool.next_id;
    pool.next_id++;

    return c;
}

/**
 * Return a coroutine to the pool, or free it if pool is full.
 */
fn void coro_destroy(Coro* c, StackPool* pool) {
    if (pool.pool_size < pool.max_pool) {
        // Return to pool — keep the stack allocation
        c.status = CORO_DEAD;
        c.result = null;
        c.parent_ctx = null;
        c.pool_next = pool.free_list;
        pool.free_list = c;
        pool.pool_size++;
    } else {
        // Pool full — unregister guard page and free everything
        stack_guard_unregister(c.stack.base);
        stack_region_free(&c.stack);
        mem::free(c);
    }
}

// =============================================================================
// SECTION 9: COROUTINE INITIALIZATION (set up initial stack frame)
// =============================================================================

// Global bootstrap pointer — set before first context switch into a coro.
// Safe because coroutine switches are cooperative (single-threaded).
tlocal CoroBootstrap* g_coro_bootstrap;

/**
 * Trampoline that runs when a coroutine starts for the first time.
 *
 * This is a normal C3 function (not naked), so it can call arbitrary code.
 * It reads the bootstrap info from the thread-local, calls the entry function,
 * then marks the coroutine as completed and switches back to the parent.
 */
fn void coro_trampoline() {
    CoroBootstrap* boot = g_coro_bootstrap;
    CoroEntryFn entry = boot.entry;
    void* arg = boot.arg;

    // Set current coro from bootstrap (only valid for first switch)
    g_current_coro = boot.self;

    // Call the actual entry function
    entry(arg);

    // Entry returned — mark as completed and switch back to parent.
    // IMPORTANT: Re-read g_current_coro here (do NOT use the stale boot.self).
    // After coro_clone + coro_resume, g_current_coro points to the clone,
    // but local variables on the cloned stack still reference the original.
    Coro* self = g_current_coro;
    self.status = CORO_COMPLETED;
    g_current_coro = null;
    stack_context_switch(&self.ctx, self.parent_ctx);

    // Should never reach here
}

/**
 * Initialize a coroutine's context to start executing entry(arg).
 *
 * Sets up the stack so that the first context_switch into this coro
 * will jump to coro_trampoline, which reads the bootstrap info and
 * calls entry(arg).
 *
 * @param c     Coroutine to initialize
 * @param entry Entry function: fn void(void*)
 * @param arg   Argument passed to entry
 */
fn void coro_init(Coro* c, CoroEntryFn entry, void* arg) {
    // Start RSP at top of usable stack, 16-byte aligned
    usz sp = (usz)c.stack.stack_top;
    sp &= ~(usz)15;  // Align to 16

    // SysV ABI: RSP must be 16-byte aligned BEFORE a CALL instruction.
    // The trampoline is a normal function — when context_switch does
    // push+ret to enter it, that simulates a CALL (pushes 8-byte return addr).
    // So we need RSP % 16 == 0 at the push point, which means RSP % 16 == 8
    // after the push, which is what the function prologue expects.
    sp -= 8;

    c.ctx.rsp = sp;
    c.ctx.rip = (ulong)&coro_trampoline;
    c.ctx.rbp = 0;  // Clean frame pointer (no parent frame)
    c.ctx.rbx = 0;
    c.ctx.r12 = 0;
    c.ctx.r13 = 0;
    c.ctx.r14 = 0;
    c.ctx.r15 = 0;

    // Default FPU control values
    c.ctx.mxcsr = 0x1F80;  // All exceptions masked
    c.ctx.x87cw = 0x037F;  // Double precision, all masked

    // Store bootstrap info on the Coro itself (read by trampoline)
    c.boot.entry = entry;
    c.boot.arg = arg;
    c.boot.self = c;

    c.status = CORO_READY;
}

/**
 * Switch into a coroutine. Sets up bootstrap and parent, then context-switches.
 *
 * @param c          Coroutine to run
 * @param parent_ctx Where to save the caller's context
 */
fn void coro_switch_to(Coro* c, StackContext* parent_ctx) {
    c.parent_ctx = parent_ctx;

    // Set bootstrap for first switch (trampoline reads this)
    if (c.status == CORO_READY) {
        g_coro_bootstrap = &c.boot;
        c.status = CORO_RUNNING;
    }

    Coro* saved = g_current_coro;
    g_current_coro = c;

    // D1: Save parent FPU state before switch
    fpu_save(&parent_ctx.mxcsr, &parent_ctx.x87cw);

    // D2: Protected switch with stack overflow recovery
    int overflow = stack_guard_protected_switch(parent_ctx, &c.ctx);

    // D1: Restore parent FPU state (also after overflow — siglongjmp doesn't restore FPU)
    fpu_restore(parent_ctx.mxcsr, parent_ctx.x87cw);

    if (overflow != 0) {
        c.status = CORO_DEAD;
    }

    g_current_coro = saved;
}

// Thread-local: currently executing coroutine (null if on main stack)
tlocal Coro* g_current_coro;

/**
 * Suspend the current coroutine and switch back to its parent.
 * This is the primitive behind yield/shift/signal.
 */
fn void coro_suspend() {
    Coro* c = g_current_coro;
    assert(c != null, "coro_suspend: not inside a coroutine");
    c.status = CORO_SUSPENDED;
    // D1: Save coro FPU state before switch to parent
    fpu_save(&c.ctx.mxcsr, &c.ctx.x87cw);
    stack_context_switch(&c.ctx, c.parent_ctx);
    // Execution resumes here when parent calls coro_resume
    // D1: Restore coro FPU state on resume
    fpu_restore(c.ctx.mxcsr, c.ctx.x87cw);
}

/**
 * Resume a suspended coroutine. Updates tracking and switches.
 */
fn void coro_resume(Coro* c, StackContext* parent_ctx) {
    assert(c.status == CORO_SUSPENDED, "coro_resume: coro not suspended");
    c.parent_ctx = parent_ctx;
    c.status = CORO_RUNNING;
    Coro* saved = g_current_coro;
    g_current_coro = c;

    // D1: Save parent FPU state before switch
    fpu_save(&parent_ctx.mxcsr, &parent_ctx.x87cw);

    // D2: Protected switch with stack overflow recovery
    int overflow = stack_guard_protected_switch(parent_ctx, &c.ctx);

    // D1: Restore parent FPU state
    fpu_restore(parent_ctx.mxcsr, parent_ctx.x87cw);

    if (overflow != 0) {
        c.status = CORO_DEAD;
    }

    g_current_coro = saved;
}

// Also update coro_switch_to to track current coro
// (done inline below in the existing coro_switch_to — we add tracking)

// =============================================================================
// SECTION 10: STACK CLONING (for multi-shot continuations)
// =============================================================================

/**
 * Clone a suspended coroutine for multi-shot continuation invocation.
 *
 * Creates a new coroutine with an identical copy of the source's stack
 * contents and register state. Adjusts all frame pointers (RBP chain)
 * in the cloned stack to point into the new stack.
 *
 * @param source  Suspended coroutine to clone
 * @param pool    Stack pool for allocation
 * @return New coroutine (caller owns), or null on failure
 */
fn Coro* coro_clone(Coro* source, StackPool* pool) {
    if (source.status != CORO_SUSPENDED) return null;

    Coro* clone = coro_create(pool);
    if (clone == null) return null;

    // Ensure clone has same-sized stack
    if (clone.stack.usable_size < source.stack.usable_size) {
        coro_destroy(clone, pool);
        return null;
    }

    // Copy entire usable stack contents
    char* src_usable = (char*)((usz)source.stack.base + GUARD_SIZE);
    char* dst_usable = (char*)((usz)clone.stack.base + GUARD_SIZE);
    usz usable = source.stack.usable_size;

    for (usz i = 0; i < usable; i++) {
        dst_usable[i] = src_usable[i];
    }

    // Copy register context
    clone.ctx = source.ctx;
    clone.status = CORO_SUSPENDED;
    clone.result = source.result;

    // Compute relocation delta: difference between stack bases
    long delta = (long)((usz)clone.stack.base - (usz)source.stack.base);

    // Adjust RSP and RBP in the cloned context
    clone.ctx.rsp = (ulong)((long)source.ctx.rsp + delta);
    clone.ctx.rbp = (ulong)((long)source.ctx.rbp + delta);

    // Walk and fixup the RBP chain on the cloned stack
    // Each frame's saved RBP points to the previous frame — adjust all
    usz lo = (usz)clone.stack.base + GUARD_SIZE;
    usz hi = (usz)clone.stack.stack_top;

    ulong* bp = (ulong*)(ulong)clone.ctx.rbp;
    for (int safety = 0; safety < 10000; safety++) {
        usz bp_addr = (usz)bp;
        if (bp_addr < lo || bp_addr >= hi || bp_addr == 0) break;

        ulong old_val = *bp;
        if (old_val == 0) break;  // End of chain

        // Only fixup if the saved RBP points into the source stack
        usz src_lo = (usz)source.stack.base + GUARD_SIZE;
        usz src_hi = (usz)source.stack.stack_top;
        if (old_val >= src_lo && old_val < src_hi) {
            *bp = (ulong)((long)old_val + delta);
        } else {
            break;  // RBP points outside source stack — end of chain
        }

        bp = (ulong*)*bp;  // Follow chain
    }

    return clone;
}

// =============================================================================
// SECTION 11: TESTS
// =============================================================================

// --- Test helpers ---

struct TestState {
    int value;
    int step;
}

fn void test_entry_simple(void* arg) {
    TestState* s = (TestState*)arg;
    s.value = 42;
}

fn void test_entry_100(void* arg) {
    TestState* s = (TestState*)arg;
    s.value = 100;
}

// Generator-style entry: yields 3 values then completes
fn void test_entry_generator(void* arg) {
    TestState* s = (TestState*)arg;

    s.value = 10;
    s.step = 1;
    coro_suspend();  // yield 10

    s.value = 20;
    s.step = 2;
    coro_suspend();  // yield 20

    s.value = 30;
    s.step = 3;
    // returns — trampoline marks COMPLETED
}

// Deeply recursive entry — tests stack depth on separate stack
fn void test_entry_recursive(void* arg) {
    TestState* s = (TestState*)arg;
    s.value = test_recursive_sum(100);
}

fn int test_recursive_sum(int n) {
    if (n <= 0) return 0;
    return n + test_recursive_sum(n - 1);
}

// --- Tests ---

fn bool test_stack_region() {
    StackRegion r = stack_region_alloc(DEFAULT_STACK_SIZE);
    if (r.base == null) return false;

    bool ok = r.total_size == DEFAULT_STACK_SIZE + GUARD_SIZE;
    ok = ok && r.usable_size == DEFAULT_STACK_SIZE;
    ok = ok && r.stack_top == (void*)((usz)r.base + r.total_size);

    // Verify usable area is writable
    char* mid = (char*)((usz)r.base + GUARD_SIZE + DEFAULT_STACK_SIZE / 2);
    *mid = 0xAB;
    ok = ok && *mid == (char)0xAB;

    stack_region_free(&r);
    return ok;
}

fn bool test_pool_lifecycle() {
    StackPool pool;
    stack_pool_init(&pool);

    Coro* c1 = coro_create(&pool);
    if (c1 == null) return false;
    uint id1 = c1.id;

    coro_destroy(c1, &pool);
    if (pool.pool_size != 1) return false;

    Coro* c2 = coro_create(&pool);
    if (c2 == null) return false;
    if (pool.pool_size != 0) return false;

    bool same_ptr = (c2 == c1);
    bool new_id = (c2.id != id1);

    coro_destroy(c2, &pool);
    stack_pool_shutdown(&pool);
    return same_ptr && new_id;
}

fn bool test_basic_coro() {
    StackPool pool;
    stack_pool_init(&pool);

    Coro* c = coro_create(&pool);
    if (c == null) { stack_pool_shutdown(&pool); return false; }

    TestState state;
    state.value = 0;

    coro_init(c, &test_entry_simple, &state);

    // Switch into the coro — it runs test_entry_simple, sets value=42,
    // returns, trampoline marks COMPLETED and switches back
    StackContext main_ctx;
    coro_switch_to(c, &main_ctx);

    bool ok = (c.status == CORO_COMPLETED) && (state.value == 42);

    coro_destroy(c, &pool);
    stack_pool_shutdown(&pool);
    return ok;
}

fn bool test_coro_second() {
    StackPool pool;
    stack_pool_init(&pool);

    Coro* c = coro_create(&pool);
    if (c == null) { stack_pool_shutdown(&pool); return false; }

    TestState state;
    state.value = 0;

    coro_init(c, &test_entry_100, &state);

    StackContext main_ctx;
    coro_switch_to(c, &main_ctx);

    bool ok = (c.status == CORO_COMPLETED) && (state.value == 100);

    coro_destroy(c, &pool);
    stack_pool_shutdown(&pool);
    return ok;
}

// THE critical test: suspend/resume (generator pattern)
fn bool test_suspend_resume() {
    StackPool pool;
    stack_pool_init(&pool);

    Coro* c = coro_create(&pool);
    if (c == null) { stack_pool_shutdown(&pool); return false; }

    TestState state;
    state.value = 0;
    state.step = 0;

    coro_init(c, &test_entry_generator, &state);

    // First switch: runs until first coro_suspend()
    StackContext main_ctx;
    coro_switch_to(c, &main_ctx);

    bool ok = (c.status == CORO_SUSPENDED) && (state.value == 10) && (state.step == 1);

    // Resume: runs until second coro_suspend()
    coro_resume(c, &main_ctx);

    ok = ok && (c.status == CORO_SUSPENDED) && (state.value == 20) && (state.step == 2);

    // Resume again: runs to completion
    coro_resume(c, &main_ctx);

    ok = ok && (c.status == CORO_COMPLETED) && (state.value == 30) && (state.step == 3);

    coro_destroy(c, &pool);
    stack_pool_shutdown(&pool);
    return ok;
}

// Test that recursive code works on separate stack
fn bool test_coro_recursion() {
    StackPool pool;
    stack_pool_init(&pool);

    Coro* c = coro_create(&pool);
    if (c == null) { stack_pool_shutdown(&pool); return false; }

    TestState state;
    state.value = 0;

    coro_init(c, &test_entry_recursive, &state);

    StackContext main_ctx;
    coro_switch_to(c, &main_ctx);

    // sum(1..100) = 5050
    bool ok = (c.status == CORO_COMPLETED) && (state.value == 5050);

    coro_destroy(c, &pool);
    stack_pool_shutdown(&pool);
    return ok;
}

// Test two coroutines interleaving
fn bool test_two_coros_interleaved() {
    StackPool pool;
    stack_pool_init(&pool);

    Coro* c1 = coro_create(&pool);
    Coro* c2 = coro_create(&pool);
    if (c1 == null || c2 == null) { stack_pool_shutdown(&pool); return false; }

    TestState s1;
    TestState s2;
    s1.value = 0; s1.step = 0;
    s2.value = 0; s2.step = 0;

    coro_init(c1, &test_entry_generator, &s1);
    coro_init(c2, &test_entry_generator, &s2);

    StackContext main_ctx;

    // Run c1 to first yield
    coro_switch_to(c1, &main_ctx);
    bool ok = (s1.value == 10) && (c1.status == CORO_SUSPENDED);

    // Run c2 to first yield
    coro_switch_to(c2, &main_ctx);
    ok = ok && (s2.value == 10) && (c2.status == CORO_SUSPENDED);

    // Resume c1 to second yield
    coro_resume(c1, &main_ctx);
    ok = ok && (s1.value == 20);

    // Resume c2 to completion (skip second yield, go to end)
    coro_resume(c2, &main_ctx);
    ok = ok && (s2.value == 20) && (c2.status == CORO_SUSPENDED);
    coro_resume(c2, &main_ctx);
    ok = ok && (s2.value == 30) && (c2.status == CORO_COMPLETED);

    // Resume c1 to completion
    coro_resume(c1, &main_ctx);
    ok = ok && (s1.value == 30) && (c1.status == CORO_COMPLETED);

    coro_destroy(c1, &pool);
    coro_destroy(c2, &pool);
    stack_pool_shutdown(&pool);
    return ok;
}

// Stack overflow entry — recurses until guard page hit
fn void test_entry_overflow(void* arg) {
    TestState* s = (TestState*)arg;
    test_overflow_recurse(s, 0);
}

fn int test_overflow_recurse(TestState* s, int depth) {
    s.value = depth;
    // 256-byte frame ensures we overflow the 64KB stack quickly (~250 frames)
    char[256] padding;
    padding[0] = (char)depth;
    return test_overflow_recurse(s, depth + 1) + (int)padding[0];
}

fn bool test_stack_overflow_recovery() {
    StackPool pool;
    stack_pool_init(&pool);

    Coro* c = coro_create(&pool);
    if (c == null) { stack_pool_shutdown(&pool); return false; }

    TestState state;
    state.value = 0;

    coro_init(c, &test_entry_overflow, &state);

    StackContext main_ctx;
    coro_switch_to(c, &main_ctx);

    // Should recover gracefully — coro marked DEAD, not crashed
    bool ok = (c.status == CORO_DEAD);
    // Should have recursed some before overflow
    ok = ok && (state.value > 10);

    coro_destroy(c, &pool);
    stack_pool_shutdown(&pool);
    return ok;
}

// Test that FPU state is preserved across coro switches
fn void test_entry_fpu(void* arg) {
    TestState* s = (TestState*)arg;
    // The FPU state should be default values on entry
    // Just set a marker to confirm we ran
    s.value = 99;
    coro_suspend();
    s.value = 100;
}

fn bool test_fpu_preservation() {
    StackPool pool;
    stack_pool_init(&pool);

    Coro* c = coro_create(&pool);
    if (c == null) { stack_pool_shutdown(&pool); return false; }

    TestState state;
    state.value = 0;
    coro_init(c, &test_entry_fpu, &state);

    // Save current FPU state
    uint mxcsr_before;
    uint x87cw_before;
    fpu_save(&mxcsr_before, &x87cw_before);

    StackContext main_ctx;
    coro_switch_to(c, &main_ctx);  // runs until suspend
    bool ok = (state.value == 99) && (c.status == CORO_SUSPENDED);

    // Verify parent FPU state preserved after switch
    uint mxcsr_after;
    uint x87cw_after;
    fpu_save(&mxcsr_after, &x87cw_after);
    ok = ok && (mxcsr_before == mxcsr_after);
    ok = ok && (x87cw_before == x87cw_after);

    // Resume and complete
    coro_resume(c, &main_ctx);
    ok = ok && (state.value == 100) && (c.status == CORO_COMPLETED);

    // FPU still preserved
    fpu_save(&mxcsr_after, &x87cw_after);
    ok = ok && (mxcsr_before == mxcsr_after);
    ok = ok && (x87cw_before == x87cw_after);

    coro_destroy(c, &pool);
    stack_pool_shutdown(&pool);
    return ok;
}

/**
 * Run all stack engine tests.
 */
fn void run_stack_engine_tests() {
    io::printfn("\n========================================");
    io::printfn("STACK ENGINE TESTS");
    io::printfn("========================================\n");

    int pass = 0;
    int fail = 0;

    if (test_stack_region()) { io::printfn("  PASS: stack region alloc/free"); pass++; }
    else { io::printfn("  FAIL: stack region alloc/free"); fail++; }

    if (test_pool_lifecycle()) { io::printfn("  PASS: pool create/recycle"); pass++; }
    else { io::printfn("  FAIL: pool create/recycle"); fail++; }

    if (test_basic_coro()) { io::printfn("  PASS: basic coro run + complete"); pass++; }
    else { io::printfn("  FAIL: basic coro run + complete"); fail++; }

    if (test_coro_second()) { io::printfn("  PASS: second coro entry"); pass++; }
    else { io::printfn("  FAIL: second coro entry"); fail++; }

    if (test_suspend_resume()) { io::printfn("  PASS: suspend/resume (generator)"); pass++; }
    else { io::printfn("  FAIL: suspend/resume (generator)"); fail++; }

    if (test_coro_recursion()) { io::printfn("  PASS: recursion on separate stack"); pass++; }
    else { io::printfn("  FAIL: recursion on separate stack"); fail++; }

    if (test_two_coros_interleaved()) { io::printfn("  PASS: two coros interleaved"); pass++; }
    else { io::printfn("  FAIL: two coros interleaved"); fail++; }

    if (test_fpu_preservation()) { io::printfn("  PASS: FPU state preserved across switch"); pass++; }
    else { io::printfn("  FAIL: FPU state preserved across switch"); fail++; }

    if (test_stack_overflow_recovery()) { io::printfn("  PASS: stack overflow recovery"); pass++; }
    else { io::printfn("  FAIL: stack overflow recovery"); fail++; }

    io::printfn("\nStack engine: %d passed, %d failed", pass, fail);
    io::printfn("========================================\n");
}
