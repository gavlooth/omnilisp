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
 *   - User-facing coroutines (coroutine/yield/resume)
 *
 * Semantic layering (what users see):
 *   continuations (reset/shift) ← the primitive
 *     ├── effects (handle/signal/resolve) ← tagged continuations
 *     └── coroutines (coroutine/yield/resume) ← a usage pattern
 *
 * Implementation layering (this file):
 *   StackContext + backend switch contract
 *   StackRegion (mmap + guard page)    ← stack allocation
 *   StackPool (free-list)              ← recycling
 *   StackCtx (execution context handle)     ← lifecycle management
 *
 * Backend status: x86_64 backend is implemented; other targets fail closed.
 */
module main;

// =============================================================================
// SECTION 9: COROUTINE INITIALIZATION (set up initial stack frame)
// =============================================================================

/**
 * Initialize a coroutine's context to start executing entry(arg).
 *
 * Sets up the stack so that the first context_switch into this context
 * will jump to stack_ctx_trampoline, which reads the bootstrap info and
 * calls entry(arg).
 *
 * @param c     Stack context to initialize
 * @param entry Entry function: fn void(void*)
 * @param arg   Argument passed to entry
 */
fn void stack_ctx_init(StackCtx* c, StackCtxEntryFn entry, void* arg) {
    stack_require_ctx_owner(c, "stack_ctx_init");
    // Start RSP at top of usable stack, 16-byte aligned
    usz sp = (usz)c.stack.stack_top;
    sp &= ~(usz)15;  // Align to 16

    // SysV ABI: RSP must be 16-byte aligned BEFORE a CALL instruction.
    // The trampoline is a normal function — when context_switch does
    // push+ret to enter it, that simulates a CALL (pushes 8-byte return addr).
    // So we need RSP % 16 == 0 at the push point, which means RSP % 16 == 8
    // after the push, which is what the function prologue expects.
    sp -= 8;

    stack_backend_init_context(&c.ctx, sp, (ulong)&stack_ctx_trampoline);

    // Store bootstrap info on the StackCtx itself (read by trampoline)
    c.boot.entry = entry;
    c.boot.arg = arg;
    c.boot.self = c;

    c.status = CTX_READY;
}

// =============================================================================
// SECTION 10: STACK CLONING (for multi-shot continuations)
// =============================================================================

// =============================================================================
// SECTION 11: TESTS
// =============================================================================

// --- Test helpers ---

