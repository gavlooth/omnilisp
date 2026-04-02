module main;

// =============================================================================
// SECTION 1: PLATFORM CONSTANTS (Linux x86_64)
// =============================================================================

extern fn void* mmap(void* addr, usz length, int prot, int flags,
                     int fd, long offset) @extern("mmap");
extern fn int munmap(void* addr, usz length) @extern("munmap");
extern fn int mprotect(void* addr, usz length, int prot) @extern("mprotect");
extern fn ZString getenv(ZString name) @extern("getenv");

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
extern fn int stack_guard_register(void* base, usz size) @extern("stack_guard_register");
extern fn void stack_guard_unregister(void* base) @extern("stack_guard_unregister");
extern fn int stack_guard_protected_switch(void* old_ctx, void* new_ctx) @extern("stack_guard_protected_switch");
extern fn void stack_asan_start_switch(void** fake_stack_save, void* stack_bottom, usz stack_size) @extern("stack_asan_start_switch");
extern fn void stack_asan_finish_switch(void* fake_stack_save) @extern("stack_asan_finish_switch");
extern fn void stack_raw_copy(void* dst, void* src, usz size) @extern("stack_raw_copy");
extern fn int stack_asan_enabled() @extern("stack_asan_enabled");
extern fn void* stack_current_sp() @extern("stack_current_sp");
extern fn usz stack_current_thread_available_bytes() @extern("stack_current_thread_available_bytes");
extern fn usz stack_current_thread_available_bytes_from_sp(void* sp) @extern("stack_current_thread_available_bytes_from_sp");

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
        // (stack_ctx_switch_to, stack_ctx_resume, stack_ctx_suspend) before calling switch.

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
