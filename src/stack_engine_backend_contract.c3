module main;

/**
 * Stack-engine backend contract.
 *
 * This file is the portability seam between stack-engine runtime flow and
 * backend ABI details. Runtime call sites should route through these helpers
 * instead of directly hard-coding backend details.
 */

enum StackBackendKind : char {
    STACK_BACKEND_X86_64,
    STACK_BACKEND_AARCH64,
    STACK_BACKEND_UNSUPPORTED,
}

extern fn int stack_backend_has_aarch64() @extern("stack_backend_has_aarch64");

fn StackBackendKind stack_backend_kind() @inline @if(env::X86_64) {
    return STACK_BACKEND_X86_64;
}

fn StackBackendKind stack_backend_kind() @inline @if(!env::X86_64) {
    if (stack_backend_has_aarch64() != 0) return STACK_BACKEND_AARCH64;
    return STACK_BACKEND_UNSUPPORTED;
}

fn char[] stack_backend_name() @inline {
    switch (stack_backend_kind()) {
        case STACK_BACKEND_X86_64:
            return "x86_64-sysv";
        case STACK_BACKEND_AARCH64:
            return "aarch64-aapcs64";
        case STACK_BACKEND_UNSUPPORTED:
            return "unsupported";
    }
}

fn bool stack_backend_supports_switch() @inline {
    StackBackendKind kind = stack_backend_kind();
    return kind == STACK_BACKEND_X86_64 || kind == STACK_BACKEND_AARCH64;
}

fn void stack_backend_init_context(StackContext* ctx, usz initial_rsp, ulong entry_rip) @inline @if(env::X86_64) {
    ctx.rsp = initial_rsp;
    ctx.rip = entry_rip;
    ctx.rbp = 0;
    ctx.rbx = 0;
    ctx.r12 = 0;
    ctx.r13 = 0;
    ctx.r14 = 0;
    ctx.r15 = 0;
    ctx.mxcsr = 0x1F80;  // SSE control/status: all exceptions masked
    ctx.x87cw = 0x037F;  // x87 control word: double precision, all masked
}

fn void stack_backend_init_context(StackContext* ctx, usz initial_rsp, ulong entry_rip) @inline @if(!env::X86_64) {
    if (stack_backend_kind() != STACK_BACKEND_AARCH64) {
        abort("stack engine context switching backend is unsupported on this target");
    }

    // Seed callee-saved register/control state from the creating thread so
    // first entry on a switched stack preserves backend runtime assumptions
    // (notably JIT state currently carried in callee-saved registers).
    stack_seed_context_nonx86(ctx);

    // stack_ctx_init keeps x86 push/ret-compatible bookkeeping (sp -= 8).
    // For aarch64 direct BR entry, undo that offset to keep SP 16-byte aligned.
    ctx.rsp = initial_rsp + 8;
    ctx.rip = entry_rip;
    // First-entry LR should remain sentinel-zero because trampoline never
    // returns to a caller frame on the fresh coroutine stack.
    ctx.lr = 0;

    // Legacy x86 FP slots are unused on this backend.
    ctx.mxcsr = 0;
    ctx.x87cw = 0;
}

fn void stack_backend_save_fp_state(StackContext* ctx) @inline @if(env::X86_64) {
    fpu_save(&ctx.mxcsr, &ctx.x87cw);
}

fn void stack_backend_save_fp_state(StackContext* ctx) @inline @if(!env::X86_64) {
    // Unsupported backend path stays fail-closed at switch time.
    // Keep FP helpers as no-ops so non-x86 builds do not depend on x86 helper symbols.
    (void)ctx;
}

fn void stack_backend_restore_fp_state(StackContext* ctx) @inline @if(env::X86_64) {
    fpu_restore(ctx.mxcsr, ctx.x87cw);
}

fn void stack_backend_restore_fp_state(StackContext* ctx) @inline @if(!env::X86_64) {
    (void)ctx;
}

fn int stack_backend_protected_switch(StackContext* old_ctx, StackContext* new_ctx) @inline @if(env::X86_64) {
    return stack_guard_protected_switch(old_ctx, new_ctx);
}

fn int stack_backend_protected_switch(StackContext* old_ctx, StackContext* new_ctx) @inline @if(!env::X86_64) {
    if (stack_backend_kind() == STACK_BACKEND_AARCH64) {
        ZString no_guard = getenv("OMNI_STACK_NO_GUARD");
        if (no_guard != null && no_guard[0] != 0) {
            stack_context_switch(old_ctx, new_ctx);
            return 0;
        }
        return stack_guard_protected_switch(old_ctx, new_ctx);
    }
    (void)old_ctx;
    (void)new_ctx;
    abort("stack engine context switching backend is unsupported on this target");
}

fn void stack_backend_switch(StackContext* old_ctx, StackContext* new_ctx) @inline @if(env::X86_64) {
    stack_context_switch(old_ctx, new_ctx);
}

fn void stack_backend_switch(StackContext* old_ctx, StackContext* new_ctx) @inline @if(!env::X86_64) {
    if (stack_backend_kind() == STACK_BACKEND_AARCH64) {
        stack_context_switch(old_ctx, new_ctx);
        return;
    }
    (void)old_ctx;
    (void)new_ctx;
    abort("stack engine context switching backend is unsupported on this target");
}
