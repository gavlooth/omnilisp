module main;

import std::io;

const ulong STACK_CLONE_LOW56_MASK = (ulong.max >> 8);
const ulong STACK_CLONE_TOPBYTE_MASK = ~STACK_CLONE_LOW56_MASK;

struct StackCloneResidualSourceWords {
    usz raw_words;
    usz low56_only_words;
}

fn bool stack_clone_is_source_stack_ptr_raw(ulong value, usz src_lo, usz src_hi) @inline {
    usz addr = (usz)value;
    return addr >= src_lo && addr < src_hi;
}

fn bool stack_clone_is_source_stack_ptr_low56(ulong value, usz src_lo, usz src_hi) @inline {
    ulong low = value & STACK_CLONE_LOW56_MASK;
    ulong lo = (ulong)src_lo & STACK_CLONE_LOW56_MASK;
    ulong hi = (ulong)src_hi & STACK_CLONE_LOW56_MASK;
    return low >= lo && low < hi;
}

fn bool stack_clone_signed_address_delta(usz dst, usz src, long* out_delta) @inline {
    if (out_delta == null) return false;
    *out_delta = 0;

    if (dst >= src) {
        usz diff = dst - src;
        if (diff > (usz)long.max) return false;
        *out_delta = (long)diff;
        return true;
    }

    usz diff = src - dst;
    if (diff > (usz)long.max) return false;
    *out_delta = -(long)diff;
    return true;
}

fn ulong stack_clone_relocate_ptr_if_needed(ulong value, usz src_lo, usz src_hi, long delta) @inline {
    if (stack_clone_is_source_stack_ptr_raw(value, src_lo, src_hi)) {
        return (ulong)((long)value + delta);
    }
    if (!stack_clone_is_source_stack_ptr_low56(value, src_lo, src_hi)) return value;

    // AArch64 may carry non-address metadata in the top byte. Relocate using
    // low 56 bits and preserve the tagged top byte unchanged.
    ulong top = value & STACK_CLONE_TOPBYTE_MASK;
    ulong low = value & STACK_CLONE_LOW56_MASK;
    ulong relocated_low = (ulong)((long)low + delta) & STACK_CLONE_LOW56_MASK;
    return top | relocated_low;
}

fn usz stack_clone_relocate_ctx_field(ulong* field, usz src_lo, usz src_hi, long delta) @inline {
    ulong before = *field;
    ulong after = stack_clone_relocate_ptr_if_needed(before, src_lo, src_hi, delta);
    *field = after;
    return after != before ? 1 : 0;
}

fn usz stack_clone_relocate_ctx_stack_pointers(StackContext* ctx, usz src_lo, usz src_hi, long delta) {
    usz relocated = 0;

    // x86_64 save set
    relocated += stack_clone_relocate_ctx_field(&ctx.rbx, src_lo, src_hi, delta);
    relocated += stack_clone_relocate_ctx_field(&ctx.rbp, src_lo, src_hi, delta);
    relocated += stack_clone_relocate_ctx_field(&ctx.r12, src_lo, src_hi, delta);
    relocated += stack_clone_relocate_ctx_field(&ctx.r13, src_lo, src_hi, delta);
    relocated += stack_clone_relocate_ctx_field(&ctx.r14, src_lo, src_hi, delta);
    relocated += stack_clone_relocate_ctx_field(&ctx.r15, src_lo, src_hi, delta);
    relocated += stack_clone_relocate_ctx_field(&ctx.rsp, src_lo, src_hi, delta);

    // aarch64 save set tail (x24..x28 + platform register x18)
    relocated += stack_clone_relocate_ctx_field(&ctx.x24, src_lo, src_hi, delta);
    relocated += stack_clone_relocate_ctx_field(&ctx.x25, src_lo, src_hi, delta);
    relocated += stack_clone_relocate_ctx_field(&ctx.x26, src_lo, src_hi, delta);
    relocated += stack_clone_relocate_ctx_field(&ctx.x27, src_lo, src_hi, delta);
    relocated += stack_clone_relocate_ctx_field(&ctx.x28, src_lo, src_hi, delta);
    relocated += stack_clone_relocate_ctx_field(&ctx.x18, src_lo, src_hi, delta);

    return relocated;
}

fn usz stack_clone_relocate_active_stack_words(usz dst_rsp, usz active_bytes, usz src_lo, usz src_hi, long delta) {
    if (active_bytes < ulong.sizeof) return 0;

    usz word_count = active_bytes / ulong.sizeof;
    ulong* words = (ulong*)(ulong)dst_rsp;
    usz relocated = 0;
    for (usz i = 0; i < word_count; i++) {
        ulong before = words[i];
        ulong after = stack_clone_relocate_ptr_if_needed(before, src_lo, src_hi, delta);
        words[i] = after;
        if (after != before) relocated++;
    }
    return relocated;
}

fn StackCloneResidualSourceWords stack_clone_classify_residual_source_stack_words(usz dst_rsp, usz active_bytes, usz src_lo, usz src_hi) {
    StackCloneResidualSourceWords residual;
    residual.raw_words = 0;
    residual.low56_only_words = 0;
    if (active_bytes < ulong.sizeof) return residual;

    usz word_count = active_bytes / ulong.sizeof;
    ulong* words = (ulong*)(ulong)dst_rsp;
    for (usz i = 0; i < word_count; i++) {
        ulong value = words[i];
        if (stack_clone_is_source_stack_ptr_raw(value, src_lo, src_hi)) {
            residual.raw_words++;
        } else if (stack_clone_is_source_stack_ptr_low56(value, src_lo, src_hi)) {
            residual.low56_only_words++;
        }
    }
    return residual;
}

fn bool stack_clone_residual_source_stack_words_fail_closed(StackCloneResidualSourceWords residual) @inline {
    // Raw source-stack addresses are true clone-isolation hazards. Low-56-only
    // matches are classified separately because top-byte metadata or integer
    // payloads can share source-stack low bits after the relocation pass.
    return residual.raw_words > 0;
}

fn bool stack_clone_trace_enabled() @inline {
    ZString raw = getenv("OMNI_STACK_CLONE_TRACE");
    return raw != null && raw[0] != 0;
}

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
fn StackCtx* stack_ctx_clone(StackCtx* source, StackPool* pool) {
    stack_require_ctx_owner(source, "stack_ctx_clone_source");
    stack_require_pool_owner(pool, "stack_ctx_clone_pool");
    if (source.status != CTX_SUSPENDED) return null;
    if (source.stack.base == null || source.stack.stack_top == null || source.stack.usable_size == 0) return null;

    usz src_lo = (usz)source.stack.base + GUARD_SIZE;
    usz src_hi = (usz)source.stack.stack_top;
    usz src_rsp = (usz)source.ctx.rsp;
    if (src_rsp < src_lo || src_rsp > src_hi) return null;
    if ((usz)source.ctx.rbp != 0 && ((usz)source.ctx.rbp < src_lo || (usz)source.ctx.rbp > src_hi)) return null;

    StackCtx* clone = stack_ctx_create(pool);
    if (clone == null) return null;

    // Copy strategy:
    // - x86_64: keep active-segment clone [RSP, stack_top) for lower overhead.
    // - aarch64: clone full usable stack lane for correctness. JIT continuation
    //   resumes can rely on pre-suspend spill slots that may live below the
    //   instantaneous RSP at capture-time.
    bool full_stack_clone = stack_backend_kind() == STACK_BACKEND_AARCH64;
    usz copy_src = full_stack_clone ? src_lo : src_rsp;
    usz copy_bytes = full_stack_clone ? source.stack.usable_size : (src_hi - src_rsp);
    if (copy_bytes > clone.stack.usable_size) {
        stack_ctx_destroy(clone, pool);
        return null;
    }

    usz dst_hi = (usz)clone.stack.stack_top;
    usz dst_lo = (usz)clone.stack.base + GUARD_SIZE;
    usz copy_dst = full_stack_clone ? dst_lo : (dst_hi - copy_bytes);
    long delta = 0;
    if (!stack_clone_signed_address_delta(copy_dst, copy_src, &delta)) {
        stack_ctx_destroy(clone, pool);
        return null;
    }
    stack_raw_copy((void*)copy_dst, (void*)copy_src, copy_bytes);

    // Copy register context and relocate stack pointers into the cloned stack.
    clone.ctx = source.ctx;
    clone.status = CTX_SUSPENDED;
    clone.result = source.result;
    clone.parent_ctx = null;
    clone.parent_stack_ctx = null;
    clone.asan_fake_stack = null;
    clone.defer_count = 0;

    usz dst_rsp = (usz)((long)source.ctx.rsp + delta);
    usz relocated_ctx_fields = stack_clone_relocate_ctx_stack_pointers(&clone.ctx, src_lo, src_hi, delta);

    // Walk and fixup the RBP chain on the cloned stack.
    usz lo = dst_lo;
    usz hi = dst_hi;

    ulong* bp = (ulong*)(ulong)clone.ctx.rbp;
    for (int safety = 0; safety < 10000; safety++) {
        usz bp_addr = (usz)bp;
        if (bp_addr < lo || bp_addr >= hi || bp_addr == 0) break;

        ulong old_val = *bp;
        if (old_val == 0) break;  // End of chain

        if (old_val >= src_lo && old_val < src_hi) {
            *bp = (ulong)((long)old_val + delta);
        } else {
            break;
        }

        bp = (ulong*)*bp;
    }

    // Relocate stack-internal pointers saved in spilled locals/register saves.
    // These can survive suspension in active frames and must point at clone memory.
    usz scan_dst = full_stack_clone ? dst_lo : dst_rsp;
    usz scan_bytes = full_stack_clone ? clone.stack.usable_size : copy_bytes;
    usz relocated_stack_words = stack_clone_relocate_active_stack_words(scan_dst, scan_bytes, src_lo, src_hi, delta);
    StackCloneResidualSourceWords residual_source_words = stack_clone_classify_residual_source_stack_words(scan_dst, scan_bytes, src_lo, src_hi);

    if (stack_clone_residual_source_stack_words_fail_closed(residual_source_words)) {
        if (stack_clone_trace_enabled()) {
            io::printfn(
                "stack-clone: residual source-stack pointer validation failed raw=%d low56_false_positive=%d",
                (int)residual_source_words.raw_words,
                (int)residual_source_words.low56_only_words
            );
        }
        stack_ctx_destroy(clone, pool);
        return null;
    }

    if (stack_clone_trace_enabled()) {
        io::printfn(
            "stack-clone: full=%d src=[%p,%p) dst=[%p,%p) src_rsp=%p dst_rsp=%p delta=%d ctx_reloc=%d word_reloc=%d word_src_raw=%d word_low56_false_positive=%d src_rbp=%p clone_rbp=%p",
            full_stack_clone ? 1 : 0,
            (void*)src_lo,
            (void*)src_hi,
            (void*)dst_lo,
            clone.stack.stack_top,
            (void*)src_rsp,
            (void*)dst_rsp,
            (int)delta,
            (int)relocated_ctx_fields,
            (int)relocated_stack_words,
            (int)residual_source_words.raw_words,
            (int)residual_source_words.low56_only_words,
            (void*)source.ctx.rbp,
            (void*)clone.ctx.rbp
        );
    }

    // Duplicate defer metadata and apply semantic clone hooks.
    if (source.defer_count > 0) {
        if (!stack_ctx_entry_clone_all(&source.defer_inline[0], source.defer_heap, source.defer_count, &clone.defer_inline[0], &clone.defer_heap, &clone.defer_capacity, STACK_CTX_DEFER_INLINE_CAP, true)) {
            stack_ctx_destroy(clone, pool);
            return null;
        }
        clone.defer_count = source.defer_count;
    }

    if (!stack_ctx_clone_lifecycle_entries(source, clone)) {
        stack_ctx_destroy(clone, pool);
        return null;
    }

    return clone;
}
