module lisp;

import std::io;
import main;

fn void run_memory_lifetime_boundary_commit_escape_allocation_guard_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: structured escape promotion must not republish a TEMP-lane
    // string alias when child string/error promotion runs out of memory.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* releasing_scope = main::scope_create(saved_scope);
        if (releasing_scope == null) {
            io::printn("[FAIL] lifetime: shared ARRAY escape promotion surfaces string alloc fault");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src_string = make_string(interp, "array string promotion oom");
            Value* src_array = interp.alloc_value();
            Array* src_payload = (Array*)mem::malloc(Array.sizeof);
            bool setup_ok = src_string != null &&
                            src_string.tag == STRING &&
                            src_array != null &&
                            src_payload != null;
            if (setup_ok) {
                src_array.tag = ARRAY;
                src_array.array_val = null;
                main::scope_register_dtor(releasing_scope, (void*)src_array, &scope_dtor_value);
                src_payload.capacity = 4;
                src_payload.length = 1;
                src_payload.items = (Value**)mem::malloc(Value*.sizeof * src_payload.capacity);
                setup_ok = src_payload.items != null;
                if (setup_ok) {
                    src_payload.items[0] = src_string;
                    src_array.array_val = src_payload;
                }
            }

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            g_boundary_force_escape_string_or_error_promotion_alloc_fail = true;
            Value* promoted = setup_ok ? promote_to_escape(src_array, interp) : null;
            g_boundary_force_escape_string_or_error_promotion_alloc_fail = false;
            boundary_promotion_context_end(interp, &ctx);

            bool ok = setup_ok &&
                      promoted != null &&
                      promoted.tag == ERROR &&
                      promoted.str_chars != null &&
                      str_starts_with(
                          promoted.str_chars[:promoted.str_len],
                          "boundary: failed to allocate string/error escape copy"
                      ) &&
                      src_array.array_val.items[0] == src_string &&
                      src_string.str_chars != null &&
                      interp.current_scope == releasing_scope;

            interp.current_scope = saved_scope;
            main::scope_release(releasing_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: shared ARRAY escape promotion surfaces string alloc fault");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: shared ARRAY escape promotion surfaces string alloc fault (setup_ok=%s tag=%d scope_ok=%s)",
                    setup_ok ? "yes" : "no",
                    promoted != null ? (int)promoted.tag : -1,
                    interp.current_scope == saved_scope ? "yes" : "no"
                );
                (*fail)++;
            }
        }
    }

    // Invariant: structured ARRAY escape promotion must not leak ESCAPE wrapper
    // slots when child promotion fails before publication commits.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* releasing_scope = main::scope_create(saved_scope);
        if (releasing_scope == null) {
            io::printn("[FAIL] lifetime: shared ARRAY escape child fault does not leak wrapper slots");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src_string = make_string(interp, "array wrapper slot leak");
            Value* src_array = interp.alloc_value();
            Array* src_payload = (Array*)mem::malloc(Array.sizeof);
            bool setup_ok = src_string != null &&
                            src_string.tag == STRING &&
                            src_array != null &&
                            src_payload != null;
            if (setup_ok) {
                src_array.tag = ARRAY;
                src_array.array_val = null;
                main::scope_register_dtor(releasing_scope, (void*)src_array, &scope_dtor_value);
                src_payload.capacity = 4;
                src_payload.length = 1;
                src_payload.items = (Value**)mem::malloc(Value*.sizeof * src_payload.capacity);
                setup_ok = src_payload.items != null;
                if (setup_ok) {
                    src_payload.items[0] = src_string;
                    src_array.array_val = src_payload;
                }
            }

            usz alloc_before = releasing_scope.alloc_count;
            g_boundary_force_escape_string_or_error_promotion_alloc_fail = true;

            PromotionContext baseline_ctx = {};
            boundary_promotion_context_begin(interp, &baseline_ctx, PROMOTION_CTX_BUDGET_RETURN);
            Value* baseline_error = setup_ok ? promote_to_escape(src_string, interp) : null;
            boundary_promotion_context_end(interp, &baseline_ctx);
            usz alloc_after_baseline = releasing_scope.alloc_count;

            PromotionContext first_ctx = {};
            boundary_promotion_context_begin(interp, &first_ctx, PROMOTION_CTX_BUDGET_RETURN);
            Value* first_promoted = setup_ok ? promote_to_escape(src_array, interp) : null;
            boundary_promotion_context_end(interp, &first_ctx);
            usz alloc_after_first = releasing_scope.alloc_count;

            PromotionContext second_ctx = {};
            boundary_promotion_context_begin(interp, &second_ctx, PROMOTION_CTX_BUDGET_RETURN);
            Value* second_promoted = setup_ok ? promote_to_escape(src_array, interp) : null;
            boundary_promotion_context_end(interp, &second_ctx);
            usz alloc_after_second = releasing_scope.alloc_count;

            g_boundary_force_escape_string_or_error_promotion_alloc_fail = false;

            usz baseline_delta = alloc_after_baseline - alloc_before;
            usz first_delta = alloc_after_first - alloc_after_baseline;
            usz second_delta = alloc_after_second - alloc_after_first;
            // The array path necessarily touches one extra source wrapper root
            // compared with direct string promotion, so one additional
            // PromotionContext memo node is expected per attempt. The invariant
            // here is that the delta stays stable across repeated failures,
            // proving no extra ESCAPE wrapper/payload slot is accumulating.
            bool ok = setup_ok &&
                      baseline_error != null &&
                      baseline_error.tag == ERROR &&
                      first_promoted != null &&
                      second_promoted != null &&
                      first_promoted.tag == ERROR &&
                      second_promoted.tag == ERROR &&
                      baseline_delta > 0 &&
                      str_starts_with(
                          first_promoted.str_chars[:first_promoted.str_len],
                          "boundary: failed to allocate string/error escape copy"
                      ) &&
                      str_starts_with(
                          second_promoted.str_chars[:second_promoted.str_len],
                          "boundary: failed to allocate string/error escape copy"
                      ) &&
                      first_delta == baseline_delta + 1 &&
                      second_delta == baseline_delta + 1 &&
                      interp.current_scope == releasing_scope;

            interp.current_scope = saved_scope;
            main::scope_release(releasing_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: shared ARRAY escape child fault does not leak wrapper slots");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: shared ARRAY escape child fault does not leak wrapper slots (setup_ok=%s baseline_tag=%d first_tag=%d second_tag=%d baseline_delta=%d first_delta=%d second_delta=%d scope_ok=%s)",
                    setup_ok ? "yes" : "no",
                    baseline_error != null ? (int)baseline_error.tag : -1,
                    first_promoted != null ? (int)first_promoted.tag : -1,
                    second_promoted != null ? (int)second_promoted.tag : -1,
                    (long)baseline_delta,
                    (long)first_delta,
                    (long)second_delta,
                    interp.current_scope == saved_scope ? "yes" : "no"
                );
                (*fail)++;
            }
        }
    }

    // Invariant: generic string constructor fails closed under forced allocation
    // failure without writing through null pointers.
    {
        g_value_force_make_string_alloc_fail = true;
        Value* made = make_string(interp, "constructor oom");
        g_value_force_make_string_alloc_fail = false;

        bool ok = made != null &&
                  made.tag == ERROR &&
                  made.str_chars != null &&
                  made.str_len > 0;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: make_string OOM fails closed");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: make_string OOM fails closed (tag=%d chars=%s len=%d)",
                made != null ? (int)made.tag : -1,
                made != null && made.str_chars != null ? "yes" : "no",
                made != null ? (int)made.str_len : -1);
            (*fail)++;
        }
    }

    // Invariant: generic error constructor fails closed under forced allocation
    // failure without writing through null pointers.
    {
        g_value_force_make_error_chars_alloc_fail_nth = 1;
        Value* made = make_error(interp, "constructor oom");
        g_value_force_make_error_chars_alloc_fail_nth = 0;

        bool ok = made != null &&
                  made.tag == ERROR &&
                  made.str_chars != null &&
                  made.str_len > 0;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: make_error OOM fails closed");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: make_error OOM fails closed (tag=%d chars=%s len=%d)",
                made != null ? (int)made.tag : -1,
                made != null && made.str_chars != null ? "yes" : "no",
                made != null ? (int)made.str_len : -1);
            (*fail)++;
        }
    }
}
