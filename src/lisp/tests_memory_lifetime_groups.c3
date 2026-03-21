module lisp;


import std::io;
import main;

fn void run_memory_lifetime_cons_barrier_fallback_gate_test(Interp* interp, int* pass, int* fail) {
    // Invariant: long list return promotion may use the destination-cons route,
    // but it must not detour through generic fallback copying.
    {
        usz base_total = copy_to_parent_total_calls();
        usz base_generic = copy_to_parent_site_calls(COPY_SITE_GENERIC);
        usz base_car = copy_to_parent_site_calls(COPY_SITE_CONS_BARRIER_CAR);
        usz base_cdr = copy_to_parent_site_calls(COPY_SITE_CONS_BARRIER_CDR);

        EvalResult r = run(
            "(let ^rec (loop (lambda (n acc) (if (= n 2000) acc (loop (+ n 1) (cons n acc))))) (length (loop 0 nil)))",
            interp);

        usz total_delta = copy_to_parent_total_calls() - base_total;
        usz generic_delta = copy_to_parent_site_calls(COPY_SITE_GENERIC) - base_generic;
        usz car_delta = copy_to_parent_site_calls(COPY_SITE_CONS_BARRIER_CAR) - base_car;
        usz cdr_delta = copy_to_parent_site_calls(COPY_SITE_CONS_BARRIER_CDR) - base_cdr;

        bool ok_eval = !r.error.has_error &&
                       r.value != null &&
                       r.value.tag == INT &&
                       r.value.int_val == 2000;
        bool ok_budget = generic_delta == 0 && car_delta > 0;
        bool ok = ok_eval && ok_budget;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: cons return avoids generic fallback");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: cons return avoids generic fallback (total_delta=%d generic_delta=%d car_delta=%d cdr_delta=%d tag=%d int=%d err=%s)",
                (long)total_delta,
                (long)generic_delta,
                (long)car_delta,
                (long)cdr_delta,
                r.value != null ? (int)r.value.tag : -1,
                (r.value != null && r.value.tag == INT) ? (int)r.value.int_val : -1,
                r.error.has_error ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_escape_disjoint_fallback_test(Interp* interp, int* pass, int* fail) {
    // Invariant: disjoint-scope values commit directly to destination ESCAPE
    // without detouring through generic copy fallback accounting.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);

        interp.current_scope = foreign;
        Value* src = make_cons(interp, make_int(interp, 33), make_nil(interp));

        interp.current_scope = target;
        usz base_site = copy_to_parent_site_calls(COPY_SITE_GENERIC);
        Value* escaped = promote_to_escape(src, interp);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_GENERIC) - base_site;

        bool pre_ok = escaped != null &&
                      escaped != src &&
                      escaped.tag == CONS &&
                      escaped.scope_gen == target.escape_generation &&
                      escaped.cons_val.car != null &&
                      escaped.cons_val.car.tag == INT &&
                      escaped.cons_val.car.int_val == 33 &&
                      site_delta == 0 &&
                      !main::is_in_scope((void*)escaped, foreign);

        main::scope_release(foreign);

        bool post_ok = escaped != null &&
                       escaped.tag == CONS &&
                       escaped.cons_val.car != null &&
                       escaped.cons_val.car.tag == INT &&
                       escaped.cons_val.car.int_val == 33;

        interp.current_scope = saved;
        main::scope_release(target);

        bool ok = pre_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: escape disjoint direct destination gate");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: escape disjoint direct destination gate (site_delta=%d pre_ok=%s post_ok=%s)",
                (long)site_delta,
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_resume_missing_yield_scope_gate_test(Interp* interp, int* pass, int* fail) {
    // Invariant: coroutine resume boundary requires explicit yield provenance.
    // Missing yield_scope must hard-error and must not re-enter copy fallback.
    {
        Value* saved_yield = interp.yield_value;
        main::ScopeRegion* saved_yield_scope = interp.yield_scope;
        defer {
            interp.yield_value = saved_yield;
            interp.yield_scope = saved_yield_scope;
        }

        usz base_site = copy_to_parent_site_calls(COPY_SITE_PRIM_RESUME);
        interp.yield_value = make_int(interp, 5150);
        interp.yield_scope = null;

        Value* result = prim_resume_yield_result(interp);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_PRIM_RESUME) - base_site;

        bool ok = result != null &&
                  result.tag == ERROR &&
                  str_starts_with(result.str_chars[:result.str_len], "resume: missing yield-scope provenance") &&
                  interp.yield_value == null &&
                  interp.yield_scope == null &&
                  site_delta == 0;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: resume missing yield provenance gate");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: resume missing yield provenance gate (site_delta=%d tag=%d state_yield=%s state_scope=%s)",
                (long)site_delta,
                result != null ? (int)result.tag : -1,
                interp.yield_value == null ? "cleared" : "dirty",
                interp.yield_scope == null ? "cleared" : "dirty");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_ast_arena_validation_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: AstArena is chunked bump storage with deterministic growth and stable teardown.
    {
        bool interp_available = interp != null;
        AstArena arena = {};
        ast_arena_init(&arena);
        bool init_ok = ast_arena_validate(&arena) && arena.chunks == null && arena.count == 0 && arena.capacity == 0;

        Expr* first_expr = (Expr*)ast_arena_alloc(&arena, Expr.sizeof);
        Expr* second_expr = (Expr*)ast_arena_alloc(&arena, Expr.sizeof);
        Pattern* first_pattern = (Pattern*)ast_arena_alloc(&arena, Pattern.sizeof);
        void* first_block = ast_arena_alloc(&arena, AST_ARENA_INITIAL_CHUNK_BYTES / 2);
        void* second_block = ast_arena_alloc(&arena, AST_ARENA_INITIAL_CHUNK_BYTES / 2);
        void* overflow_block = ast_arena_alloc(&arena, Expr.sizeof);

        bool alloc_ok = first_expr != null &&
                        second_expr != null &&
                        first_pattern != null &&
                        first_block != null &&
                        second_block != null &&
                        overflow_block != null &&
                        arena.count >= 2 &&
                        ((usz)first_expr % AST_ARENA_ALIGNMENT == 0) &&
                        ((usz)first_pattern % AST_ARENA_ALIGNMENT == 0) &&
                        ((usz)overflow_block % AST_ARENA_ALIGNMENT == 0) &&
                        ast_arena_contains(&arena, first_expr) &&
                        ast_arena_contains(&arena, first_pattern) &&
                        ast_arena_contains(&arena, first_block) &&
                        ast_arena_contains(&arena, second_block) &&
                        ast_arena_contains(&arena, overflow_block);

        bool overflow_gate_ok = true;
        AstArenaChunk* chunk = arena.chunks;
        if (chunk != null) {
            usz chunk_used_save = chunk.used;
            chunk.used = chunk.capacity + 1;
            overflow_gate_ok = !ast_arena_validate(&arena);
            chunk.used = chunk_used_save;
        } else {
            overflow_gate_ok = false;
        }

        usz count_before_destroy = arena.count;
        usz capacity_before_destroy = arena.capacity;
        ast_arena_destroy(&arena);
        bool destroy_ok = ast_arena_validate(&arena) &&
                          arena.chunks == null &&
                          arena.count == 0 &&
                          arena.capacity == 0 &&
                          count_before_destroy > 0;

        ast_arena_init(&arena);
        usz expected_capacity = arena.capacity;
        Expr* reinit_expr = (Expr*)ast_arena_alloc(&arena, Expr.sizeof);
        bool reinit_ok = ast_arena_validate(&arena) &&
                         reinit_expr != null &&
                         ast_arena_contains(&arena, reinit_expr) &&
                         expected_capacity == 0;

        void* foreign = mem::malloc(16);
        bool contains_foreign_is_false = foreign != null &&
                                         !ast_arena_contains(&arena, foreign);
        if (foreign != null) mem::free(foreign);

        ast_arena_destroy(&arena);
        bool ok = interp_available &&
                  init_ok &&
                  alloc_ok &&
                  overflow_gate_ok &&
                  destroy_ok &&
                  reinit_ok &&
                  contains_foreign_is_false;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: ast_arena chunked allocator validation");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: ast_arena chunked allocator validation (init=%s alloc=%s overflow=%s destroy=%s reinit=%s contains=%s)",
                init_ok ? "yes" : "no",
                alloc_ok ? "yes" : "no",
                overflow_gate_ok ? "yes" : "no",
                destroy_ok ? "yes" : "no",
                reinit_ok ? "yes" : "no",
                contains_foreign_is_false ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_root_fallback_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_root_boundary_promotion_test(interp, pass, fail);
    run_memory_lifetime_root_boundary_direct_promotion_test(interp, pass, fail);
    run_memory_lifetime_root_boundary_array_clone_test(interp, pass, fail);
    run_memory_lifetime_root_boundary_hashmap_clone_test(interp, pass, fail);
    run_memory_lifetime_root_boundary_method_table_clone_test(interp, pass, fail);
    run_memory_lifetime_root_boundary_primitive_clone_test(interp, pass, fail);
    run_memory_lifetime_cons_barrier_fallback_gate_test(interp, pass, fail);
    run_memory_lifetime_escape_disjoint_fallback_test(interp, pass, fail);
    run_memory_lifetime_resume_missing_yield_scope_gate_test(interp, pass, fail);
}
