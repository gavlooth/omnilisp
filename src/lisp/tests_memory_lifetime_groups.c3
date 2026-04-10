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

fn void run_memory_lifetime_escape_primitive_guard_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: disjoint-scope primitive wrappers crossing into ESCAPE must
    // clone plain primitive headers and reject opaque payload aliases.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: escape primitive plain clone path");
            (*fail)++;
        } else {
            interp.current_scope = foreign;
            Value* src = interp.alloc_value();
            src.tag = PRIMITIVE;
            main::scope_register_dtor(foreign, (void*)src, &scope_dtor_value);
            src.prim_val = null;

            Primitive* prim = (Primitive*)mem::malloc(Primitive.sizeof);
            if (prim == null) {
                interp.current_scope = saved;
                main::scope_release(foreign);
                main::scope_release(target);
                io::printn("[FAIL] lifetime: escape primitive plain clone path (setup primitive)");
                (*fail)++;
                return;
            }
            *prim = {};
            prim.name[0] = 'e';
            prim.name[1] = 0;
            prim.arity = 2;
            prim.tag = 451;
            src.prim_val = prim;

            interp.current_scope = target;
            usz base_site = copy_to_parent_site_calls(COPY_SITE_GENERIC);
            Value* promoted = promote_to_escape(src, interp);
            usz site_delta = copy_to_parent_site_calls(COPY_SITE_GENERIC) - base_site;

            bool pre_ok = promoted != null &&
                          promoted != src &&
                          promoted.tag == PRIMITIVE &&
                          promoted.scope_gen == target.escape_generation &&
                          promoted.prim_val != null &&
                          promoted.prim_val != src.prim_val &&
                          promoted.prim_val.arity == 2 &&
                          promoted.prim_val.tag == 451 &&
                          promoted.prim_val.name[0] == 'e' &&
                          promoted.prim_val.name[1] == 0 &&
                          site_delta == 0 &&
                          !main::is_in_scope((void*)promoted, foreign);

            main::scope_release(foreign);

            bool post_ok = promoted != null &&
                           promoted.tag == PRIMITIVE &&
                           promoted.prim_val != null &&
                           promoted.prim_val.arity == 2 &&
                           promoted.prim_val.tag == 451 &&
                           promoted.prim_val.name[0] == 'e' &&
                           promoted.prim_val.name[1] == 0;

            interp.current_scope = saved;
            main::scope_release(target);

            bool ok = pre_ok && post_ok;
            if (ok) {
                emit_pass_literal("[PASS] lifetime: escape primitive plain clone path");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: escape primitive plain clone path (site_delta=%d pre_ok=%s post_ok=%s same_payload=%s)",
                    (long)site_delta,
                    pre_ok ? "yes" : "no",
                    post_ok ? "yes" : "no",
                    (promoted != null && src != null && promoted.tag == PRIMITIVE && src.tag == PRIMITIVE && promoted.prim_val == src.prim_val) ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: escape primitive opaque payload reject");
            (*fail)++;
        } else {
            interp.current_scope = foreign;
            Value* src = interp.alloc_value();
            src.tag = PRIMITIVE;
            main::scope_register_dtor(foreign, (void*)src, &scope_dtor_value);
            src.prim_val = null;

            Primitive* prim = (Primitive*)mem::malloc(Primitive.sizeof);
            if (prim == null) {
                interp.current_scope = saved;
                main::scope_release(foreign);
                main::scope_release(target);
                io::printn("[FAIL] lifetime: escape primitive opaque payload reject (setup primitive)");
                (*fail)++;
                return;
            }
            *prim = {};
            prim.name[0] = 'o';
            prim.name[1] = 0;
            prim.arity = 1;
            prim.tag = 452;
            int* opaque_payload = (int*)mem::malloc(int.sizeof);
            if (opaque_payload == null) {
                mem::free(prim);
                interp.current_scope = saved;
                main::scope_release(foreign);
                main::scope_release(target);
                io::printn("[FAIL] lifetime: escape primitive opaque payload reject (setup payload)");
                (*fail)++;
                return;
            }
            *opaque_payload = 91;
            prim.user_data = (void*)opaque_payload;
            src.prim_val = prim;

            interp.current_scope = target;
            usz base_site = copy_to_parent_site_calls(COPY_SITE_GENERIC);
            Value* promoted = promote_to_escape(src, interp);
            usz site_delta = copy_to_parent_site_calls(COPY_SITE_GENERIC) - base_site;

            bool pre_ok = promoted != null &&
                          promoted.tag == ERROR &&
                          promoted.str_chars != null &&
                          str_contains(
                              promoted.str_chars[:promoted.str_len],
                              "boundary: refusing primitive escape promotion with opaque user_data"
                          ) &&
                          site_delta == 0;

            main::scope_release(foreign);

            bool post_ok = promoted != null &&
                           promoted.tag == ERROR &&
                           promoted.str_chars != null &&
                           str_contains(
                               promoted.str_chars[:promoted.str_len],
                               "boundary: refusing primitive escape promotion with opaque user_data"
                           );

            interp.current_scope = saved;
            main::scope_release(target);

            bool ok = pre_ok && post_ok;
            if (ok) {
                emit_pass_literal("[PASS] lifetime: escape primitive opaque payload reject");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: escape primitive opaque payload reject (site_delta=%d pre_ok=%s post_ok=%s)",
                    (long)site_delta,
                    pre_ok ? "yes" : "no",
                    post_ok ? "yes" : "no");
                (*fail)++;
            }
        }
    }
}

fn void run_memory_lifetime_escape_partial_transactional_cleanup_test(Interp* interp, int* pass, int* fail) {
    // Invariant: escape promotion rolls back already-promoted partial args when
    // a later arg promotion fails.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: escape partial transactional cleanup");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__escape_partial_txn_payload");

        interp.current_scope = foreign;
        Env* captured_env = make_env(interp, interp.global_env);
        main::ScopeRegion* closure_env_scope = null;
        Value* shared_closure = null;
        if (captured_env != null) {
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 612), interp);
            shared_closure = make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope);
        }

        Value* opaque = interp.alloc_value();
        Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        int* opaque_payload = (int*)mem::malloc(int.sizeof);
        bool opaque_ok = opaque != null && opaque_prim != null && opaque_payload != null;
        if (opaque_ok) {
            *opaque_prim = {};
            *opaque_payload = 612;
            opaque.tag = PRIMITIVE;
            main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
            opaque_prim.name[0] = 'e';
            opaque_prim.name[1] = 0;
            opaque_prim.arity = 2;
            opaque_prim.tag = 612;
            opaque_prim.user_data = (void*)opaque_payload;
            opaque.prim_val = opaque_prim;
        }

        Value* partial = interp.alloc_value();
        bool setup_ok = shared_closure != null &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1 &&
                        opaque_ok &&
                        partial != null;
        if (setup_ok) {
            partial.tag = PARTIAL_PRIM;
            partial.partial_val.func = &prim_add;
            partial.partial_val.remaining = 1;
            partial.partial_val.first_arg = shared_closure;
            partial.partial_val.second_arg = opaque;
        }

        interp.current_scope = target;
        Value* promoted = setup_ok ? promote_to_escape(partial, interp) : null;
        int closure_rc_after_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;

        interp.current_scope = saved;
        main::scope_release(foreign);
        main::scope_release(target);

        bool fault_ok = promoted != null && promoted.tag == ERROR;
        bool rollback_ok = closure_rc_after_fail == 1;
        bool ok = setup_ok && fault_ok && rollback_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: escape partial transactional cleanup");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: escape partial transactional cleanup (setup_ok=%s fault_ok=%s closure_rc=%d)",
                setup_ok ? "yes" : "no",
                fault_ok ? "yes" : "no",
                closure_rc_after_fail
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_escape_cons_transactional_cleanup_test(Interp* interp, int* pass, int* fail) {
    // Invariant: escape promotion rolls back already-promoted cons children
    // when a later cdr promotion fails.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: escape cons transactional cleanup");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__escape_cons_txn_payload");

        interp.current_scope = foreign;
        Env* captured_env = make_env(interp, interp.global_env);
        main::ScopeRegion* closure_env_scope = null;
        Value* shared_closure = null;
        if (captured_env != null) {
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 616), interp);
            shared_closure = make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope);
        }

        Value* opaque = interp.alloc_value();
        Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        int* opaque_payload = (int*)mem::malloc(int.sizeof);
        bool opaque_ok = opaque != null && opaque_prim != null && opaque_payload != null;
        if (opaque_ok) {
            *opaque_prim = {};
            *opaque_payload = 616;
            opaque.tag = PRIMITIVE;
            main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
            opaque_prim.name[0] = 'g';
            opaque_prim.name[1] = 0;
            opaque_prim.arity = 2;
            opaque_prim.tag = 616;
            opaque_prim.user_data = (void*)opaque_payload;
            opaque.prim_val = opaque_prim;
        }

        Value* cons = make_cons(interp, shared_closure, opaque);
        bool setup_ok = shared_closure != null &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1 &&
                        opaque_ok &&
                        cons != null &&
                        cons.tag == CONS;

        interp.current_scope = target;
        Value* promoted = setup_ok ? promote_to_escape(cons, interp) : null;
        int closure_rc_after_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;

        interp.current_scope = saved;
        main::scope_release(foreign);
        main::scope_release(target);

        bool fault_ok = promoted != null && promoted.tag == ERROR;
        bool rollback_ok = closure_rc_after_fail == 1;
        bool ok = setup_ok && fault_ok && rollback_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: escape cons transactional cleanup");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: escape cons transactional cleanup (setup_ok=%s fault_ok=%s closure_rc=%d)",
                setup_ok ? "yes" : "no",
                fault_ok ? "yes" : "no",
                closure_rc_after_fail
            );
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
    run_memory_lifetime_root_boundary_method_table_abort_no_wrapper_slot_leak_test(interp, pass, fail);
    run_memory_lifetime_root_boundary_primitive_clone_test(interp, pass, fail);
    run_memory_lifetime_root_boundary_shared_wrapper_partial_cleanup_test(interp, pass, fail);
    run_memory_lifetime_cons_barrier_fallback_gate_test(interp, pass, fail);
    run_memory_lifetime_escape_disjoint_fallback_test(interp, pass, fail);
    run_memory_lifetime_escape_primitive_guard_tests(interp, pass, fail);
    run_memory_lifetime_escape_cons_transactional_cleanup_test(interp, pass, fail);
    run_memory_lifetime_escape_partial_transactional_cleanup_test(interp, pass, fail);
    run_memory_lifetime_nested_wrapper_boundary_copy_fail_closed_test(interp, pass, fail);
    run_memory_lifetime_cons_boundary_copy_transactional_cleanup_test(interp, pass, fail);
    run_memory_lifetime_partial_boundary_copy_transactional_cleanup_test(interp, pass, fail);
    run_memory_lifetime_nested_partial_iterator_boundary_cleanup_test(interp, pass, fail);
    run_memory_lifetime_splice_legality_explicit_reasons_test(interp, pass, fail);
    run_memory_lifetime_resume_missing_yield_scope_gate_test(interp, pass, fail);
}
