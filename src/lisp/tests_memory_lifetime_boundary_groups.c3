module lisp;

import std::io;
import main;

fn void run_memory_lifetime_boundary_transition_regression_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: return/env/splice boundary transitions produce deterministic outcomes.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary transition regression pack");
            (*fail)++;
            return;
        }

        // Return transition: budget-0 commit must deterministically hard-fail with promotion-aborted fault.
        interp.current_scope = releasing_scope;
        Value* return_src = make_cons(interp, make_int(interp, 11), make_nil(interp));
        PromotionContext return_ctx = {};
        boundary_promotion_context_begin(interp, &return_ctx, 0);
        BoundaryCommitEscapeResult committed = boundary_commit_escape(
            return_src,
            interp,
            target_scope,
            releasing_scope,
            &return_ctx
        );
        boundary_promotion_context_end(interp, &return_ctx);
        bool return_ok = committed.outcome == BOUNDARY_COMMIT_FALLBACK_DISALLOWED &&
                         committed.fault_code == BOUNDARY_COMMIT_FAULT_PROMOTION_ABORTED &&
                         committed.value != null &&
                         committed.value.tag == ERROR;

        // Env transition: depth-limit path must deterministically report depth fault.
        interp.current_scope = releasing_scope;
        Env* env_src = make_env(interp, interp.global_env);
        SymbolId env_sym = interp.symbols.intern("__boundary_transition_env");
        env_define_with_barrier(env_src, env_sym, make_int(interp, 22), interp);
        BoundaryEnvCopyResult env_copied = boundary_copy_env_to_target_scope_checked(
            env_src,
            interp,
            target_scope,
            256
        );
        bool env_ok = !boundary_env_copy_result_ok(env_copied) &&
                      env_copied.fault_code == BOUNDARY_ENV_COPY_FAULT_DEPTH_LIMIT;

        // Splice transition: non-unique child refcount must deterministically reject with explicit reason.
        main::scope_retain(releasing_scope);
        BoundaryScopeTransferResult splice_result =
            boundary_try_scope_splice_escapes_result(target_scope, releasing_scope);
        bool splice_ok = !splice_result.allowed &&
                         splice_result.reason == BOUNDARY_SCOPE_TRANSFER_CHILD_REFCOUNT_NOT_ONE;
        main::scope_release(releasing_scope);

        interp.current_scope = saved_scope;
        main::scope_release(releasing_scope);
        main::scope_release(target_scope);

        bool ok = return_ok && env_ok && splice_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary transition regression pack");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary transition regression pack (return_ok=%s env_ok=%s splice_ok=%s env_fault=%s splice_reason=%s commit_fault=%s)",
                return_ok ? "yes" : "no",
                env_ok ? "yes" : "no",
                splice_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(env_copied.fault_code),
                (ZString)boundary_scope_transfer_reason_name(splice_result.reason),
                (ZString)boundary_commit_fault_name(committed.fault_code)
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_run_program_boundary_state_test(Interp* interp, int* pass, int* fail) {
    // Invariant: run_program preserves boundary scope state across multi-form execution.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;

        EvalResult r = run_program(
            "(define __boundary_prog_x 10)\n"
            "(define __boundary_prog_y 32)\n"
            "(+ __boundary_prog_x __boundary_prog_y)",
            interp);

        bool ok_eval = !r.error.has_error &&
                       r.value != null &&
                       r.value.tag == INT &&
                       r.value.int_val == 42;
        bool ok_state = interp.current_scope == saved_scope &&
                        interp.releasing_scope == saved_releasing;
        bool ok = ok_eval && ok_state;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: run_program boundary state restore");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: run_program boundary state restore (ok_eval=%s ok_state=%s scope_match=%s releasing_match=%s)",
                ok_eval ? "yes" : "no",
                ok_state ? "yes" : "no",
                interp.current_scope == saved_scope ? "yes" : "no",
                interp.releasing_scope == saved_releasing ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_run_program_error_boundary_state_test(Interp* interp, int* pass, int* fail) {
    // Invariant: run_program preserves boundary scope state on parse/runtime error exits.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;

        EvalResult parse_err = run_program(
            "(define __boundary_prog_err 1\n"
            "(+ __boundary_prog_err 2)",
            interp);
        bool parse_ok = parse_err.error.has_error &&
                        interp.current_scope == saved_scope &&
                        interp.releasing_scope == saved_releasing;

        EvalResult eval_err = run_program(
            "(define __boundary_prog_eval_err 1)\n"
            "(car 1)",
            interp);
        bool eval_ok = eval_err.error.has_error &&
                       interp.current_scope == saved_scope &&
                       interp.releasing_scope == saved_releasing;

        bool ok = parse_ok && eval_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: run_program error boundary state restore");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: run_program error boundary state restore (parse_ok=%s eval_ok=%s)",
                parse_ok ? "yes" : "no",
                eval_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test(Interp* interp, int* pass, int* fail) {
    // Invariant: wrapper values in target chain are reused, while disjoint wrappers are defensively copied.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* releasing = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || releasing == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (releasing != null) main::scope_release(releasing);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: wrapper reuse vs defensive copy");
            (*fail)++;
            return;
        }

        interp.current_scope = target;
        void* h_reuse = mem::malloc(8);
        Value* reusable = make_ffi_handle_ex(interp, h_reuse, "__reuse", null, true);

        interp.current_scope = foreign;
        void* h_foreign = mem::malloc(8);
        Value* disjoint = make_ffi_handle_ex(interp, h_foreign, "__disjoint", null, true);

        interp.current_scope = target;
        interp.releasing_scope = releasing;

        Value* reused = copy_to_parent(reusable, interp);
        Value* copied = copy_to_parent(disjoint, interp);

        bool reuse_ok = reused == reusable;
        bool copied_ok = copied != null &&
                         copied.tag == FFI_HANDLE &&
                         copied != disjoint &&
                         copied.ffi_val != null &&
                         copied.ffi_val == disjoint.ffi_val &&
                         copied.ffi_val.refcount == 2;

        main::scope_release(foreign);

        bool post_ok = copied != null &&
                       copied.tag == FFI_HANDLE &&
                       copied.ffi_val != null &&
                       copied.ffi_val.refcount == 1;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(releasing);
        main::scope_release(target);

        bool ok = reusable != null && disjoint != null && reuse_ok && copied_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: wrapper reuse vs defensive copy");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: wrapper reuse vs defensive copy (reusable=%s disjoint=%s reuse_ok=%s copied_ok=%s post_ok=%s)",
                reusable != null ? "yes" : "no",
                disjoint != null ? "yes" : "no",
                reuse_ok ? "yes" : "no",
                copied_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }

    // Invariant: disjoint shared wrappers (collections/method tables) are
    // defensively cloned rather than aliased once reuse is declined.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* releasing = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || releasing == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (releasing != null) main::scope_release(releasing);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy");
            (*fail)++;
            return;
        }

        // ARRAY
        interp.current_scope = foreign;
        Value* array_src = interp.alloc_value();
        array_src.tag = ARRAY;
        main::scope_register_dtor(foreign, (void*)array_src, &scope_dtor_value);
        Array* array_payload = (Array*)mem::malloc(Array.sizeof);
        if (array_payload == null) {
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup array)");
            (*fail)++;
            return;
        }
        array_payload.capacity = 4;
        array_payload.length = 1;
        array_payload.items = (Value**)mem::malloc(Value*.sizeof * array_payload.capacity);
        if (array_payload.items == null) {
            mem::free(array_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup array items)");
            (*fail)++;
            return;
        }
        for (usz i = 0; i < array_payload.capacity; i++) {
            array_payload.items[i] = null;
        }
        array_payload.items[0] = make_int(interp, 71);
        array_src.array_val = array_payload;

        // HASHMAP
        Value* hashmap_src = interp.alloc_value();
        hashmap_src.tag = HASHMAP;
        main::scope_register_dtor(foreign, (void*)hashmap_src, &scope_dtor_value);
        HashMap* hashmap_payload = (HashMap*)mem::malloc(HashMap.sizeof);
        if (hashmap_payload == null) {
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup hashmap)");
            (*fail)++;
            return;
        }
        hashmap_payload.capacity = 8;
        hashmap_payload.count = 1;
        hashmap_payload.mask = hashmap_payload.capacity - 1;
        hashmap_payload.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * hashmap_payload.capacity);
        if (hashmap_payload.entries == null) {
            mem::free(hashmap_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup hashmap entries)");
            (*fail)++;
            return;
        }
        for (uint i = 0; i < hashmap_payload.capacity; i++) {
            hashmap_payload.entries[i].key = null;
            hashmap_payload.entries[i].value = null;
        }
        Value* hashmap_key = make_int(interp, 7);
        Value* hashmap_value = make_int(interp, 99);
        uint hashmap_idx = hash_value(hashmap_key) & hashmap_payload.mask;
        hashmap_payload.entries[hashmap_idx].key = hashmap_key;
        hashmap_payload.entries[hashmap_idx].value = hashmap_value;
        hashmap_src.hashmap_val = hashmap_payload;

        // SET
        Value* set_src = interp.alloc_value();
        set_src.tag = SET;
        main::scope_register_dtor(foreign, (void*)set_src, &scope_dtor_value);
        HashMap* set_payload = (HashMap*)mem::malloc(HashMap.sizeof);
        if (set_payload == null) {
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup set)");
            (*fail)++;
            return;
        }
        set_payload.capacity = 8;
        set_payload.count = 1;
        set_payload.mask = set_payload.capacity - 1;
        set_payload.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * set_payload.capacity);
        if (set_payload.entries == null) {
            mem::free(set_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup set entries)");
            (*fail)++;
            return;
        }
        for (uint i = 0; i < set_payload.capacity; i++) {
            set_payload.entries[i].key = null;
            set_payload.entries[i].value = null;
        }
        Value* set_key = make_int(interp, 11);
        Value* set_value = make_int(interp, 1);
        uint set_idx = hash_value(set_key) & set_payload.mask;
        set_payload.entries[set_idx].key = set_key;
        set_payload.entries[set_idx].value = set_value;
        set_src.hashmap_val = set_payload;

        // METHOD_TABLE
        Value* mt_src = interp.alloc_value();
        mt_src.tag = METHOD_TABLE;
        main::scope_register_dtor(foreign, (void*)mt_src, &scope_dtor_value);
        MethodTable* mt_payload = (MethodTable*)mem::malloc(MethodTable.sizeof);
        if (mt_payload == null) {
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup method table)");
            (*fail)++;
            return;
        }
        mt_payload.name = interp.symbols.intern("__shared_wrapper_mt");
        mt_payload.capacity = 1;
        mt_payload.entry_count = 1;
        mt_payload.fallback = make_int(interp, 123);
        mt_payload.entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof * mt_payload.capacity);
        if (mt_payload.entries == null) {
            mem::free(mt_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup method table entries)");
            (*fail)++;
            return;
        }
        method_signature_init_empty(&mt_payload.entries[0].sig);
        MethodSignature mt_sig_src;
        method_signature_init_empty(&mt_sig_src);
        if (!method_signature_alloc_arrays_in_scope(&mt_sig_src, foreign, 1, 0)) {
            mem::free(mt_payload.entries);
            mem::free(mt_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup method table signature)");
            (*fail)++;
            return;
        }
        mt_sig_src.param_types[0] = interp.tid_Integer;
        mt_sig_src.param_type_vars[0] = (SymbolId)0;
        mt_sig_src.has_val_literal[0] = false;
        if (!method_signature_copy_to_heap(&mt_payload.entries[0].sig, &mt_sig_src)) {
            mem::free(mt_payload.entries);
            mem::free(mt_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (copy method signature)");
            (*fail)++;
            return;
        }
        mt_payload.entries[0].implementation = make_int(interp, 456);
        mt_src.method_table_val = mt_payload;

        interp.current_scope = target;
        interp.releasing_scope = releasing;

        Value* array_copied = copy_to_parent(array_src, interp);
        Value* hashmap_copied = copy_to_parent(hashmap_src, interp);
        Value* set_copied = copy_to_parent(set_src, interp);
        Value* mt_copied = copy_to_parent(mt_src, interp);
        Value* array_escaped = promote_to_escape(array_src, interp);
        Value* hashmap_escaped = promote_to_escape(hashmap_src, interp);
        Value* set_escaped = promote_to_escape(set_src, interp);
        Value* mt_escaped = promote_to_escape(mt_src, interp);

        bool array_copy_ok = array_copied != null &&
                             array_copied != array_src &&
                             array_copied.tag == ARRAY &&
                             array_copied.array_val != null &&
                             array_copied.array_val != array_src.array_val &&
                             array_copied.array_val.length == 1 &&
                             array_copied.array_val.items != null &&
                             array_copied.array_val.items[0] != null &&
                             array_copied.array_val.items[0] != array_src.array_val.items[0] &&
                             array_copied.array_val.items[0].tag == INT &&
                             array_copied.array_val.items[0].int_val == 71;

        Value* copied_hashmap_key = null;
        Value* copied_hashmap_value = null;
        if (hashmap_copied != null && hashmap_copied.tag == HASHMAP && hashmap_copied.hashmap_val != null && hashmap_copied.hashmap_val.entries != null) {
            for (uint i = 0; i < hashmap_copied.hashmap_val.capacity; i++) {
                if (hashmap_copied.hashmap_val.entries[i].key != null) {
                    copied_hashmap_key = hashmap_copied.hashmap_val.entries[i].key;
                    copied_hashmap_value = hashmap_copied.hashmap_val.entries[i].value;
                    break;
                }
            }
        }
        bool hashmap_copy_ok = hashmap_copied != null &&
                               hashmap_copied != hashmap_src &&
                               hashmap_copied.tag == HASHMAP &&
                               hashmap_copied.hashmap_val != null &&
                               hashmap_copied.hashmap_val != hashmap_src.hashmap_val &&
                               copied_hashmap_key != null &&
                               copied_hashmap_value != null &&
                               copied_hashmap_key != hashmap_src.hashmap_val.entries[hashmap_idx].key &&
                               copied_hashmap_value != hashmap_src.hashmap_val.entries[hashmap_idx].value &&
                               copied_hashmap_value.tag == INT &&
                               copied_hashmap_value.int_val == 99;

        Value* copied_set_key = null;
        Value* copied_set_value = null;
        if (set_copied != null && set_copied.tag == SET && set_copied.hashmap_val != null && set_copied.hashmap_val.entries != null) {
            for (uint i = 0; i < set_copied.hashmap_val.capacity; i++) {
                if (set_copied.hashmap_val.entries[i].key != null) {
                    copied_set_key = set_copied.hashmap_val.entries[i].key;
                    copied_set_value = set_copied.hashmap_val.entries[i].value;
                    break;
                }
            }
        }
        bool set_copy_ok = set_copied != null &&
                           set_copied != set_src &&
                           set_copied.tag == SET &&
                           set_copied.hashmap_val != null &&
                           set_copied.hashmap_val != set_src.hashmap_val &&
                           copied_set_key != null &&
                           copied_set_value != null &&
                           copied_set_key != set_src.hashmap_val.entries[set_idx].key &&
                           copied_set_value != set_src.hashmap_val.entries[set_idx].value &&
                           copied_set_value.tag == INT &&
                           copied_set_value.int_val == 1;

        bool mt_copy_ok = mt_copied != null &&
                          mt_copied != mt_src &&
                          mt_copied.tag == METHOD_TABLE &&
                          mt_copied.method_table_val != null &&
                          mt_copied.method_table_val != mt_src.method_table_val &&
                          mt_copied.method_table_val.entry_count == 1 &&
                          mt_copied.method_table_val.entries != null &&
                          mt_copied.method_table_val.entries[0].implementation != null &&
                          mt_copied.method_table_val.entries[0].implementation != mt_src.method_table_val.entries[0].implementation &&
                          mt_copied.method_table_val.entries[0].implementation.tag == INT &&
                          mt_copied.method_table_val.entries[0].implementation.int_val == 456 &&
                          mt_copied.method_table_val.fallback != null &&
                          mt_copied.method_table_val.fallback != mt_src.method_table_val.fallback &&
                          mt_copied.method_table_val.fallback.tag == INT &&
                          mt_copied.method_table_val.fallback.int_val == 123;

        bool array_escape_ok = array_escaped != null &&
                               array_escaped != array_src &&
                               array_escaped.tag == ARRAY &&
                               array_escaped.scope_gen == target.escape_generation &&
                               array_escaped.array_val != null &&
                               array_escaped.array_val != array_src.array_val &&
                               array_escaped.array_val.items != null &&
                               array_escaped.array_val.items[0] != null &&
                               array_escaped.array_val.items[0] != array_src.array_val.items[0] &&
                               array_escaped.array_val.items[0].tag == INT &&
                               array_escaped.array_val.items[0].int_val == 71;

        bool hashmap_escape_ok = hashmap_escaped != null &&
                                 hashmap_escaped != hashmap_src &&
                                 hashmap_escaped.tag == HASHMAP &&
                                 hashmap_escaped.scope_gen == target.escape_generation &&
                                 hashmap_escaped.hashmap_val != null &&
                                 hashmap_escaped.hashmap_val != hashmap_src.hashmap_val;

        bool set_escape_ok = set_escaped != null &&
                             set_escaped != set_src &&
                             set_escaped.tag == SET &&
                             set_escaped.scope_gen == target.escape_generation &&
                             set_escaped.hashmap_val != null &&
                             set_escaped.hashmap_val != set_src.hashmap_val;

        bool mt_escape_ok = mt_escaped != null &&
                            mt_escaped != mt_src &&
                            mt_escaped.tag == METHOD_TABLE &&
                            mt_escaped.scope_gen == target.escape_generation &&
                            mt_escaped.method_table_val != null &&
                            mt_escaped.method_table_val != mt_src.method_table_val &&
                            mt_escaped.method_table_val.entry_count == 1 &&
                            mt_escaped.method_table_val.entries != null &&
                            mt_escaped.method_table_val.entries[0].implementation != null &&
                            mt_escaped.method_table_val.entries[0].implementation.tag == INT &&
                            mt_escaped.method_table_val.entries[0].implementation.int_val == 456 &&
                            mt_escaped.method_table_val.fallback != null &&
                            mt_escaped.method_table_val.fallback.tag == INT &&
                            mt_escaped.method_table_val.fallback.int_val == 123;

        main::scope_release(foreign);

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(releasing);
        main::scope_release(target);

        bool ok = array_copy_ok && hashmap_copy_ok && set_copy_ok && mt_copy_ok &&
                  array_escape_ok && hashmap_escape_ok && set_escape_ok && mt_escape_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: shared wrapper defensive clone policy");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: shared wrapper defensive clone policy (array_copy=%s hashmap_copy=%s set_copy=%s mt_copy=%s array_escape=%s hashmap_escape=%s set_escape=%s mt_escape=%s)",
                array_copy_ok ? "yes" : "no",
                hashmap_copy_ok ? "yes" : "no",
                set_copy_ok ? "yes" : "no",
                mt_copy_ok ? "yes" : "no",
                array_escape_ok ? "yes" : "no",
                hashmap_escape_ok ? "yes" : "no",
                set_escape_ok ? "yes" : "no",
                mt_escape_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }

    // Invariant: primitive wrappers already in target chain are reused, while
    // disjoint plain primitive wrappers are cloned and opaque payloads fail as
    // typed boundary-copy faults.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* releasing = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || releasing == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (releasing != null) main::scope_release(releasing);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: primitive wrapper copy policy");
            (*fail)++;
            return;
        }

        interp.current_scope = target;
        Value* reusable = interp.alloc_value();
        reusable.tag = PRIMITIVE;
        main::scope_register_dtor(target, (void*)reusable, &scope_dtor_value);
        Primitive* reusable_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        if (reusable_prim == null) {
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: primitive wrapper copy policy (setup reusable)");
            (*fail)++;
            return;
        }
        *reusable_prim = {};
        reusable_prim.name[0] = 'r';
        reusable_prim.name[1] = 0;
        reusable_prim.arity = 1;
        reusable_prim.tag = 601;
        reusable.prim_val = reusable_prim;

        interp.current_scope = foreign;
        Value* disjoint = interp.alloc_value();
        disjoint.tag = PRIMITIVE;
        main::scope_register_dtor(foreign, (void*)disjoint, &scope_dtor_value);
        Primitive* disjoint_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        if (disjoint_prim == null) {
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: primitive wrapper copy policy (setup disjoint)");
            (*fail)++;
            return;
        }
        *disjoint_prim = {};
        disjoint_prim.name[0] = 'd';
        disjoint_prim.name[1] = 0;
        disjoint_prim.arity = 2;
        disjoint_prim.tag = 602;
        disjoint.prim_val = disjoint_prim;

        Value* opaque = interp.alloc_value();
        opaque.tag = PRIMITIVE;
        main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
        Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        int* opaque_payload = (int*)mem::malloc(int.sizeof);
        if (opaque_prim == null || opaque_payload == null) {
            if (opaque_prim != null) mem::free(opaque_prim);
            if (opaque_payload != null) mem::free(opaque_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: primitive wrapper copy policy (setup opaque)");
            (*fail)++;
            return;
        }
        *opaque_prim = {};
        *opaque_payload = 603;
        opaque_prim.name[0] = 'o';
        opaque_prim.name[1] = 0;
        opaque_prim.arity = 3;
        opaque_prim.tag = 603;
        opaque_prim.user_data = (void*)opaque_payload;
        opaque.prim_val = opaque_prim;

        interp.current_scope = target;
        interp.releasing_scope = releasing;

        Value* reused = copy_to_parent(reusable, interp);
        Value* copied = copy_to_parent(disjoint, interp);
        BoundaryCopyResult rejected = boundary_copy_to_parent_site_ctx_checked(
            opaque,
            interp,
            COPY_SITE_GENERIC,
            null
        );

        bool reuse_ok = reused == reusable;
        bool copied_ok = copied != null &&
                         copied.tag == PRIMITIVE &&
                         copied != disjoint &&
                         copied.prim_val != null &&
                         copied.prim_val != disjoint.prim_val &&
                         copied.prim_val.arity == 2 &&
                         copied.prim_val.tag == 602 &&
                         copied.prim_val.name[0] == 'd' &&
                         copied.prim_val.name[1] == 0;
        bool reject_ok = !boundary_copy_result_ok(rejected) &&
                         rejected.value == null &&
                         rejected.fault_code == BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;

        main::scope_release(foreign);

        bool post_ok = copied != null &&
                       copied.tag == PRIMITIVE &&
                       copied.prim_val != null &&
                       copied.prim_val.arity == 2 &&
                       copied.prim_val.tag == 602 &&
                       copied.prim_val.name[0] == 'd' &&
                       copied.prim_val.name[1] == 0;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(releasing);
        main::scope_release(target);

        bool ok = reuse_ok && copied_ok && reject_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: primitive wrapper copy policy");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: primitive wrapper copy policy (reuse_ok=%s copied_ok=%s reject_ok=%s post_ok=%s fault=%s)",
                reuse_ok ? "yes" : "no",
                copied_ok ? "yes" : "no",
                reject_ok ? "yes" : "no",
                post_ok ? "yes" : "no",
                (ZString)boundary_copy_fault_name(rejected.fault_code));
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_nested_wrapper_boundary_copy_fail_closed_test(Interp* interp, int* pass, int* fail) {
    // Invariant: nested CONS/PARTIAL_PRIM/ITERATOR boundary copies fail closed
    // when they transitively encounter an opaque primitive payload.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: nested boundary copy fail-closed");
            (*fail)++;
            return;
        }

        interp.current_scope = foreign;
        Value* opaque = interp.alloc_value();
        opaque.tag = PRIMITIVE;
        main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
        Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        int* opaque_payload = (int*)mem::malloc(int.sizeof);
        if (opaque_prim == null || opaque_payload == null) {
            if (opaque_prim != null) mem::free(opaque_prim);
            if (opaque_payload != null) mem::free(opaque_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: nested boundary copy fail-closed (setup opaque)");
            (*fail)++;
            return;
        }
        *opaque_prim = {};
        *opaque_payload = 701;
        opaque_prim.name[0] = 'n';
        opaque_prim.name[1] = 0;
        opaque_prim.arity = 2;
        opaque_prim.tag = 701;
        opaque_prim.user_data = (void*)opaque_payload;
        opaque.prim_val = opaque_prim;

        Value* partial = interp.alloc_value();
        partial.tag = PARTIAL_PRIM;
        partial.partial_val.func = &prim_add;
        partial.partial_val.remaining = 1;
        partial.partial_val.first_arg = opaque;
        partial.partial_val.second_arg = make_int(interp, 5);

        Value* iter = make_iterator(interp, partial);
        Value* cons = make_cons(interp, iter, make_nil(interp));

        interp.current_scope = target;
        interp.releasing_scope = foreign;

        BoundaryCopyResult cons_copy = boundary_copy_to_parent_site_ctx_checked(cons, interp, COPY_SITE_GENERIC, null);
        BoundaryCopyResult partial_copy = boundary_copy_to_parent_site_ctx_checked(partial, interp, COPY_SITE_GENERIC, null);
        BoundaryCopyResult iter_copy = boundary_copy_to_parent_site_ctx_checked(iter, interp, COPY_SITE_GENERIC, null);

        bool cons_ok = !boundary_copy_result_ok(cons_copy) &&
                       cons_copy.value == null &&
                       cons_copy.fault_code == BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;
        bool partial_ok = !boundary_copy_result_ok(partial_copy) &&
                          partial_copy.value == null &&
                          partial_copy.fault_code == BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;
        bool iter_ok = !boundary_copy_result_ok(iter_copy) &&
                       iter_copy.value == null &&
                       iter_copy.fault_code == BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;

        main::scope_release(foreign);

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(target);

        bool ok = cons_ok && partial_ok && iter_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: nested boundary copy fail-closed");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: nested boundary copy fail-closed (cons_ok=%s partial_ok=%s iter_ok=%s)",
                cons_ok ? "yes" : "no",
                partial_ok ? "yes" : "no",
                iter_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_method_table_abort_cleanup_test(Interp* interp, int* pass, int* fail) {
    // Invariant: method-table copy/escape abort paths reclaim already-copied
    // heap signatures when a later signature copy fails.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* releasing = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || releasing == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (releasing != null) main::scope_release(releasing);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: method table abort cleanup telemetry");
            (*fail)++;
            return;
        }

        interp.current_scope = foreign;
        Value* mt_src = interp.alloc_value();
        mt_src.tag = METHOD_TABLE;
        main::scope_register_dtor(foreign, (void*)mt_src, &scope_dtor_value);

        MethodTable* mt_payload = (MethodTable*)mem::malloc(MethodTable.sizeof);
        if (mt_payload == null) {
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: method table abort cleanup telemetry (setup table)");
            (*fail)++;
            return;
        }
        mt_payload.name = interp.symbols.intern("__mt_abort_cleanup");
        mt_payload.capacity = 2;
        mt_payload.entry_count = 2;
        mt_payload.fallback = null;
        mt_payload.entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof * mt_payload.capacity);
        if (mt_payload.entries == null) {
            mem::free(mt_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: method table abort cleanup telemetry (setup entries)");
            (*fail)++;
            return;
        }

        bool setup_ok = true;
        for (usz i = 0; i < mt_payload.entry_count; i++) {
            method_signature_init_empty(&mt_payload.entries[i].sig);
            MethodSignature sig_src;
            method_signature_init_empty(&sig_src);
            if (!method_signature_alloc_arrays_in_scope(&sig_src, foreign, 1, 0)) {
                setup_ok = false;
                break;
            }
            sig_src.param_types[0] = interp.tid_Integer;
            sig_src.param_type_vars[0] = (SymbolId)0;
            sig_src.has_val_literal[0] = false;
            if (!method_signature_copy_to_heap(&mt_payload.entries[i].sig, &sig_src)) {
                setup_ok = false;
                break;
            }
            mt_payload.entries[i].implementation = make_int(interp, (long)(900 + i));
        }

        if (!setup_ok) {
            method_table_free_partial_heap(mt_payload, mt_payload.entry_count);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: method table abort cleanup telemetry (setup signature)");
            (*fail)++;
            return;
        }
        mt_src.method_table_val = mt_payload;

        interp.current_scope = target;
        interp.releasing_scope = releasing;

        method_table_partial_cleanup_reset();
        g_method_signature_heap_copy_call_count = 0;
        g_method_signature_force_heap_copy_fail_on_call = 2;
        BoundaryCopyResult copy_result = boundary_copy_to_parent_site_ctx_checked(
            mt_src,
            interp,
            COPY_SITE_GENERIC,
            null
        );
        usz copy_cleanup_calls = method_table_partial_cleanup_calls();
        usz copy_cleanup_entries = method_table_partial_cleanup_entries();

        method_table_partial_cleanup_reset();
        g_method_signature_heap_copy_call_count = 0;
        g_method_signature_force_heap_copy_fail_on_call = 2;
        Value* escape_result = promote_to_escape(mt_src, interp);
        usz escape_cleanup_calls = method_table_partial_cleanup_calls();
        usz escape_cleanup_entries = method_table_partial_cleanup_entries();
        g_method_signature_force_heap_copy_fail_on_call = 0;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(foreign);
        main::scope_release(releasing);
        main::scope_release(target);

        bool copy_ok = !boundary_copy_result_ok(copy_result) &&
            copy_result.value == null &&
            copy_result.fault_code == BOUNDARY_COPY_FAULT_NULL_RESULT &&
            copy_cleanup_calls == 1 &&
            copy_cleanup_entries == 1;
        bool escape_ok = escape_result != null &&
            escape_result.tag == ERROR &&
            escape_cleanup_calls == 1 &&
            escape_cleanup_entries == 1;

        bool ok = copy_ok && escape_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: method table abort cleanup telemetry");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: method table abort cleanup telemetry (copy_ok=%s escape_ok=%s copy_fault=%s copy_calls=%d copy_entries=%d escape_tag=%d escape_calls=%d escape_entries=%d)",
                copy_ok ? "yes" : "no",
                escape_ok ? "yes" : "no",
                (ZString)boundary_copy_fault_name(copy_result.fault_code),
                (long)copy_cleanup_calls,
                (long)copy_cleanup_entries,
                escape_result != null ? (int)escape_result.tag : -1,
                (long)escape_cleanup_calls,
                (long)escape_cleanup_entries
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_shared_wrapper_partial_cleanup_test(Interp* interp, int* pass, int* fail) {
    // Invariant: shared-wrapper partial-copy failure unwinds already-copied
    // child ownership side effects instead of leaking retained env scopes.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* foreign = main::scope_create(saved);
        main::ScopeRegion* target = main::scope_create(saved);
        if (foreign == null || target == null) {
            if (foreign != null) main::scope_release(foreign);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: shared-wrapper partial cleanup releases retains");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__shared_wrapper_partial_cleanup_payload");

        interp.current_scope = foreign;
        Env* captured_env = make_env(interp, interp.global_env);
        if (captured_env != null) {
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 808), interp);
        }
        main::ScopeRegion* closure_env_scope = null;
        Value* shared_closure =
            captured_env != null ? make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope) : null;
        Value* opaque = interp.alloc_value();
        opaque.tag = PRIMITIVE;
        main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
        Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        int* opaque_payload = (int*)mem::malloc(int.sizeof);
        if (opaque_prim == null || opaque_payload == null) {
            if (opaque_prim != null) mem::free(opaque_prim);
            if (opaque_payload != null) mem::free(opaque_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared-wrapper partial cleanup releases retains (setup opaque)");
            (*fail)++;
            return;
        }
        *opaque_prim = {};
        *opaque_payload = 809;
        opaque_prim.name[0] = 'p';
        opaque_prim.name[1] = 0;
        opaque_prim.arity = 1;
        opaque_prim.tag = 809;
        opaque_prim.user_data = (void*)opaque_payload;
        opaque.prim_val = opaque_prim;

        Value* src_array = null;
        if (shared_closure != null) {
            src_array = interp.alloc_value();
            if (src_array != null) {
                src_array.tag = ARRAY;
                main::scope_register_dtor(foreign, (void*)src_array, &scope_dtor_value);
                Array* src_array_payload = (Array*)mem::malloc(Array.sizeof);
                if (src_array_payload != null) {
                    src_array_payload.capacity = 2;
                    src_array_payload.length = 2;
                    src_array_payload.items = (Value**)mem::malloc(Value*.sizeof * src_array_payload.capacity);
                    if (src_array_payload.items != null) {
                        src_array_payload.items[0] = shared_closure;
                        src_array_payload.items[1] = opaque;
                        src_array.array_val = src_array_payload;
                    } else {
                        mem::free(src_array_payload);
                        src_array.array_val = null;
                    }
                } else {
                    src_array.array_val = null;
                }
            }
        }

        bool setup_ok = shared_closure != null &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1 &&
                        opaque != null &&
                        opaque.tag == PRIMITIVE &&
                        opaque.prim_val != null &&
                        opaque.prim_val.user_data != null &&
                        src_array != null &&
                        src_array.tag == ARRAY &&
                        src_array.array_val != null &&
                        src_array.array_val.length == 2;

        interp.current_scope = target;
        interp.releasing_scope = foreign;
        BoundaryCopyResult copy_result = setup_ok
            ? boundary_copy_to_parent_site_ctx_checked(src_array, interp, COPY_SITE_GENERIC, null)
            : boundary_copy_fail(BOUNDARY_COPY_FAULT_NULL_RESULT);
        int rc_after_copy_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(foreign);
        main::scope_release(target);

        bool copy_ok = !boundary_copy_result_ok(copy_result) &&
                       copy_result.value == null &&
                       copy_result.fault_code == BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD &&
                       rc_after_copy_fail == 1;
        bool ok = setup_ok && copy_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: shared-wrapper partial cleanup releases retains");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: shared-wrapper partial cleanup releases retains (setup_ok=%s copy_fault=%s rc_after_copy_fail=%d)",
                setup_ok ? "yes" : "no",
                (ZString)boundary_copy_fault_name(copy_result.fault_code),
                rc_after_copy_fail
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_splice_legality_explicit_reasons_test(Interp* interp, int* pass, int* fail) {
    // Invariant: splice fast-path rejects sibling/foreign/retained-child transfers with explicit reasons.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* parent = main::scope_create(saved);
        main::ScopeRegion* child = main::scope_create(parent);
        main::ScopeRegion* sibling = main::scope_create(parent);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (parent == null || child == null || sibling == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (sibling != null) main::scope_release(sibling);
            if (child != null) main::scope_release(child);
            if (parent != null) main::scope_release(parent);
            io::printn("[FAIL] lifetime: splice legality explicit reasons");
            (*fail)++;
            return;
        }

        interp.current_scope = child;
        void* escaped = child.alloc_escape(16);
        interp.current_scope = saved;

        BoundaryScopeTransferResult sibling_result = boundary_try_scope_splice_escapes_result(sibling, child);
        BoundaryScopeTransferResult foreign_result = boundary_try_scope_splice_escapes_result(foreign, child);

        void* saved_child_owner = child.owner_thread_token;
        child.owner_thread_token = null;
        BoundaryScopeTransferResult owner_result = boundary_try_scope_splice_escapes_result(parent, child);
        child.owner_thread_token = saved_child_owner;

        main::scope_retain(child);
        BoundaryScopeTransferResult retained_result = boundary_try_scope_splice_escapes_result(parent, child);

        bool sibling_ok = !sibling_result.allowed &&
                          sibling_result.reason == BOUNDARY_SCOPE_TRANSFER_NOT_IMMEDIATE_CHILD;
        bool foreign_ok = !foreign_result.allowed &&
                          foreign_result.reason == BOUNDARY_SCOPE_TRANSFER_NOT_IMMEDIATE_CHILD;
        bool owner_ok = !owner_result.allowed &&
                        owner_result.reason == BOUNDARY_SCOPE_TRANSFER_OWNER_THREAD_MISMATCH;
        bool retained_ok = !retained_result.allowed &&
                           retained_result.reason == BOUNDARY_SCOPE_TRANSFER_CHILD_REFCOUNT_NOT_ONE;
        bool state_ok = escaped != null &&
                        child.parent == parent &&
                        child.refcount == 2 &&
                        main::is_in_scope(escaped, child) &&
                        !main::is_in_scope(escaped, parent) &&
                        !main::is_in_scope(escaped, sibling) &&
                        !main::is_in_scope(escaped, foreign);

        main::scope_release(child); // Drop extra retain before normal teardown.
        main::scope_release(sibling);
        main::scope_release(child);
        main::scope_release(foreign);
        main::scope_release(parent);

        bool ok = sibling_ok && foreign_ok && owner_ok && retained_ok && state_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: splice legality explicit reasons");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: splice legality explicit reasons (sibling=%s foreign=%s owner=%s retained=%s state_ok=%s reasons=%s/%s/%s/%s)",
                sibling_ok ? "yes" : "no",
                foreign_ok ? "yes" : "no",
                owner_ok ? "yes" : "no",
                retained_ok ? "yes" : "no",
                state_ok ? "yes" : "no",
                boundary_scope_transfer_reason_name(sibling_result.reason),
                boundary_scope_transfer_reason_name(foreign_result.reason),
                boundary_scope_transfer_reason_name(owner_result.reason),
                boundary_scope_transfer_reason_name(retained_result.reason)
            );
            (*fail)++;
        }
    }
}
