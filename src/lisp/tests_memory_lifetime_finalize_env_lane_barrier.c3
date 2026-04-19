module lisp;

import std::io;
import main;

fn void run_memory_lifetime_env_lane_barrier_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: env_define_with_barrier promotes temp payload for ESCAPE-lane env.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* s = main::scope_create(saved);
        interp.current_scope = s;

        Env* e = make_env(interp, null);
        e.scope_gen = s.escape_generation; // Simulate ESCAPE-lane env frame
        SymbolId sid = interp.symbols.intern("__barrier_define");
        Value* tmp = make_int(interp, 11);
        env_define_with_barrier(e, sid, tmp, interp);
        Value* got = e.lookup(sid);

        bool ok = got != null &&
                  got.tag == INT &&
                  got.int_val == 11 &&
                  got.scope_gen == s.escape_generation;

        interp.current_scope = saved;
        main::scope_release(s);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: env_define_with_barrier promotes temp value");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: env_define_with_barrier promotes temp value");
            (*fail)++;
        }
    }

    // Invariant: env_set_with_barrier promotes temp payload at write target frame.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* s = main::scope_create(saved);
        interp.current_scope = s;

        Env* e = make_env(interp, null);
        e.scope_gen = s.escape_generation; // Simulate ESCAPE-lane env frame
        SymbolId sid = interp.symbols.intern("__barrier_set");
        env_define_with_barrier(e, sid, make_int(interp, 1), interp);
        Value* tmp_new = make_int(interp, 99);
        bool set_ok = true;
        if (catch err = env_set_with_barrier(e, sid, tmp_new, interp)) {
            set_ok = false;
        }
        Value* got = e.lookup(sid);

        bool ok = set_ok &&
                  got != null &&
                  got.tag == INT &&
                  got.int_val == 99 &&
                  got.scope_gen == s.escape_generation;

        interp.current_scope = saved;
        main::scope_release(s);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: env_set_with_barrier promotes temp value");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: env_set_with_barrier promotes temp value");
            (*fail)++;
        }
    }

    // Invariant: mutating a detached copied env frame routes through target-scope
    // authority and never leaves raw source TEMP values in detached bindings.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved);
        main::ScopeRegion* detached = main::scope_create(null);
        bool setup_ok = source != null && detached != null;
        if (!setup_ok) {
            if (source != null) main::scope_release(source);
            if (detached != null) main::scope_release(detached);
            io::printn("[FAIL] lifetime: env_set_with_barrier detached copied env scope authority");
            (*fail)++;
        } else {
            interp.current_scope = source;

            SymbolId sid = interp.symbols.intern("__barrier_detached_copied_env");
            Env* source_env = make_env(interp, interp.global_env);
            env_define_with_barrier(source_env, sid, make_int(interp, 1), interp);
            Env* detached_env = boundary_copy_env_to_target_scope(source_env, interp, detached);
            Value* temp_pair = make_cons(interp, make_int(interp, 42), make_nil(interp));

            bool write_ok = detached_env != null;
            if (write_ok) {
                if (catch err = env_set_with_barrier(detached_env, sid, temp_pair, interp)) {
                    write_ok = false;
                }
            }

            Value* got = detached_env != null ? detached_env.lookup(sid) : null;
            bool got_in_detached = got != null && main::is_in_scope((void*)got, detached);
            bool got_in_source = got != null && main::is_in_scope((void*)got, source);

            interp.current_scope = saved;
            main::scope_release(source);

            bool post_release_ok = got_in_detached &&
                                   got != null &&
                                   got.tag == CONS &&
                                   got.cons_val.car != null &&
                                   got.cons_val.car.tag == INT &&
                                   got.cons_val.car.int_val == 42;
            bool detached_env_scope_ok = detached_env != null &&
                                         detached_env.scope_gen == detached.generation;
            main::scope_release(detached);

            bool ok = write_ok &&
                      detached_env != null &&
                      detached_env_scope_ok &&
                      got != null &&
                      got != temp_pair &&
                      got_in_detached &&
                      !got_in_source &&
                      post_release_ok;
            if (ok) {
                emit_pass_literal("[PASS] lifetime: env_set_with_barrier detached copied env scope authority");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: env_set_with_barrier detached copied env scope authority (write_ok=%s detached_env=%s got_same=%s in_detached=%s in_source=%s post_release_ok=%s)",
                    write_ok ? "yes" : "no",
                    detached_env != null ? "yes" : "no",
                    got == temp_pair ? "yes" : "no",
                    got_in_detached ? "yes" : "no",
                    got_in_source ? "yes" : "no",
                    post_release_ok ? "yes" : "no"
                );
                (*fail)++;
            }
        }
    }

    // Invariant: after closure env normalization into a detached env_scope,
    // env-set barrier writes into normalized env authority rather than source TEMP.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved);
        bool setup_ok = source != null;
        if (!setup_ok) {
            io::printn("[FAIL] lifetime: env_set_with_barrier normalized detached closure env scope authority");
            (*fail)++;
        } else {
            interp.current_scope = source;

            SymbolId sid = interp.symbols.intern("__barrier_detached_normalized_env");
            Env* captured_env = make_env(interp, interp.global_env);
            env_define_with_barrier(captured_env, sid, make_int(interp, 2), interp);

            Value* closure = make_closure_no_param(interp, null, captured_env);
            main::ScopeRegion* stale_scope = null;
            bool normalized_ok = closure != null && closure.tag == CLOSURE;
            if (normalized_ok) {
                stale_scope = main::scope_create(null);
                normalized_ok = stale_scope != null;
            }
            if (normalized_ok && stale_scope != null) {
                closure.closure_val.env_scope = stale_scope;
                Value* normalized = boundary_commit_normalize_closure_env(closure, interp, null);
                normalized_ok = normalized != null &&
                                normalized == closure &&
                                closure.closure_val.env_scope != null &&
                                closure.closure_val.env_scope != stale_scope &&
                                closure.closure_val.env != null &&
                                boundary_ptr_in_scope((void*)closure.closure_val.env, closure.closure_val.env_scope);
            }

            Value* temp_pair = make_cons(interp, make_int(interp, 84), make_nil(interp));
            bool write_ok = normalized_ok;
            if (write_ok) {
                if (catch err = env_set_with_barrier(closure.closure_val.env, sid, temp_pair, interp)) {
                    write_ok = false;
                }
            }

            Value* got = normalized_ok ? closure.closure_val.env.lookup(sid) : null;
            main::ScopeRegion* normalized_scope = normalized_ok ? closure.closure_val.env_scope : null;
            bool got_in_normalized = got != null && normalized_scope != null && main::is_in_scope((void*)got, normalized_scope);
            bool got_in_source = got != null && main::is_in_scope((void*)got, source);

            if (normalized_scope != null) main::scope_retain(normalized_scope);
            interp.current_scope = saved;
            main::scope_release(source);

            bool post_release_ok = got_in_normalized &&
                                   got != null &&
                                   got.tag == CONS &&
                                   got.cons_val.car != null &&
                                   got.cons_val.car.tag == INT &&
                                   got.cons_val.car.int_val == 84;

            if (normalized_scope != null) main::scope_release(normalized_scope);

            bool ok = write_ok &&
                      normalized_ok &&
                      got != null &&
                      got != temp_pair &&
                      got_in_normalized &&
                      !got_in_source &&
                      post_release_ok;
            if (ok) {
                emit_pass_literal("[PASS] lifetime: env_set_with_barrier normalized detached closure env scope authority");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: env_set_with_barrier normalized detached closure env scope authority (normalized_ok=%s write_ok=%s got_same=%s in_normalized=%s in_source=%s post_release_ok=%s)",
                    normalized_ok ? "yes" : "no",
                    write_ok ? "yes" : "no",
                    got == temp_pair ? "yes" : "no",
                    got_in_normalized ? "yes" : "no",
                    got_in_source ? "yes" : "no",
                    post_release_ok ? "yes" : "no"
                );
                (*fail)++;
            }
        }
    }

    // Invariant: checked env-set surfaces barrier write failure and preserves
    // the previously installed binding value.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* s = main::scope_create(saved);
        interp.current_scope = s;

        Env* e = make_env(interp, null);
        bool setup_ok = e != null;
        if (setup_ok) {
            e.scope_gen = s.escape_generation; // Simulate ESCAPE-lane env frame
        }
        SymbolId sid = interp.symbols.intern("__barrier_set_checked");
        if (setup_ok) {
            env_define_with_barrier(e, sid, make_int(interp, 7), interp);
        }

        bool found = false;
        g_env_force_barrier_promote_failure = true;
        bool set_ok = setup_ok &&
                      env_set_with_barrier_checked(e, sid, make_int(interp, 99), interp, &found);
        g_env_force_barrier_promote_failure = false;

        Value* got = e != null ? e.lookup(sid) : null;
        bool ok = setup_ok &&
                  !set_ok &&
                  found &&
                  got != null &&
                  got.tag == INT &&
                  got.int_val == 7;

        interp.current_scope = saved;
        main::scope_release(s);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: checked env set surfaces barrier failure");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: checked env set surfaces barrier failure (setup_ok=%s set_ok=%s found=%s got_tag=%d got_int=%d)",
                setup_ok ? "yes" : "no",
                set_ok ? "yes" : "no",
                found ? "yes" : "no",
                got != null ? (int)got.tag : -1,
                got != null && got.tag == INT ? (int)got.int_val : -1
            );
            (*fail)++;
        }
    }

    // Invariant: checked env define surfaces binding-array growth failure and
    // leaves existing bindings intact.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* s = main::scope_create(saved);
        interp.current_scope = s;

        Env* e = make_env(interp, null);
        bool setup_ok = e != null;
        SymbolId missing_sid = (SymbolId)0;
        for (usz i = 0; i < 4 && setup_ok; i++) {
            char[48] name_buf;
            if (try char[] name = io::bprintf(&name_buf, "__barrier_growth_seed_%d", (int)i)) {
                SymbolId sid = interp.symbols.intern(name);
                setup_ok = env_define_with_barrier_checked(e, sid, make_int(interp, (long)(i + 1)), interp);
            } else {
                setup_ok = false;
            }
        }

        char[48] missing_buf;
        if (try char[] missing_name = io::bprintf(&missing_buf, "__barrier_growth_missing_%d", 4)) {
            missing_sid = interp.symbols.intern(missing_name);
        } else {
            setup_ok = false;
        }

        g_env_force_bindings_alloc_oom = true;
        bool define_ok = setup_ok &&
                         env_define_with_barrier_checked(e, missing_sid, make_int(interp, 99), interp);
        g_env_force_bindings_alloc_oom = false;

        bool existing_ok = setup_ok;
        for (usz i = 0; i < 4 && existing_ok; i++) {
            char[48] name_buf;
            if (try char[] name = io::bprintf(&name_buf, "__barrier_growth_seed_%d", (int)i)) {
                SymbolId sid = interp.symbols.intern(name);
                Value* got = e.lookup(sid);
                existing_ok = got != null &&
                              got.tag == INT &&
                              got.int_val == (long)(i + 1);
            } else {
                existing_ok = false;
            }
        }
        Value* missing = e.lookup(missing_sid);
        usz binding_count = e != null ? e.binding_count : 0;
        usz capacity = e != null ? e.capacity : 0;

        bool ok = setup_ok &&
                  !define_ok &&
                  binding_count == 4 &&
                  capacity == 4 &&
                  missing == null &&
                  existing_ok;

        interp.current_scope = saved;
        main::scope_release(s);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: checked env define surfaces growth failure");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: checked env define surfaces growth failure (setup_ok=%s define_ok=%s binding_count=%d capacity=%d existing_ok=%s missing=%s)",
                setup_ok ? "yes" : "no",
                define_ok ? "yes" : "no",
                (int)binding_count,
                (int)capacity,
                existing_ok ? "yes" : "no",
                missing == null ? "null" : "present"
            );
            (*fail)++;
        }
    }

    // Invariant: hash rebuild OOM preserves binding visibility by degrading back
    // to linear-scan lookup instead of hiding the just-inserted binding.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* s = main::scope_create(saved);
        interp.current_scope = s;

        Env* e = make_env(interp, null);
        bool setup_ok = e != null;
        for (usz i = 0; i < ENV_HASH_THRESHOLD && setup_ok; i++) {
            char[48] name_buf;
            if (try char[] name = io::bprintf(&name_buf, "__hash_rebuild_seed_%d", (int)i)) {
                SymbolId sid = interp.symbols.intern(name);
                setup_ok = env_define_with_barrier_checked(e, sid, make_int(interp, (long)(100 + i)), interp);
            } else {
                setup_ok = false;
            }
        }

        char[48] trigger_buf;
        SymbolId trigger_sid = (SymbolId)0;
        if (try char[] trigger_name = io::bprintf(&trigger_buf, "__hash_rebuild_trigger_%d", (int)ENV_HASH_THRESHOLD)) {
            trigger_sid = interp.symbols.intern(trigger_name);
        } else {
            setup_ok = false;
        }

        g_env_force_hash_alloc_oom = true;
        bool define_ok = setup_ok &&
                         env_define_with_barrier_checked(e, trigger_sid, make_int(interp, 777), interp);
        g_env_force_hash_alloc_oom = false;

        Value* trigger_value = e.lookup(trigger_sid);
        usz binding_count = e != null ? e.binding_count : 0;
        bool has_hash_table = e != null && e.hash_table != null;
        bool ok = setup_ok &&
                  define_ok &&
                  binding_count == ENV_HASH_THRESHOLD + 1 &&
                  !has_hash_table &&
                  e.hash_capacity == 0 &&
                  trigger_value != null &&
                  trigger_value.tag == INT &&
                  trigger_value.int_val == 777;

        interp.current_scope = saved;
        main::scope_release(s);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: env hash rebuild oom preserves binding visibility");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env hash rebuild oom preserves binding visibility (setup_ok=%s define_ok=%s binding_count=%d hash_table=%s trigger_tag=%d trigger_int=%d)",
                setup_ok ? "yes" : "no",
                define_ok ? "yes" : "no",
                (int)binding_count,
                has_hash_table ? "present" : "null",
                trigger_value != null ? (int)trigger_value.tag : -1,
                (trigger_value != null && trigger_value.tag == INT) ? (int)trigger_value.int_val : -1
            );
            (*fail)++;
        }
    }

    // Invariant: runtime bootstrap stops before stdlib loading when primitive
    // registration hits a recoverable binding-growth failure.
    {
        Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
        bool setup_ok = tmp != null;
        if (setup_ok) {
            tmp.init();
        }

        g_env_force_bindings_alloc_oom = true;
        bool boot_ok = setup_ok && bootstrap_runtime_interp(tmp);
        g_env_force_bindings_alloc_oom = false;

        SymbolId plus_sid = setup_ok ? tmp.symbols.intern("+") : (SymbolId)0;
        SymbolId map_sid = setup_ok ? tmp.symbols.intern("map") : (SymbolId)0;
        Value* plus_v = setup_ok ? tmp.global_env.lookup(plus_sid) : null;
        Value* map_v = setup_ok ? tmp.global_env.lookup(map_sid) : null;
        bool ok = setup_ok &&
                  !boot_ok &&
                  plus_v != null &&
                  map_v == null;

        if (tmp != null) {
            tmp.destroy();
            mem::free(tmp);
        }

        if (ok) {
            emit_pass_literal("[PASS] lifetime: bootstrap runtime stops before stdlib on registration failure");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: bootstrap runtime stops before stdlib on registration failure (setup_ok=%s boot_ok=%s plus=%s map=%s)",
                setup_ok ? "yes" : "no",
                boot_ok ? "yes" : "no",
                plus_v != null ? "present" : "null",
                map_v != null ? "present" : "null"
            );
            (*fail)++;
        }
    }
}
