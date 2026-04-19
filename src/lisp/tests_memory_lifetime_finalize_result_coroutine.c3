module lisp;

import std::io;
import main;

fn bool run_memory_lifetime_result_and_coroutine_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: run() must return a value that lives in caller scope.
    // For closure results, this catches stale child-scope pointers.
    {
        EvalResult r = run("((lambda (x) (lambda (y) x)) 10)", interp);
        bool ok = !r.error.has_error &&
                  r.value != null &&
                  main::is_in_scope((void*)r.value, interp.current_scope);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: run promotes closure result to caller scope");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: run promotes closure result to caller scope");
            (*fail)++;
        }
    }

    // Invariant: completed user coroutines release their StackCtx back to the
    // runtime ownership path. Normal builds recycle into the pool; ASAN builds
    // free directly to avoid stale fake-stack reuse.
    {
        Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
        if (tmp == null) {
            io::printn("[FAIL] lifetime: completed coroutine releases StackCtx after completion (bootstrap alloc)");
            (*fail)++;
            return false;
        }
        tmp.init();
        if (!register_primitives(tmp)) {
            io::printn("[FAIL] lifetime: completed coroutine releases StackCtx after completion (bootstrap)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        EvalResult r_thunk = run("(lambda () 42)", tmp);
        if (r_thunk.error.has_error || r_thunk.value == null) {
            io::printn("[FAIL] lifetime: completed coroutine releases StackCtx after completion (failed to build thunk)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        Value*[1] coro_args;
        coro_args[0] = r_thunk.value;

        usz before = tmp.stack_ctx_pool.pool_size;
        Value* c = prim_coroutine(coro_args[..], null, tmp);

        Value*[1] resume_args;
        resume_args[0] = c;
        Value* r = prim_resume(resume_args[..], null, tmp);
        usz after = tmp.stack_ctx_pool.pool_size;

        bool ok = c != null &&
                  c.tag == COROUTINE &&
                  r != null &&
                  r.tag == INT &&
                  r.int_val == 42 &&
                  lifetime_stack_ctx_release_ok(before, after);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: completed coroutine releases StackCtx after completion");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: completed coroutine releases StackCtx after completion (before=%d after=%d, coro_tag=%d, result_tag=%d)",
                (int)before, (int)after,
                c != null ? (int)c.tag : -1,
                r != null ? (int)r.tag : -1);
            (*fail)++;
        }

        tmp.destroy();
        mem::free(tmp);
    }

    {
        Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
        if (tmp == null) {
            io::printn("[FAIL] lifetime: closure wrapper escape alloc failure keeps source ownership (bootstrap alloc)");
            (*fail)++;
            return false;
        }
        tmp.init();
        if (!register_primitives(tmp)) {
            io::printn("[FAIL] lifetime: closure wrapper escape alloc failure keeps source ownership (bootstrap)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        SymbolId payload_sym = tmp.symbols.intern("__closure_escape_wrapper_alloc_payload");
        Env* captured_env = make_env(tmp, tmp.global_env);
        if (captured_env == null) {
            io::printn("[FAIL] lifetime: closure wrapper escape alloc failure keeps source ownership (env setup)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }
        env_define_with_barrier(captured_env, payload_sym, make_int(tmp, 313), tmp);

        main::ScopeRegion* detached_env_scope = null;
        Value* closure = make_env_scope_closure_for_test(tmp, captured_env, &detached_env_scope);
        if (closure == null || closure.tag != CLOSURE || detached_env_scope == null || closure.closure_val == null) {
            io::printn("[FAIL] lifetime: closure wrapper escape alloc failure keeps source ownership (closure setup)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        Closure* owned_closure = closure.closure_val;
        closure.closure_val = null;

        main::ScopeRegion* saved = tmp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: closure wrapper escape alloc failure keeps source ownership (failed to allocate child scope)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        tmp.current_scope = child;
        Value* local_owner = boundary_alloc_value_in_scope(tmp, child, true);
        local_owner.tag = CLOSURE;
        local_owner.closure_val = owned_closure;
        int saved_force = g_boundary_force_alloc_escape_value_fail_nth;
        g_boundary_force_alloc_escape_value_fail_nth = 1;
        Value* escaped = promote_to_escape(local_owner, tmp);
        g_boundary_force_alloc_escape_value_fail_nth = saved_force;

        bool fail_closed = escaped != null &&
                           escaped.tag == ERROR &&
                           local_owner.closure_val == owned_closure &&
                           local_owner.closure_val.env_scope == detached_env_scope &&
                           closure_env_lookup_int(local_owner, payload_sym, 313);

        tmp.current_scope = saved;
        main::scope_release(child);
        bool ok = fail_closed;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: closure wrapper escape alloc failure keeps source ownership");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: closure wrapper escape alloc failure keeps source ownership (fail_closed=%s refcount=%d local_has=%s)",
                fail_closed ? "yes" : "no",
                (int)detached_env_scope.refcount,
                local_owner.closure_val == owned_closure ? "yes" : "no");
            (*fail)++;
        }

        tmp.destroy();
        mem::free(tmp);
    }

    // Invariant: boundary copy to parent clones coroutine wrappers without
    // mutating the source wrapper in place, even for repeated copies.
    {
        Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
        if (tmp == null) {
            io::printn("[FAIL] lifetime: promote_to_root clones coroutine wrappers without source mutation (bootstrap alloc)");
            (*fail)++;
            return false;
        }
        tmp.init();
        if (!register_primitives(tmp)) {
            io::printn("[FAIL] lifetime: promote_to_root clones coroutine wrappers without source mutation (bootstrap)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        EvalResult r_thunk = run("(lambda () 42)", tmp);
        if (r_thunk.error.has_error || r_thunk.value == null) {
            io::printn("[FAIL] lifetime: promote_to_root clones coroutine wrappers without source mutation (failed to build thunk)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        Value*[1] coro_args;
        coro_args[0] = r_thunk.value;
        Value* base = prim_coroutine(coro_args[..], null, tmp);
        if (base == null || base.tag != COROUTINE || base.coroutine_val == null) {
            io::printn("[FAIL] lifetime: promote_to_root clones coroutine wrappers without source mutation (failed to build coroutine)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        main::StackCtx* owned_ctx = base.coroutine_val;
        base.coroutine_val = null;

        main::ScopeRegion* saved = tmp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: promote_to_root clones coroutine wrappers without source mutation (failed to allocate child scope)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        tmp.current_scope = child;
        Value* local_owner = boundary_alloc_value_in_scope(tmp, child, true);
        local_owner.tag = COROUTINE;
        local_owner.coroutine_val = owned_ctx;

        Value* promoted_a = promote_to_root(local_owner, tmp);
        Value* promoted_b = promote_to_root(local_owner, tmp);
        bool clone_ok = promoted_a != null &&
                        promoted_b != null &&
                        promoted_a.tag == COROUTINE &&
                        promoted_b.tag == COROUTINE &&
                        promoted_a != local_owner &&
                        promoted_b != local_owner &&
                        promoted_a.coroutine_val != null &&
                        promoted_b.coroutine_val != null &&
                        promoted_a.coroutine_val != owned_ctx &&
                        promoted_b.coroutine_val != owned_ctx &&
                        promoted_a.coroutine_val != promoted_b.coroutine_val &&
                        local_owner.coroutine_val == owned_ctx;

        usz before_release = tmp.stack_ctx_pool.pool_size;
        tmp.current_scope = saved;
        main::scope_release(child);
        usz after_release = tmp.stack_ctx_pool.pool_size;

        bool source_release_ok = lifetime_stack_ctx_release_ok(before_release, after_release);

        Value* resume_a = null;
        usz after_resume_a = after_release;
        if (promoted_a != null && promoted_a.tag == COROUTINE) {
            Value*[1] resume_args;
            resume_args[0] = promoted_a;
            resume_a = prim_resume(resume_args[..], null, tmp);
            after_resume_a = tmp.stack_ctx_pool.pool_size;
        }

        Value* resume_b = null;
        usz after_resume_b = after_resume_a;
        if (promoted_b != null && promoted_b.tag == COROUTINE) {
            Value*[1] resume_args;
            resume_args[0] = promoted_b;
            resume_b = prim_resume(resume_args[..], null, tmp);
            after_resume_b = tmp.stack_ctx_pool.pool_size;
        }

        bool resume_a_ok = resume_a != null &&
                           resume_a.tag == INT &&
                           resume_a.int_val == 42 &&
                           promoted_a != null &&
                           promoted_a.coroutine_val == null &&
                           lifetime_stack_ctx_release_ok(after_release, after_resume_a);
        bool resume_b_ok = resume_b != null &&
                           resume_b.tag == INT &&
                           resume_b.int_val == 42 &&
                           promoted_b != null &&
                           promoted_b.coroutine_val == null &&
                           lifetime_stack_ctx_release_ok(after_resume_a, after_resume_b);

        bool ok = clone_ok && source_release_ok && resume_a_ok && resume_b_ok;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: promote_to_root clones coroutine wrappers without source mutation");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: promote_to_root clones coroutine wrappers without source mutation (clone_ok=%s source_release_ok=%s resume_a_ok=%s resume_b_ok=%s release=%d->%d resume_a=%d resume_b=%d)",
                clone_ok ? "yes" : "no",
                source_release_ok ? "yes" : "no",
                resume_a_ok ? "yes" : "no",
                resume_b_ok ? "yes" : "no",
                (int)before_release,
                (int)after_release,
                (int)after_resume_a,
                (int)after_resume_b
            );
            (*fail)++;
        }

        tmp.destroy();
        mem::free(tmp);
    }

    {
        Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
        if (tmp == null) {
            io::printn("[FAIL] lifetime: promote_to_root coroutine wrapper alloc failure keeps source ownership (bootstrap alloc)");
            (*fail)++;
            return false;
        }
        tmp.init();
        if (!register_primitives(tmp)) {
            io::printn("[FAIL] lifetime: promote_to_root coroutine wrapper alloc failure keeps source ownership (bootstrap)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        EvalResult r_thunk = run("(lambda () 19)", tmp);
        if (r_thunk.error.has_error || r_thunk.value == null) {
            io::printn("[FAIL] lifetime: promote_to_root coroutine wrapper alloc failure keeps source ownership (failed to build thunk)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        Value*[1] coro_args;
        coro_args[0] = r_thunk.value;
        Value* base = prim_coroutine(coro_args[..], null, tmp);
        if (base == null || base.tag != COROUTINE || base.coroutine_val == null) {
            io::printn("[FAIL] lifetime: promote_to_root coroutine wrapper alloc failure keeps source ownership (failed to build coroutine)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        main::StackCtx* owned_ctx = base.coroutine_val;
        base.coroutine_val = null;

        main::ScopeRegion* saved = tmp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: promote_to_root coroutine wrapper alloc failure keeps source ownership (failed to allocate child scope)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        tmp.current_scope = child;
        Value* local_owner = boundary_alloc_value_in_scope(tmp, child, true);
        local_owner.tag = COROUTINE;
        local_owner.coroutine_val = owned_ctx;

        int saved_force = g_boundary_force_alloc_value_fail_nth;
        g_boundary_force_alloc_value_fail_nth = 1;
        Value* promoted = promote_to_root(local_owner, tmp);
        g_boundary_force_alloc_value_fail_nth = saved_force;

        usz before_release = tmp.stack_ctx_pool.pool_size;
        bool fail_closed = promoted != null &&
            promoted.tag == ERROR &&
            local_owner.coroutine_val == owned_ctx;

        tmp.current_scope = saved;
        main::scope_release(child);
        usz after_release = tmp.stack_ctx_pool.pool_size;

        bool ok = fail_closed && lifetime_stack_ctx_release_ok(before_release, after_release);
        if (ok) {
            emit_pass_literal("[PASS] lifetime: promote_to_root coroutine wrapper alloc failure keeps source ownership");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: promote_to_root coroutine wrapper alloc failure keeps source ownership");
            (*fail)++;
        }

        tmp.destroy();
        mem::free(tmp);
    }

    // Invariant: promote_to_escape transfers coroutine-wrapper ownership into ESCAPE lane.
    {
        Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
        if (tmp == null) {
            io::printn("[FAIL] lifetime: coroutine wrapper transfer on promote_to_escape (bootstrap alloc)");
            (*fail)++;
            return false;
        }
        tmp.init();
        if (!register_primitives(tmp)) {
            io::printn("[FAIL] lifetime: coroutine wrapper transfer on promote_to_escape (bootstrap)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        EvalResult r_thunk = run("(lambda () 7)", tmp);
        if (r_thunk.error.has_error || r_thunk.value == null) {
            io::printn("[FAIL] lifetime: coroutine wrapper transfer on promote_to_escape (failed to build thunk)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        Value*[1] coro_args;
        coro_args[0] = r_thunk.value;
        Value* base = prim_coroutine(coro_args[..], null, tmp);
        if (base == null || base.tag != COROUTINE || base.coroutine_val == null) {
            io::printn("[FAIL] lifetime: coroutine wrapper transfer on promote_to_escape (failed to build coroutine)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        main::StackCtx* owned_ctx = base.coroutine_val;
        base.coroutine_val = null;

        main::ScopeRegion* saved = tmp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: coroutine wrapper transfer on promote_to_escape (failed to allocate child scope)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        tmp.current_scope = child;
        Value* local_owner = boundary_alloc_value_in_scope(tmp, child, true);
        local_owner.tag = COROUTINE;
        local_owner.coroutine_val = owned_ctx;

        usz before = tmp.stack_ctx_pool.pool_size;
        Value* escaped = promote_to_escape(local_owner, tmp);
        bool transfer_ok = escaped != null &&
                           escaped.tag == COROUTINE &&
                           escaped.scope_gen == child.escape_generation &&
                           escaped.coroutine_val == owned_ctx &&
                           local_owner.coroutine_val == null;

        tmp.current_scope = saved;
        main::scope_release(child);
        usz after = tmp.stack_ctx_pool.pool_size;

        bool ok = transfer_ok && lifetime_stack_ctx_release_ok(before, after);
        if (ok) {
            emit_pass_literal("[PASS] lifetime: promote_to_escape transfers coroutine wrapper ownership");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: promote_to_escape transfers coroutine wrapper ownership");
            (*fail)++;
        }

        tmp.destroy();
        mem::free(tmp);
    }

    {
        Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
        if (tmp == null) {
            io::printn("[FAIL] lifetime: coroutine wrapper escape alloc failure keeps source ownership (bootstrap alloc)");
            (*fail)++;
            return false;
        }
        tmp.init();
        if (!register_primitives(tmp)) {
            io::printn("[FAIL] lifetime: coroutine wrapper escape alloc failure keeps source ownership (bootstrap)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        EvalResult r_thunk = run("(lambda () 23)", tmp);
        if (r_thunk.error.has_error || r_thunk.value == null) {
            io::printn("[FAIL] lifetime: coroutine wrapper escape alloc failure keeps source ownership (failed to build thunk)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        Value*[1] coro_args;
        coro_args[0] = r_thunk.value;
        Value* base = prim_coroutine(coro_args[..], null, tmp);
        if (base == null || base.tag != COROUTINE || base.coroutine_val == null) {
            io::printn("[FAIL] lifetime: coroutine wrapper escape alloc failure keeps source ownership (failed to build coroutine)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        main::StackCtx* owned_ctx = base.coroutine_val;
        base.coroutine_val = null;

        main::ScopeRegion* saved = tmp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: coroutine wrapper escape alloc failure keeps source ownership (failed to allocate child scope)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        tmp.current_scope = child;
        Value* local_owner = boundary_alloc_value_in_scope(tmp, child, true);
        local_owner.tag = COROUTINE;
        local_owner.coroutine_val = owned_ctx;

        bool saved_force = g_boundary_force_escape_coroutine_wrapper_alloc_fail;
        g_boundary_force_escape_coroutine_wrapper_alloc_fail = true;
        usz before_release = tmp.stack_ctx_pool.pool_size;
        Value* escaped = promote_to_escape(local_owner, tmp);
        g_boundary_force_escape_coroutine_wrapper_alloc_fail = saved_force;

        bool fail_closed = escaped != null &&
            escaped.tag == ERROR &&
            local_owner.coroutine_val == owned_ctx;

        tmp.current_scope = saved;
        main::scope_release(child);
        usz after_release = tmp.stack_ctx_pool.pool_size;

        bool ok = fail_closed && lifetime_stack_ctx_release_ok(before_release, after_release);
        if (ok) {
            emit_pass_literal("[PASS] lifetime: coroutine wrapper escape alloc failure keeps source ownership");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: coroutine wrapper escape alloc failure keeps source ownership");
            (*fail)++;
        }

        tmp.destroy();
        mem::free(tmp);
    }

    // Invariant: handler clauses that ignore k must not retain suspended handle contexts.
    {
        Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
        if (tmp == null) {
            io::printn("[FAIL] lifetime: handler clause teardown releases suspended context (bootstrap alloc)");
            (*fail)++;
            return false;
        }
        tmp.init();
        if (!register_primitives(tmp)) {
            io::printn("[FAIL] lifetime: handler clause teardown releases suspended context (bootstrap)");
            (*fail)++;
            tmp.destroy();
            mem::free(tmp);
            return false;
        }

        usz before = tmp.stack_ctx_pool.pool_size;
        EvalResult r = run("(handle (signal ask 1) (ask k x 99))", tmp);
        usz after = tmp.stack_ctx_pool.pool_size;

        bool ok = !r.error.has_error &&
                  r.value != null &&
                  r.value.tag == INT &&
                  r.value.int_val == 99 &&
                  lifetime_stack_ctx_release_ok(before, after);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: handle ignore-k does not retain suspended context");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: handle ignore-k does not retain suspended context (before=%d after=%d)",
                (int)before, (int)after);
            (*fail)++;
        }

        tmp.destroy();
        mem::free(tmp);
    }

    return true;
}
