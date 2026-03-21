# Boundary Surface Audit

Generated: 2026-03-10T04:55:26Z
Policy file: `scripts/boundary_facade_policy.txt`

## Summary

- total direct legacy callsites scanned: 33
- allowed by policy: 19
- ignored by policy globs: 14
- violations: 0

## Allowed Callsites

```text
src/lisp/eval_promotion_copy.c3:17:        current_new.cons_val.car = copy_to_parent(current_old.cons_val.car, interp);
src/lisp/eval_promotion_copy.c3:32:            current_new.cons_val.cdr = copy_to_parent(old_cdr, interp);
src/lisp/eval_promotion_copy.c3:135:    result.partial_val.first_arg = copy_to_parent(v.partial_val.first_arg, interp);
src/lisp/eval_promotion_copy.c3:136:    result.partial_val.second_arg = copy_to_parent(v.partial_val.second_arg, interp);
src/lisp/eval_promotion_copy.c3:148:    Value* promoted_thunk = copy_to_parent(v.iterator_val, interp);
src/lisp/eval_promotion_copy.c3:293:fn Value* copy_to_parent(Value* v, Interp* interp) {
src/lisp/eval_promotion_copy.c3:317:    return copy_to_parent(v, interp);
src/lisp/eval_promotion_escape.c3:90:        current_new.cons_val.car = promote_to_escape(current_old.cons_val.car, interp);
src/lisp/eval_promotion_escape.c3:101:            current_new.cons_val.cdr = promote_to_escape(old_cdr, interp);
src/lisp/eval_promotion_escape.c3:147:    result.partial_val.first_arg = promote_to_escape(v.partial_val.first_arg, interp);
src/lisp/eval_promotion_escape.c3:148:    result.partial_val.second_arg = promote_to_escape(v.partial_val.second_arg, interp);
src/lisp/eval_promotion_escape.c3:232:    result.iterator_val = promote_to_escape(v.iterator_val, interp);
src/lisp/eval_promotion_escape.c3:359:fn Value* promote_to_escape(Value* v, Interp* interp) {
src/lisp/eval_boundary_api.c3:534:    return promote_to_escape(v, interp);
src/lisp/eval_promotion_escape.c3:725:fn Value* promote_to_root(Value* v, Interp* interp) {
src/lisp/eval_boundary_api.c3:548:    return promote_to_root(v, interp);
src/lisp/eval_env_copy.c3:251:fn BoundaryEnvCopyResult copy_env_to_scope_checked(
src/lisp/eval_boundary_api.c3:560:    return copy_env_to_scope_checked(env, interp, depth, ctx);
src/lisp/eval_boundary_provenance.c3:186:        main::scope_splice_escapes(parent, child);
```

## Ignored Callsites

```text
src/lisp/tests_memory_lifetime_env_copy_groups.c3:68:        Value* copied = setup_ok ? copy_to_parent(closure, interp) : null;
src/lisp/tests_memory_lifetime_env_copy_groups.c3:465:            Value* copied = setup_ok ? copy_to_parent(closure, interp) : null;
src/lisp/tests_memory_lifetime_boundary_groups.c3:394:        Value* reused = copy_to_parent(reusable, interp);
src/lisp/tests_memory_lifetime_boundary_groups.c3:395:        Value* copied = copy_to_parent(disjoint, interp);
src/lisp/tests_memory_lifetime_promotion_context_groups.c3:127:        Value* promoted = promote_to_escape(src, interp);
src/lisp/tests_memory_lifetime_promotion_context_groups.c3:345:            Value* promoted = promote_to_escape(src, interp);
src/lisp/tests_memory_lifetime_boundary_graph_txn_groups.c3:75:            Value* escape_root = promote_to_escape(temp_list, interp);
src/lisp/tests_memory_lifetime_boundary_graph_txn_groups.c3:1323:            Value* escaped_first = promote_to_escape(make_int(interp, (long)(i + 501)), interp);
src/lisp/tests_memory_lifetime_boundary_graph_txn_groups.c3:1324:            Value* escaped_second = promote_to_escape(make_int(interp, (long)(i + 901)), interp);
src/lisp/tests_memory_lifetime_groups.c3:444:        Value* esc_list = promote_to_escape(tmp_list, interp);
src/lisp/tests_memory_lifetime_groups.c3:1079:        Value* escaped = promote_to_escape(src, interp);
src/lisp/tests_memory_lifetime_boundary_commit_groups.c3:42:            Value* escape_owned = promote_to_escape(escape_seed, interp);
src/lisp/tests_memory_lifetime_boundary_commit_groups.c3:491:            Value* escaped_first = promote_to_escape(make_int(interp, 1201), interp);
src/lisp/tests_memory_lifetime_boundary_commit_groups.c3:492:            Value* escaped_second = promote_to_escape(make_int(interp, 1202), interp);
```

## Violations

```text
<none>
```
