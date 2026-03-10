module lisp;

import std::io;
import main;

fn void run_memory_lifetime_boundary_policy_parse_tests(Interp* interp, int* pass, int* fail) {
    (void)interp;

    // Invariant: boundary policy parser accepts explicit true/false tokens and rejects ambiguous strings.
    {
        bool parsed = false;
        BoundaryPolicyParseStatus s_true =
            boundary_policy_parse_bool_token((char*)" true ", &parsed);
        bool true_ok = s_true == BOUNDARY_POLICY_PARSE_OK && parsed;

        BoundaryPolicyParseStatus s_false =
            boundary_policy_parse_bool_token((char*)"off", &parsed);
        bool false_ok = s_false == BOUNDARY_POLICY_PARSE_OK && !parsed;

        BoundaryPolicyParseStatus s_invalid =
            boundary_policy_parse_bool_token((char*)"truthy", &parsed);
        bool invalid_ok = s_invalid == BOUNDARY_POLICY_PARSE_INVALID;

        BoundaryPolicyParseStatus s_missing =
            boundary_policy_parse_bool_token((char*)"   ", &parsed);
        bool missing_ok = s_missing == BOUNDARY_POLICY_PARSE_MISSING;

        bool ok = true_ok && false_ok && invalid_ok && missing_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary policy bool parser strict tokens");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: boundary policy bool parser strict tokens");
            (*fail)++;
        }
    }

    // Invariant: boundary policy parser accepts only trimmed decimal usize tokens.
    {
        usz parsed = 0;
        BoundaryPolicyParseStatus s_ok =
            boundary_policy_parse_usize_token((char*)" 4096 ", &parsed);
        bool ok_value = s_ok == BOUNDARY_POLICY_PARSE_OK && parsed == 4096;

        BoundaryPolicyParseStatus s_invalid =
            boundary_policy_parse_usize_token((char*)"16k", &parsed);
        bool invalid_ok = s_invalid == BOUNDARY_POLICY_PARSE_INVALID;

        BoundaryPolicyParseStatus s_missing =
            boundary_policy_parse_usize_token((char*)"", &parsed);
        bool missing_ok = s_missing == BOUNDARY_POLICY_PARSE_MISSING;

        bool ok = ok_value && invalid_ok && missing_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary policy usize parser strict tokens");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: boundary policy usize parser strict tokens");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_boundary_graph_audit_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: graph audit accepts an ESCAPE-root graph with no TEMP-reachable edges.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        if (target_scope == null) {
            io::printn("[FAIL] lifetime: boundary graph audit accepts escape graph");
            (*fail)++;
        } else {
            interp.current_scope = target_scope;
            Value* temp_list = boundary_commit_make_temp_int_list(interp, 8);
            Value* escape_root = promote_to_escape(temp_list, interp);

            BoundaryGraphAuditResult audit = boundary_graph_audit_escape_reachability_result(escape_root, target_scope);
            bool ok = escape_root != null &&
                      escape_root.scope_gen == target_scope.escape_generation &&
                      audit.ok &&
                      audit.reason == BOUNDARY_GRAPH_AUDIT_OK &&
                      audit.root_tag == (usz)escape_root.tag &&
                      audit.violating_edge_tag == BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE &&
                      audit.root_scope_gen == (usz)escape_root.scope_gen &&
                      audit.violating_scope_gen == 0 &&
                      audit.target_scope_gen == (usz)target_scope.generation &&
                      audit.target_escape_gen == (usz)target_scope.escape_generation;

            interp.current_scope = saved_scope;
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary graph audit accepts escape graph");
                (*pass)++;
            } else {
                io::printn("[FAIL] lifetime: boundary graph audit accepts escape graph");
                (*fail)++;
            }
        }
    }

    // Invariant: graph audit rejects an ESCAPE-root graph that reaches TEMP-lane values.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        if (target_scope == null) {
            io::printn("[FAIL] lifetime: boundary graph audit rejects temp edge");
            (*fail)++;
        } else {
            interp.current_scope = target_scope;
            Value* temp_leaf = make_int(interp, 901);
            Value* escape_nil = interp.alloc_value_escape();
            Value* escape_root = interp.alloc_value_escape();

            if (escape_nil != null) escape_nil.tag = NIL;
            if (escape_root != null) {
                escape_root.tag = CONS;
                escape_root.cons_val.car = temp_leaf;
                escape_root.cons_val.cdr = escape_nil;
            }

            BoundaryGraphAuditResult audit = boundary_graph_audit_escape_reachability_result(escape_root, target_scope);
            bool ok = escape_root != null &&
                      temp_leaf != null &&
                      escape_root.scope_gen == target_scope.escape_generation &&
                      temp_leaf.scope_gen == target_scope.generation &&
                      !audit.ok &&
                      audit.reason == BOUNDARY_GRAPH_AUDIT_REACHABLE_TEMP_VALUE &&
                      audit.root_tag == (usz)escape_root.tag &&
                      audit.violating_edge_tag == (usz)temp_leaf.tag &&
                      audit.root_scope_gen == (usz)escape_root.scope_gen &&
                      audit.violating_scope_gen == (usz)temp_leaf.scope_gen &&
                      audit.target_scope_gen == (usz)target_scope.generation &&
                      audit.target_escape_gen == (usz)target_scope.escape_generation;

            interp.current_scope = saved_scope;
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary graph audit rejects temp edge");
                (*pass)++;
            } else {
                io::printn("[FAIL] lifetime: boundary graph audit rejects temp edge");
                (*fail)++;
            }
        }
    }

    // Invariant: opaque wrappers are excluded from Omni-owned edge traversal.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        if (target_scope == null) {
            io::printn("[FAIL] lifetime: boundary graph audit opaque wrapper exclusion");
            (*fail)++;
        } else {
            interp.current_scope = target_scope;
            Value* opaque = interp.alloc_value_escape();
            if (opaque != null) {
                opaque.tag = FFI_HANDLE;
                opaque.ffi_val = null;
            }

            BoundaryGraphAuditResult audit = boundary_graph_audit_escape_reachability_result(opaque, target_scope);
            bool ok = opaque != null &&
                      opaque.scope_gen == target_scope.escape_generation &&
                      audit.ok &&
                      audit.reason == BOUNDARY_GRAPH_AUDIT_OK &&
                      audit.root_tag == (usz)opaque.tag &&
                      audit.violating_edge_tag == BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE &&
                      audit.root_scope_gen == (usz)opaque.scope_gen &&
                      audit.violating_scope_gen == 0 &&
                      audit.target_scope_gen == (usz)target_scope.generation &&
                      audit.target_escape_gen == (usz)target_scope.escape_generation;

            interp.current_scope = saved_scope;
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary graph audit opaque wrapper exclusion");
                (*pass)++;
            } else {
                io::printn("[FAIL] lifetime: boundary graph audit opaque wrapper exclusion");
                (*fail)++;
            }
        }
    }

    // Invariant: deep nested aggregate graphs pass under gated audit mode.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        if (target_scope == null) {
            io::printn("[FAIL] lifetime: boundary graph audit deep nested aggregate");
            (*fail)++;
        } else {
            BoundaryRuntimePolicy saved_policy = g_boundary_runtime_policy;
            g_boundary_runtime_policy.initialized = true;
            g_boundary_runtime_policy.graph_audit = true;
            g_boundary_runtime_policy.graph_audit_rate = 1;
            g_boundary_runtime_policy.graph_audit_max_roots = 0;

            interp.current_scope = target_scope;
            Value* root = interp.alloc_value_escape();
            Value* tail = interp.alloc_value_escape();
            if (tail != null) tail.tag = NIL;
            if (root != null) {
                root.tag = CONS;
                root.cons_val.car = tail;
                root.cons_val.cdr = tail;
            }

            const usz DEPTH = 1024;
            bool alloc_ok = root != null && tail != null;
            for (usz i = 0; alloc_ok && i < DEPTH; i++) {
                Value* leaf = interp.alloc_value_escape();
                Value* cell = interp.alloc_value_escape();
                if (leaf == null || cell == null) {
                    alloc_ok = false;
                    break;
                }
                leaf.tag = INT;
                leaf.int_val = (long)i;

                cell.tag = CONS;
                cell.cons_val.car = leaf;
                cell.cons_val.cdr = root.cons_val.cdr;
                root.cons_val.cdr = cell;
            }

            BoundaryGraphAuditResult audit =
                boundary_graph_audit_escape_reachability_result(root, target_scope, true);
            bool ok = alloc_ok &&
                      audit.ok &&
                      audit.reason == BOUNDARY_GRAPH_AUDIT_OK &&
                      audit.root_tag == (usz)root.tag &&
                      audit.violating_edge_tag == BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE &&
                      audit.target_scope_gen == (usz)target_scope.generation &&
                      audit.target_escape_gen == (usz)target_scope.escape_generation;

            g_boundary_runtime_policy = saved_policy;
            interp.current_scope = saved_scope;
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary graph audit deep nested aggregate");
                (*pass)++;
            } else {
                io::printn("[FAIL] lifetime: boundary graph audit deep nested aggregate");
                (*fail)++;
            }
        }
    }
}

fn void run_memory_lifetime_boundary_txn_protocol_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: transition policy rejects protocol misuse states.
    {
        bool matrix_ok =
            !boundary_txn_transition_allowed(BOUNDARY_TXN_INIT, BOUNDARY_TXN_ACTION_COMMIT) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_INIT, BOUNDARY_TXN_ACTION_ABORT) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_INIT, BOUNDARY_TXN_ACTION_CLOSE) &&
            boundary_txn_transition_allowed(BOUNDARY_TXN_OPEN, BOUNDARY_TXN_ACTION_COMMIT) &&
            boundary_txn_transition_allowed(BOUNDARY_TXN_OPEN, BOUNDARY_TXN_ACTION_ABORT) &&
            boundary_txn_transition_allowed(BOUNDARY_TXN_OPEN, BOUNDARY_TXN_ACTION_CLOSE) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_COMMITTED, BOUNDARY_TXN_ACTION_COMMIT) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_COMMITTED, BOUNDARY_TXN_ACTION_ABORT) &&
            boundary_txn_transition_allowed(BOUNDARY_TXN_COMMITTED, BOUNDARY_TXN_ACTION_CLOSE) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_ABORTED, BOUNDARY_TXN_ACTION_COMMIT) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_ABORTED, BOUNDARY_TXN_ACTION_ABORT) &&
            boundary_txn_transition_allowed(BOUNDARY_TXN_ABORTED, BOUNDARY_TXN_ACTION_CLOSE) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_CLOSED, BOUNDARY_TXN_ACTION_COMMIT) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_CLOSED, BOUNDARY_TXN_ACTION_ABORT) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_CLOSED, BOUNDARY_TXN_ACTION_CLOSE);

        if (matrix_ok) {
            emit_pass_literal("[PASS] lifetime: boundary txn transition policy matrix");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: boundary txn transition policy matrix");
            (*fail)++;
        }
    }

    // Invariant: open->commit->close is valid and session close restores state.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;

        BoundaryTxn txn = boundary_txn_begin(interp);
        bool open_ok = txn.state == BOUNDARY_TXN_OPEN &&
                        txn.session.active &&
                        txn.session.current_scope == saved_scope &&
                        txn.session.releasing_scope == saved_releasing;
        boundary_txn_commit(&txn);
        bool commit_ok = txn.state == BOUNDARY_TXN_COMMITTED &&
                         txn.session.active;
        boundary_txn_close(interp, &txn);
        bool close_ok = txn.state == BOUNDARY_TXN_CLOSED &&
                        !txn.session.active &&
                        interp.current_scope == saved_scope &&
                        interp.releasing_scope == saved_releasing;

        bool ok = open_ok && commit_ok && close_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary txn commit protocol");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary txn commit protocol (open_ok=%s commit_ok=%s close_ok=%s)",
                open_ok ? "yes" : "no",
                commit_ok ? "yes" : "no",
                close_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }

    // Invariant: open->abort->close is valid and session close restores state.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;

        BoundaryTxn txn = boundary_txn_begin(interp);
        bool open_ok = txn.state == BOUNDARY_TXN_OPEN &&
                        txn.session.active &&
                        txn.session.current_scope == saved_scope &&
                        txn.session.releasing_scope == saved_releasing;
        boundary_txn_abort(&txn);
        bool abort_ok = txn.state == BOUNDARY_TXN_ABORTED &&
                        txn.session.active;
        boundary_txn_close(interp, &txn);
        bool close_ok = txn.state == BOUNDARY_TXN_CLOSED &&
                        !txn.session.active &&
                        interp.current_scope == saved_scope &&
                        interp.releasing_scope == saved_releasing;

        bool ok = open_ok && abort_ok && close_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary txn abort protocol");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary txn abort protocol (open_ok=%s abort_ok=%s close_ok=%s)",
                open_ok ? "yes" : "no",
                abort_ok ? "yes" : "no",
                close_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }

    // Invariant: double-commit is blocked by protocol guard path.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;

        BoundaryTxn txn = boundary_txn_begin(interp);
        bool first_commit_ok = boundary_txn_try_commit(&txn);
        bool second_commit_blocked = !boundary_txn_try_commit(&txn);
        boundary_txn_close(interp, &txn);
        bool close_ok = txn.state == BOUNDARY_TXN_CLOSED &&
                        !txn.session.active &&
                        interp.current_scope == saved_scope &&
                        interp.releasing_scope == saved_releasing;

        bool ok = first_commit_ok && second_commit_blocked && close_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary txn double-commit guard");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary txn double-commit guard (first_commit_ok=%s second_commit_blocked=%s close_ok=%s)",
                first_commit_ok ? "yes" : "no",
                second_commit_blocked ? "yes" : "no",
                close_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }

    // Invariant: commit after abort is blocked by protocol guard path.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;

        BoundaryTxn txn = boundary_txn_begin(interp);
        bool abort_ok = boundary_txn_try_abort(&txn);
        bool commit_after_abort_blocked = !boundary_txn_try_commit(&txn);
        boundary_txn_close(interp, &txn);
        bool close_ok = txn.state == BOUNDARY_TXN_CLOSED &&
                        !txn.session.active &&
                        interp.current_scope == saved_scope &&
                        interp.releasing_scope == saved_releasing;

        bool ok = abort_ok && commit_after_abort_blocked && close_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary txn abort-then-commit guard");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary txn abort-then-commit guard (abort_ok=%s commit_after_abort_blocked=%s close_ok=%s)",
                abort_ok ? "yes" : "no",
                commit_after_abort_blocked ? "yes" : "no",
                close_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }

    // Invariant: close-before-open and double-close are rejected by guard path.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;

        BoundaryTxn unbegun = {};
        bool close_before_open_blocked = !boundary_txn_try_close(interp, &unbegun);

        BoundaryTxn txn = boundary_txn_begin(interp);
        bool commit_ok = boundary_txn_try_commit(&txn);
        bool first_close_ok = boundary_txn_try_close(interp, &txn);
        bool second_close_blocked = !boundary_txn_try_close(interp, &txn);
        bool state_ok = txn.state == BOUNDARY_TXN_CLOSED && !txn.session.active;
        bool scope_ok = interp.current_scope == saved_scope &&
                        interp.releasing_scope == saved_releasing;

        bool ok = close_before_open_blocked &&
                  commit_ok &&
                  first_close_ok &&
                  second_close_blocked &&
                  state_ok &&
                  scope_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary txn close guard policy");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary txn close guard policy (close_before_open_blocked=%s commit_ok=%s first_close_ok=%s second_close_blocked=%s state_ok=%s scope_ok=%s)",
                close_before_open_blocked ? "yes" : "no",
                commit_ok ? "yes" : "no",
                first_close_ok ? "yes" : "no",
                second_close_blocked ? "yes" : "no",
                state_ok ? "yes" : "no",
                scope_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}

const usz BOUNDARY_BENCH_DECISION_ITERS = 2048;
const usz BOUNDARY_BENCH_SPLICE_ITERS = 2048;
const usz BOUNDARY_BENCH_SPLICE_ESCAPES_PER_ITER = 64;
const usz AST_ARENA_BENCH_EXPR_ALLOCS = 131072;
const usz AST_ARENA_BENCH_PATTERN_ALLOCS = 65536;
const usz AST_ARENA_BENCH_RESET_INTERVAL = 8192;

fn bool boundary_bench_enabled() @inline {
    return boundary_c_getenv("OMNI_BOUNDARY_BENCH") != null;
}

fn bool ast_arena_bench_enabled() @inline {
    return boundary_c_getenv("OMNI_AST_ARENA_BENCH") != null;
}

fn bool ast_arena_bench_assertions_enabled() @inline {
    return boundary_c_getenv("OMNI_AST_ARENA_BENCH_ASSERT") != null;
}

fn bool ast_arena_bench_mutation_enabled() @inline {
    return boundary_c_getenv("OMNI_AST_ARENA_BENCH_MUT") != null;
}

fn bool ast_arena_bench_reset_enabled() @inline {
    return boundary_c_getenv("OMNI_AST_ARENA_BENCH_RESET") != null;
}

macro ast_arena_bench_summary_alloc(expr_iters, expr_ok, expr_ms, pattern_iters, pattern_ok, pattern_ms, resets, blocks, cap, ast_valid_fail) {
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=ast_arena_alloc expr_allocs=%d expr_ok=%d expr_ms=%d pattern_allocs=%d pattern_ok=%d pattern_ms=%d arena_blocks=%d arena_capacity=%d ast_resets=%d ast_validation_fail=%d",
        (long)(expr_iters),
        (long)(expr_ok),
        (long)(expr_ms),
        (long)(pattern_iters),
        (long)(pattern_ok),
        (long)(pattern_ms),
        (long)(resets),
        (long)(blocks),
        (long)(cap),
        (long)(ast_valid_fail)
    );
}

macro ast_arena_bench_summary_alloc_reset(expr_iters, expr_ok, expr_ms, pattern_iters, pattern_ok, pattern_ms, resets, blocks, cap, ast_valid_fail) {
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=ast_arena_alloc_reset expr_allocs=%d expr_ok=%d expr_ms=%d pattern_allocs=%d pattern_ok=%d pattern_ms=%d arena_blocks=%d arena_capacity=%d ast_resets=%d ast_validation_fail=%d",
        (long)(expr_iters),
        (long)(expr_ok),
        (long)(expr_ms),
        (long)(pattern_iters),
        (long)(pattern_ok),
        (long)(pattern_ms),
        (long)(resets),
        (long)(blocks),
        (long)(cap),
        (long)(ast_valid_fail)
    );
}

macro ast_arena_bench_summary_parser(parse_iters, parse_ms, parse_ok, parse_failed, parse_owned_failed, parse_mut_failed, parse_exprs, ast_blocks, parse_resets, ast_valid_fail) {
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=ast_parser_smoke iters=%d parse_ms=%d parse_ok=%d parse_failed=%d parse_owned_failed=%d parse_mut_failed=%d exprs=%d ast_blocks=%d ast_resets=%d ast_validation_fail=%d",
        (long)(parse_iters),
        (long)(parse_ms),
        (long)(parse_ok),
        (long)(parse_failed),
        (long)(parse_owned_failed),
        (long)(parse_mut_failed),
        (long)(parse_exprs),
        (long)(ast_blocks),
        (long)(parse_resets),
        (long)(ast_valid_fail)
    );
}

macro ast_arena_bench_summary_parser_reset(parse_iters, parse_ms, parse_ok, parse_failed, parse_owned_failed, parse_mut_failed, parse_exprs, ast_blocks, parse_resets, ast_valid_fail) {
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=ast_parser_smoke_reset iters=%d parse_ms=%d parse_ok=%d parse_failed=%d parse_owned_failed=%d parse_mut_failed=%d exprs=%d ast_blocks=%d ast_resets=%d ast_validation_fail=%d",
        (long)(parse_iters),
        (long)(parse_ms),
        (long)(parse_ok),
        (long)(parse_failed),
        (long)(parse_owned_failed),
        (long)(parse_mut_failed),
        (long)(parse_exprs),
        (long)(ast_blocks),
        (long)(parse_resets),
        (long)(ast_valid_fail)
    );
}

macro ast_arena_bench_summary_compile(
    compile_iters,
    compile_ms,
    compile_ok,
    compile_failed,
    compile_empty,
    compile_owned_failed,
    output_bytes,
    ast_blocks,
    compile_resets,
    ast_valid_fail
) {
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=ast_compiler_smoke iters=%d compile_ms=%d compile_ok=%d compile_failed=%d compile_empty=%d compile_owned_failed=%d output_bytes=%d ast_blocks=%d ast_resets=%d ast_validation_fail=%d",
        (long)(compile_iters),
        (long)(compile_ms),
        (long)(compile_ok),
        (long)(compile_failed),
        (long)(compile_empty),
        (long)(compile_owned_failed),
        (long)(output_bytes),
        (long)(ast_blocks),
        (long)(compile_resets),
        (long)(ast_valid_fail)
    );
}

macro ast_arena_bench_summary_compile_reset(
    compile_iters,
    compile_ms,
    compile_ok,
    compile_failed,
    compile_empty,
    compile_owned_failed,
    output_bytes,
    ast_blocks,
    compile_resets,
    ast_valid_fail
) {
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=ast_compiler_smoke_reset iters=%d compile_ms=%d compile_ok=%d compile_failed=%d compile_empty=%d compile_owned_failed=%d output_bytes=%d ast_blocks=%d ast_resets=%d ast_validation_fail=%d",
        (long)(compile_iters),
        (long)(compile_ms),
        (long)(compile_ok),
        (long)(compile_failed),
        (long)(compile_empty),
        (long)(compile_owned_failed),
        (long)(output_bytes),
        (long)(ast_blocks),
        (long)(compile_resets),
        (long)(ast_valid_fail)
    );
}

macro ast_arena_bench_summary_macro(macro_iters, macro_ms, macro_ok, macro_failed, macro_not_expanded, macro_owned_failed, macro_mutation_failed, macro_resets, ast_valid_fail) {
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=ast_macro_smoke iters=%d macro_ms=%d macro_ok=%d macro_failed=%d macro_not_expanded=%d macro_owned_failed=%d macro_mutation_failed=%d ast_resets=%d ast_validation_fail=%d",
        (long)(macro_iters),
        (long)(macro_ms),
        (long)(macro_ok),
        (long)(macro_failed),
        (long)(macro_not_expanded),
        (long)(macro_owned_failed),
        (long)(macro_mutation_failed),
        (long)(macro_resets),
        (long)(ast_valid_fail)
    );
}

macro ast_arena_bench_summary_macro_reset(macro_iters, macro_ms, macro_ok, macro_failed, macro_not_expanded, macro_owned_failed, macro_mutation_failed, macro_resets, ast_valid_fail) {
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=ast_macro_smoke_reset iters=%d macro_ms=%d macro_ok=%d macro_failed=%d macro_not_expanded=%d macro_owned_failed=%d macro_mutation_failed=%d ast_resets=%d ast_validation_fail=%d",
        (long)(macro_iters),
        (long)(macro_ms),
        (long)(macro_ok),
        (long)(macro_failed),
        (long)(macro_not_expanded),
        (long)(macro_owned_failed),
        (long)(macro_mutation_failed),
        (long)(macro_resets),
        (long)(ast_valid_fail)
    );
}

fn char[] ast_arena_bench_parser_source(usz i) @inline {
    switch (i & 3) {
        case 0: return "(let ((x 10) (y 20)) (+ x y))";
        case 1: return "(match (list 1 2 3) ([a b c] (+ a (+ b c))))";
        case 2: return "(define [macro] double ([x] (+ x x)))";
        default: return "(cond ((> 7 3) (+ 1 2)) (else (- 3 1)))";
    }
}

fn char[] ast_arena_bench_compile_source(usz i) @inline {
    switch (i & 3) {
        case 0: return "(define (square x) (* x x)) (square 9)";
        case 1: return "(define [macro] double ([x] (+ x x))) (double 21)";
        case 2: return "(block (define total 0) (if (> 3 1) (set! total (+ total 1)) 0) total)";
        default: return "(let (fib (lambda (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))) ) (fib 10))";
    }
}

fn char[] ast_arena_bench_macro_source(usz i) @inline {
    switch (i & 3) {
        case 0: return "(block (define [macro] double ([x] (+ x x))) (double 6))";
        case 1: return "(block (define [macro] inc ([x] (+ x 1))) (inc 41))";
        case 2: return "(block (define [macro] swap ([a b] (list b a))) (swap 1 2))";
        default: return "(block (define [macro] when-positive ([x expr]) (if (> x 0) expr 0)) (when-positive 3 7))";
    }
}

fn SymbolId ast_arena_bench_macro_sym(Interp* interp, usz i) @inline {
    switch (i & 3) {
        case 0: return interp.symbols.intern("double");
        case 1: return interp.symbols.intern("inc");
        case 2: return interp.symbols.intern("swap");
        default: return interp.symbols.intern("when-positive");
    }
}

fn Expr* ast_arena_bench_mutate_expr(Expr* expr) @inline {
    if (expr == null) return null;
    expr.loc_line = expr.loc_line + 1;
    return expr;
}

fn bool ast_arena_bench_macro_expanded(Expr* root, SymbolId macro_sym) @inline {
    if (root == null) return false;

    Expr* target = root;
    if (root.tag == E_BLOCK && root.block != null && root.block.expr_count > 0) {
        target = root.block.exprs[root.block.expr_count - 1];
    }

    if (target == null) return false;
    if (target.tag != E_CALL) return true;
    if (target.call == null) return false;
    if (target.call.func == null) return false;
    if (target.call.func.tag != E_VAR) return true;
    return target.call.func.var_expr.name != macro_sym;
}

struct AstArenaBenchAllocResult {
    long expr_ms;
    long pattern_ms;
    usz expr_ok;
    usz pattern_ok;
    usz ast_resets;
    usz expr_count;
    usz pattern_count;
    usz ast_validation_failures;
}

struct AstArenaBenchCompileResult {
    bool parse_ok;
    bool compile_ok;
    bool ast_owned;
    bool mutated;
    usz output_len;
    Expr* parsed;
}

struct AstArenaBenchParserSuiteResult {
    long parse_ms;
    usz parse_ok;
    usz parse_failed;
    usz parse_owned_failed;
    usz parse_mutation_failed;
    usz exprs;
    usz ast_blocks;
    usz ast_resets;
    usz ast_validation_failures;
}

struct AstArenaBenchCompileSuiteResult {
    long compile_ms;
    usz compile_ok;
    usz compile_failed;
    usz compile_empty;
    usz compile_owned_failed;
    usz compile_mutation_failed;
    usz output_bytes;
    usz ast_blocks;
    usz ast_resets;
    usz ast_validation_failures;
}

struct AstArenaBenchMacroSuiteResult {
    long macro_ms;
    usz macro_ok;
    usz macro_failed;
    usz macro_not_expanded;
    usz macro_owned_failed;
    usz macro_mutation_failed;
    usz ast_blocks;
    usz ast_resets;
    usz ast_validation_failures;
}

fn bool ast_arena_bench_should_reset(usz iter, bool reset_enabled) @inline {
    if (!reset_enabled) return false;
    if (AST_ARENA_BENCH_RESET_INTERVAL == 0) return false;
    return iter > 0 && (iter % AST_ARENA_BENCH_RESET_INTERVAL) == 0;
}

fn AstArenaBenchAllocResult ast_arena_bench_run_alloc(
    AstArena* arena,
    usz expr_count,
    usz pattern_count,
    bool reset_mode
) {
    AstArenaBenchAllocResult result = {
        .expr_count = expr_count,
        .pattern_count = pattern_count,
    };

    long start_ms = boundary_bench_now_ms();
    for (usz i = 0; i < expr_count; i++) {
        if (ast_arena_bench_should_reset(i, reset_mode)) {
            ast_arena_destroy(arena);
            ast_arena_init(arena);
            result.ast_resets++;
        }
        Expr* expr = (Expr*)ast_arena_alloc(arena, Expr.sizeof);
        if (expr != null) result.expr_ok++;
    }
    long expr_done_ms = boundary_bench_now_ms();
    for (usz i = 0; i < pattern_count; i++) {
        if (ast_arena_bench_should_reset(AST_ARENA_BENCH_EXPR_ALLOCS + i, reset_mode)) {
            ast_arena_destroy(arena);
            ast_arena_init(arena);
            result.ast_resets++;
        }
        Pattern* pattern = (Pattern*)ast_arena_alloc(arena, Pattern.sizeof);
        if (pattern != null) result.pattern_ok++;
    }
    long end_ms = boundary_bench_now_ms();

    result.expr_ms = expr_done_ms > start_ms ? expr_done_ms - start_ms : 0;
    result.pattern_ms = end_ms > expr_done_ms ? end_ms - expr_done_ms : 0;
    result.ast_validation_failures = ast_arena_validate(arena) ? 0 : 1;
    return result;
}

fn AstArenaBenchParserSuiteResult ast_arena_bench_run_parser_suite(
    Interp* interp,
    bool collect_assertions,
    bool collect_mutation,
    bool reset_mode
) {
    AstArenaBenchParserSuiteResult result = {};

    long start_ms = boundary_bench_now_ms();
    for (usz i = 0; i < AST_ARENA_BENCH_EXPR_ALLOCS; i++) {
        if (ast_arena_bench_should_reset(i, reset_mode)) {
            ast_arena_destroy(&interp.ast_arena);
            ast_arena_init(&interp.ast_arena);
            result.ast_resets++;
        }

        char[] source = ast_arena_bench_parser_source(i);
        Expr* parsed = (Expr*)parse(source, interp);
        if (parsed == null) {
            result.parse_failed++;
            continue;
        }

        bool parse_ok_status = true;
        if (collect_assertions) {
            if (!ast_arena_contains(&interp.ast_arena, parsed)) {
                result.parse_owned_failed++;
                parse_ok_status = false;
            }
        }

        if (collect_mutation) {
            if (ast_arena_bench_mutate_expr(parsed) == null) {
                result.parse_mutation_failed++;
                parse_ok_status = false;
            }
        }

        result.exprs++;
        if (parse_ok_status) {
            result.parse_ok++;
        }
    }
    long end_ms = boundary_bench_now_ms();
    result.parse_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    result.ast_blocks = interp.ast_arena.count;
    result.ast_validation_failures = ast_arena_validate(&interp.ast_arena) ? 0 : 1;
    return result;
}

fn AstArenaBenchCompileSuiteResult ast_arena_bench_run_compile_suite(
    Interp* interp,
    bool collect_assertions,
    bool collect_mutation,
    bool reset_mode
) {
    AstArenaBenchCompileSuiteResult result = {};

    long start_ms = boundary_bench_now_ms();
    for (usz i = 0; i < AST_ARENA_BENCH_PATTERN_ALLOCS; i++) {
        if (ast_arena_bench_should_reset(i, reset_mode)) {
            ast_arena_destroy(&interp.ast_arena);
            ast_arena_init(&interp.ast_arena);
            result.ast_resets++;
        }

        char[] source = ast_arena_bench_compile_source(i);
        AstArenaBenchCompileResult compile_result = ast_arena_bench_compile_once(interp, source, collect_assertions, collect_mutation);

        if (compile_result.parse_ok && compile_result.ast_owned && compile_result.mutated && compile_result.compile_ok && compile_result.output_len > 0) {
            result.compile_ok++;
            result.output_bytes += compile_result.output_len;
        } else {
            result.compile_failed++;
            if (!compile_result.parse_ok) {
                // No parser node at all; not an ownership or mutation failure signal.
            } else {
                if (!compile_result.ast_owned) result.compile_owned_failed++;
                if (!compile_result.mutated) result.compile_mutation_failed++;
                if (compile_result.output_len == 0) result.compile_empty++;
            }
        }
    }
    long end_ms = boundary_bench_now_ms();
    result.compile_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    result.ast_blocks = interp.ast_arena.count;
    result.ast_validation_failures = ast_arena_validate(&interp.ast_arena) ? 0 : 1;
    return result;
}

fn AstArenaBenchMacroSuiteResult ast_arena_bench_run_macro_suite(
    Interp* interp,
    bool collect_assertions,
    bool collect_mutation,
    bool reset_mode
) {
    AstArenaBenchMacroSuiteResult result = {};

    long start_ms = boundary_bench_now_ms();
    for (usz i = 0; i < AST_ARENA_BENCH_EXPR_ALLOCS; i++) {
        if (ast_arena_bench_should_reset(i, reset_mode)) {
            ast_arena_destroy(&interp.ast_arena);
            ast_arena_init(&interp.ast_arena);
            result.ast_resets++;
        }

        char[] source = ast_arena_bench_macro_source(i);
        SymbolId macro_sym = ast_arena_bench_macro_sym(interp, i);

        Expr* parsed = (Expr*)parse(source, interp);
        if (parsed == null) {
            result.macro_failed++;
            continue;
        }

        bool macro_ok_status = true;
        if (collect_assertions) {
            if (!ast_arena_contains(&interp.ast_arena, parsed)) {
                result.macro_owned_failed++;
                macro_ok_status = false;
            }
        }

        if (collect_mutation) {
            if (ast_arena_bench_mutate_expr(parsed) == null) {
                result.macro_mutation_failed++;
                macro_ok_status = false;
            }
        }

        Expr* expanded = expand_macros_in_expr(parsed, interp);
        bool expanded_ok = ast_arena_bench_macro_expanded(expanded, macro_sym);
        if (!expanded_ok) {
            result.macro_not_expanded++;
            macro_ok_status = false;
        }

        if (macro_ok_status) {
            result.macro_ok++;
        } else {
            result.macro_failed++;
        }
    }
    long end_ms = boundary_bench_now_ms();
    result.macro_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    result.ast_blocks = interp.ast_arena.count;
    result.ast_validation_failures = ast_arena_validate(&interp.ast_arena) ? 0 : 1;
    return result;
}

fn AstArenaBenchCompileResult ast_arena_bench_compile_once(
    Interp* interp,
    char[] source,
    bool collect_assertions,
    bool collect_mutation
) {
    AstArenaBenchCompileResult result = {
        .parse_ok = false,
        .compile_ok = false,
        .ast_owned = true,
        .mutated = true,
        .output_len = 0,
        .parsed = null,
    };

    Expr* parsed = parse(source, interp);
    result.parsed = parsed;
    if (parsed == null) {
        return result;
    }
    result.parse_ok = true;

    if (collect_assertions && !ast_arena_contains(&interp.ast_arena, parsed)) {
        result.ast_owned = false;
        return result;
    }

    if (collect_mutation) {
        result.parsed = ast_arena_bench_mutate_expr(parsed);
        result.mutated = result.parsed != null;
        if (!result.mutated) return result;
    }

    Compiler compiler;
    compiler.init(interp);
    char[] output = compiler.compile_program(source);
    result.output_len = output.len;
    compiler.free();
    result.compile_ok = output.len > 0;
    return result;
}

fn long boundary_bench_now_ms() @inline {
    long now_ms = scheduler_monotonic_ms();
    if (now_ms > 0) return now_ms;
    return 0;
}

fn void run_memory_lifetime_ast_arena_benchmark(Interp* interp) {
    if (!ast_arena_bench_enabled()) return;

    if (interp == null) return;

    bool assertions_enabled = ast_arena_bench_assertions_enabled();
    bool mutation_enabled = ast_arena_bench_mutation_enabled();
    bool reset_enabled = ast_arena_bench_reset_enabled();

    Interp bench_interp = {};
    bench_interp.init();
    defer bench_interp.destroy();

    bench_interp.current_scope = bench_interp.root_scope;
    bench_interp.releasing_scope = null;

    AstArena arena = {};
    ast_arena_init(&arena);

    AstArenaBenchAllocResult alloc_result = {
        .expr_count = AST_ARENA_BENCH_EXPR_ALLOCS,
        .pattern_count = AST_ARENA_BENCH_PATTERN_ALLOCS,
    };
    AstArenaBenchParserSuiteResult parse_result = {};
    AstArenaBenchParserSuiteResult parse_result_reset = {};
    AstArenaBenchCompileSuiteResult compile_result = {};
    AstArenaBenchCompileSuiteResult compile_result_reset = {};
    AstArenaBenchMacroSuiteResult macro_result = {};
    AstArenaBenchMacroSuiteResult macro_result_reset = {};

    // Parser smoke pass: repeatedly parse common language forms through parse().
    ast_arena_destroy(&bench_interp.ast_arena);
    ast_arena_init(&bench_interp.ast_arena);
    parse_result = ast_arena_bench_run_parser_suite(&bench_interp, assertions_enabled, mutation_enabled, false);
    if (reset_enabled) {
        ast_arena_destroy(&bench_interp.ast_arena);
        ast_arena_init(&bench_interp.ast_arena);
        parse_result_reset = ast_arena_bench_run_parser_suite(&bench_interp, assertions_enabled, mutation_enabled, true);
    }
    ast_arena_destroy(&bench_interp.ast_arena);
    ast_arena_init(&bench_interp.ast_arena);

    // Compiler smoke pass: parse + expand + compile through compiler pipeline.
    ast_arena_destroy(&bench_interp.ast_arena);
    ast_arena_init(&bench_interp.ast_arena);
    compile_result = ast_arena_bench_run_compile_suite(&bench_interp, assertions_enabled, mutation_enabled, false);
    if (reset_enabled) {
        ast_arena_destroy(&bench_interp.ast_arena);
        ast_arena_init(&bench_interp.ast_arena);
        compile_result_reset = ast_arena_bench_run_compile_suite(&bench_interp, assertions_enabled, mutation_enabled, true);
    }
    ast_arena_destroy(&bench_interp.ast_arena);
    ast_arena_init(&bench_interp.ast_arena);

    // Macro smoke pass: parse + expand common macro definitions/calls.
    ast_arena_destroy(&bench_interp.ast_arena);
    ast_arena_init(&bench_interp.ast_arena);
    macro_result = ast_arena_bench_run_macro_suite(&bench_interp, assertions_enabled, mutation_enabled, false);
    if (reset_enabled) {
        ast_arena_destroy(&bench_interp.ast_arena);
        ast_arena_init(&bench_interp.ast_arena);
        macro_result_reset = ast_arena_bench_run_macro_suite(&bench_interp, assertions_enabled, mutation_enabled, true);
    }
    ast_arena_destroy(&bench_interp.ast_arena);
    ast_arena_init(&bench_interp.ast_arena);

    // AST allocator throughput pass: direct AST node allocation in both lanes.
    alloc_result = ast_arena_bench_run_alloc(
        &arena,
        AST_ARENA_BENCH_EXPR_ALLOCS,
        AST_ARENA_BENCH_PATTERN_ALLOCS,
        false
    );
    usz alloc_result_blocks = arena.count;
    usz alloc_result_capacity = arena.capacity;
    if (reset_enabled) {
        AstArenaBenchAllocResult alloc_result_reset = ast_arena_bench_run_alloc(
            &arena,
            AST_ARENA_BENCH_EXPR_ALLOCS,
            AST_ARENA_BENCH_PATTERN_ALLOCS,
            true
        );
        ast_arena_bench_summary_alloc_reset(
            alloc_result_reset.expr_count,
            alloc_result_reset.expr_ok,
            alloc_result_reset.expr_ms,
            alloc_result_reset.pattern_count,
            alloc_result_reset.pattern_ok,
            alloc_result_reset.pattern_ms,
            alloc_result_reset.ast_resets,
            arena.count,
            arena.capacity,
            alloc_result_reset.ast_validation_failures
        );
        ast_arena_destroy(&arena);
        ast_arena_init(&arena);
    }

    ast_arena_bench_summary_alloc(
        alloc_result.expr_count,
        alloc_result.expr_ok,
        alloc_result.expr_ms,
        alloc_result.pattern_count,
        alloc_result.pattern_ok,
        alloc_result.pattern_ms,
        alloc_result.ast_resets,
        alloc_result_blocks,
        alloc_result_capacity,
        alloc_result.ast_validation_failures
    );
    ast_arena_bench_summary_parser(
        AST_ARENA_BENCH_EXPR_ALLOCS,
        parse_result.parse_ms,
        parse_result.parse_ok,
        parse_result.parse_failed,
        parse_result.parse_owned_failed,
        parse_result.parse_mutation_failed,
        parse_result.exprs,
        parse_result.ast_blocks,
        parse_result.ast_resets,
        parse_result.ast_validation_failures
    );
    if (reset_enabled) {
        ast_arena_bench_summary_parser_reset(
            AST_ARENA_BENCH_EXPR_ALLOCS,
            parse_result_reset.parse_ms,
            parse_result_reset.parse_ok,
            parse_result_reset.parse_failed,
            parse_result_reset.parse_owned_failed,
            parse_result_reset.parse_mutation_failed,
            parse_result_reset.exprs,
            parse_result_reset.ast_blocks,
            parse_result_reset.ast_resets,
            parse_result_reset.ast_validation_failures
        );
    }
    ast_arena_bench_summary_compile(
        AST_ARENA_BENCH_PATTERN_ALLOCS,
        compile_result.compile_ms,
        compile_result.compile_ok,
        compile_result.compile_failed,
        compile_result.compile_empty,
        compile_result.compile_owned_failed,
        compile_result.output_bytes,
        compile_result.ast_blocks,
        compile_result.ast_resets,
        compile_result.ast_validation_failures
    );
    if (reset_enabled) {
        ast_arena_bench_summary_compile_reset(
            AST_ARENA_BENCH_PATTERN_ALLOCS,
            compile_result_reset.compile_ms,
            compile_result_reset.compile_ok,
            compile_result_reset.compile_failed,
            compile_result_reset.compile_empty,
            compile_result_reset.compile_owned_failed,
            compile_result_reset.output_bytes,
            compile_result_reset.ast_blocks,
            compile_result_reset.ast_resets,
            compile_result_reset.ast_validation_failures
        );
    }
    ast_arena_bench_summary_macro(
        AST_ARENA_BENCH_EXPR_ALLOCS,
        macro_result.macro_ms,
        macro_result.macro_ok,
        macro_result.macro_failed,
        macro_result.macro_not_expanded,
        macro_result.macro_owned_failed,
        macro_result.macro_mutation_failed,
        macro_result.ast_resets,
        macro_result.ast_validation_failures
    );
    if (reset_enabled) {
        ast_arena_bench_summary_macro_reset(
            AST_ARENA_BENCH_EXPR_ALLOCS,
            macro_result_reset.macro_ms,
            macro_result_reset.macro_ok,
            macro_result_reset.macro_failed,
            macro_result_reset.macro_not_expanded,
            macro_result_reset.macro_owned_failed,
            macro_result_reset.macro_mutation_failed,
            macro_result_reset.ast_resets,
            macro_result_reset.ast_validation_failures
        );
    }

    ast_arena_destroy(&arena);
    ast_arena_destroy(&bench_interp.ast_arena);
}

fn void run_memory_lifetime_boundary_benchmarks(Interp* interp) {
    if (!boundary_bench_enabled()) return;

    main::ScopeRegion* root_scope = interp.current_scope;

    usz decision_splice_ok = 0;
    usz decision_disallowed_ok = 0;
    usz decision_reuse_ok = 0;
    usz decision_destination_reuse_ok = 0;
    usz splice_tail_ok = 0;

    long decision_splice_ms = 0;
    long decision_disallowed_ms = 0;
    long decision_reuse_ms = 0;
    long decision_destination_reuse_ms = 0;
    long splice_tail_ms = 0;

    // Decision benchmark: splice-commit path.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_DECISION_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = releasing_scope;
            Value* src = make_cons(interp, make_int(interp, (long)(i + 1)), make_nil(interp));

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                src,
                interp,
                target_scope,
                releasing_scope,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);

            bool ok = (committed.releasing_scope_spliced ||
                       committed.outcome == BOUNDARY_COMMIT_DESTINATION_CONS_BUILT) &&
                      committed.value != null;
            if (ok) decision_splice_ok++;

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }
        }
        long end_ms = boundary_bench_now_ms();
        decision_splice_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    // Decision benchmark: budget-abort hard-disallowed path.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_DECISION_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = releasing_scope;
            Value* src = boundary_commit_make_temp_int_list(interp, 8);

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, 0);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                src,
                interp,
                target_scope,
                releasing_scope,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);

            bool ok = !committed.releasing_scope_spliced &&
                      committed.outcome == BOUNDARY_COMMIT_FALLBACK_DISALLOWED &&
                      committed.fault_code == BOUNDARY_COMMIT_FAULT_PROMOTION_ABORTED &&
                      committed.value != null &&
                      committed.value.tag == ERROR;
            if (ok) decision_disallowed_ok++;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);
        }
        long end_ms = boundary_bench_now_ms();
        decision_disallowed_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    // Decision benchmark: target-chain reuse path.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_DECISION_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = target_scope;
            Value* safe = make_cons(interp, make_int(interp, (long)(i + 17)), make_nil(interp));
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                safe,
                interp,
                target_scope,
                releasing_scope
            );

            bool ok = !committed.releasing_scope_spliced &&
                      committed.reused_in_target_chain &&
                      committed.value == safe;
            if (ok) decision_reuse_ok++;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);
        }
        long end_ms = boundary_bench_now_ms();
        decision_reuse_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    // Decision benchmark: destination partial build with target-escape routed args.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_DECISION_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = target_scope;
            Value* escaped_first = promote_to_escape(make_int(interp, (long)(i + 501)), interp);
            Value* escaped_second = promote_to_escape(make_int(interp, (long)(i + 901)), interp);

            interp.current_scope = releasing_scope;
            Value* src = interp.alloc_value();
            src.tag = PARTIAL_PRIM;
            src.partial_val.func = null;
            src.partial_val.remaining = 1;
            src.partial_val.first_arg = escaped_first;
            src.partial_val.second_arg = escaped_second;

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            Value* destination_partial = boundary_build_destination_partial_escape(
                src,
                interp,
                target_scope,
                releasing_scope,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);

            bool ok = destination_partial != null &&
                      destination_partial.tag == PARTIAL_PRIM &&
                      destination_partial.scope_gen == target_scope.escape_generation &&
                      destination_partial.partial_val.first_arg == escaped_first &&
                      destination_partial.partial_val.second_arg == escaped_second;
            if (ok) decision_destination_reuse_ok++;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);
        }
        long end_ms = boundary_bench_now_ms();
        decision_destination_reuse_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    // Splice benchmark: focus on escape-lane concat throughput/tail consistency.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_SPLICE_ITERS; i++) {
            main::ScopeRegion* parent = main::scope_create(root_scope);
            main::ScopeRegion* child = parent == null ? null : main::scope_create(parent);
            if (parent == null || child == null) {
                if (child != null) main::scope_release(child);
                if (parent != null) main::scope_release(parent);
                continue;
            }

            interp.current_scope = child;
            Value* first_escape = null;
            for (usz j = 0; j < BOUNDARY_BENCH_SPLICE_ESCAPES_PER_ITER; j++) {
                Value* v = interp.alloc_value_escape();
                if (v != null) {
                    v.tag = INT;
                    v.int_val = (long)(j + 1);
                    if (j == 0) first_escape = v;
                }
            }
            interp.current_scope = root_scope;

            BoundaryScopeTransferResult ready = boundary_try_scope_splice_escapes_result(parent, child);
            bool ok = false;
            if (ready.allowed) {
                ok = first_escape != null &&
                     main::is_in_scope((void*)first_escape, parent) &&
                     main::scope_escape_chunk_tail_consistent(parent) &&
                     main::scope_escape_dtor_tail_consistent(parent);
            }
            if (ok) splice_tail_ok++;
            main::scope_release(parent);
        }
        long end_ms = boundary_bench_now_ms();
        splice_tail_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    interp.current_scope = root_scope;
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=boundary_decision_cost iters=%d splice_ms=%d disallowed_ms=%d reuse_ms=%d splice_ok=%d disallowed_ok=%d reuse_ok=%d",
        (long)BOUNDARY_BENCH_DECISION_ITERS,
        decision_splice_ms,
        decision_disallowed_ms,
        decision_reuse_ms,
        (long)decision_splice_ok,
        (long)decision_disallowed_ok,
        (long)decision_reuse_ok
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=boundary_destination_routed_escape iters=%d partial_ms=%d partial_ok=%d",
        (long)BOUNDARY_BENCH_DECISION_ITERS,
        decision_destination_reuse_ms,
        (long)decision_destination_reuse_ok
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=scope_splice_tail iters=%d escapes_per_iter=%d splice_ms=%d splice_ok=%d",
        (long)BOUNDARY_BENCH_SPLICE_ITERS,
        (long)BOUNDARY_BENCH_SPLICE_ESCAPES_PER_ITER,
        splice_tail_ms,
        (long)splice_tail_ok
    );
}
