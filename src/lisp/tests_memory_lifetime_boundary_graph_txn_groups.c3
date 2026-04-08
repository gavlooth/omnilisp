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
