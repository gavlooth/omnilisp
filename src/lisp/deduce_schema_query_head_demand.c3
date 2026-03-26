module lisp;

import std::core::mem;

fn Value* deduce_query_build_head_demand_from_filter_expr(
    Relation* rel,
    Expr* body,
    SymbolId row_param,
    Env* capture_env,
    DeduceRuleHeadDemand* out_head_demand,
    bool* has_any_bound_out,
    bool* supported_out,
    Interp* interp
) {
    if (out_head_demand == null || has_any_bound_out == null || supported_out == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid head demand state");
    }

    *out_head_demand = {};
    *has_any_bound_out = false;
    *supported_out = false;

    if (rel == null || body == null || interp == null || row_param == (SymbolId)0) return null;
    if (rel.col_count == 0) return null;

    bool[] term_bound = mem::new_array(bool, rel.col_count);
    if (term_bound.ptr == null) {
        return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
    }
    Value*[] term_values = mem::new_array(Value*, rel.col_count);
    if (term_values.ptr == null) {
        mem::free(term_bound.ptr);
        return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
    }
    for (usz i = 0; i < rel.col_count; i++) {
        term_bound[i] = false;
        term_values[i] = null;
    }

    bool has_any_bound = false;
    if (!deduce_query_filter_expr_try_collect_equalities(
            body,
            row_param,
            capture_env,
            rel,
            term_bound,
            term_values,
            interp,
            &has_any_bound
        ) || !has_any_bound) {
        mem::free(term_values.ptr);
        mem::free(term_bound.ptr);
        return null;
    }

    *has_any_bound_out = has_any_bound;
    *supported_out = true;
    *out_head_demand = {
        .predicate = rel.name,
        .term_bound = term_bound.ptr,
        .term_values = term_values.ptr,
        .term_count = rel.col_count,
    };
    return null;
}

fn Value* deduce_query_collect_disjunctive_head_demands_from_expr(
    Relation* rel,
    Expr* expr,
    SymbolId row_param,
    Env* capture_env,
    DeduceQueryHeadDemandCases* out_demands,
    bool* supported_out,
    Interp* interp
) {
    if (supported_out != null) *supported_out = false;
    if (rel == null || expr == null || out_demands == null || supported_out == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid disjunctive head demand state");
    }

    switch (expr.tag) {
        case E_BLOCK: {
            if (expr.block == null || expr.block.expr_count == 0 || expr.block.exprs == null) return null;
            Expr* tail = expr.block.exprs[expr.block.expr_count - 1];
            if (tail == null) return null;
            return deduce_query_collect_disjunctive_head_demands_from_expr(
                rel,
                tail,
                row_param,
                capture_env,
                out_demands,
                supported_out,
                interp
            );
        }
        case E_IF: {
            if (expr.if_expr.test == null || expr.if_expr.then_branch == null || expr.if_expr.else_branch == null) {
                return null;
            }
            Value* test_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.if_expr.test,
                    row_param,
                    capture_env,
                    interp,
                    &test_value
                )) return null;
            Expr* selected_branch = is_falsy(test_value, interp)
                ? expr.if_expr.else_branch
                : expr.if_expr.then_branch;
            return deduce_query_collect_disjunctive_head_demands_from_expr(
                rel,
                selected_branch,
                row_param,
                capture_env,
                out_demands,
                supported_out,
                interp
            );
        }
        case E_LET: {
            if (expr.let_expr.body == null || expr.let_expr.init == null) return null;
            if (expr.let_expr.is_recursive || expr.let_expr.pattern != null) return null;

            Value* init_value = null;
            if (!deduce_query_expr_try_extract_literal_value(
                    expr.let_expr.init,
                    row_param,
                    capture_env,
                    interp,
                    &init_value
                )) return null;
            if (init_value == null) return null;

            Env* parent_env = capture_env != null ? capture_env : interp.global_env;
            if (parent_env == null) return null;
            Env* scoped_env = make_env(interp, parent_env);
            if (scoped_env == null) {
                return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
            }
            scoped_env.define(expr.let_expr.name, init_value);

            return deduce_query_collect_disjunctive_head_demands_from_expr(
                rel,
                expr.let_expr.body,
                row_param,
                scoped_env,
                out_demands,
                supported_out,
                interp
            );
        }
        case E_OR: {
            if (expr.or_expr.left == null || expr.or_expr.right == null) return null;

            Value* left_value = null;
            if (deduce_query_expr_try_extract_literal_value(
                    expr.or_expr.left,
                    row_param,
                    capture_env,
                    interp,
                    &left_value
                )) {
                if (!is_falsy(left_value, interp)) return null;
                return deduce_query_collect_disjunctive_head_demands_from_expr(
                    rel,
                    expr.or_expr.right,
                    row_param,
                    capture_env,
                    out_demands,
                    supported_out,
                    interp
                );
            }

            Value* right_value = null;
            if (deduce_query_expr_try_extract_literal_value(
                    expr.or_expr.right,
                    row_param,
                    capture_env,
                    interp,
                    &right_value
                )) {
                if (!is_falsy(right_value, interp)) return null;
                return deduce_query_collect_disjunctive_head_demands_from_expr(
                    rel,
                    expr.or_expr.left,
                    row_param,
                    capture_env,
                    out_demands,
                    supported_out,
                    interp
                );
            }

            DeduceQueryHeadDemandCases left_demands = {};
            defer deduce_query_head_demand_cases_release(&left_demands);
            DeduceQueryHeadDemandCases right_demands = {};
            defer deduce_query_head_demand_cases_release(&right_demands);
            bool left_supported = false;
            bool right_supported = false;
            Value* left_err = deduce_query_collect_disjunctive_head_demands_from_expr(
                rel,
                expr.or_expr.left,
                row_param,
                capture_env,
                &left_demands,
                &left_supported,
                interp
            );
            if (left_err != null) return left_err;
            Value* right_err = deduce_query_collect_disjunctive_head_demands_from_expr(
                rel,
                expr.or_expr.right,
                row_param,
                capture_env,
                &right_demands,
                &right_supported,
                interp
            );
            if (right_err != null) return right_err;
            if (!left_supported || !right_supported || left_demands.count == 0 || right_demands.count == 0) {
                return null;
            }

            for (usz i = 0; i < left_demands.count; i++) {
                Value* append_err = deduce_query_head_demand_cases_append_take(
                    out_demands,
                    &left_demands.entries[i],
                    interp
                );
                if (append_err != null) return append_err;
            }
            for (usz i = 0; i < right_demands.count; i++) {
                Value* append_err = deduce_query_head_demand_cases_append_take(
                    out_demands,
                    &right_demands.entries[i],
                    interp
                );
                if (append_err != null) return append_err;
            }

            *supported_out = true;
            return null;
        }
        default: {
            DeduceRuleHeadDemand demand = {};
            bool has_any_bound = false;
            bool supported = false;
            Value* demand_err = deduce_query_build_head_demand_from_filter_expr(
                rel,
                expr,
                row_param,
                capture_env,
                &demand,
                &has_any_bound,
                &supported,
                interp
            );
            if (demand_err != null) return demand_err;
            if (!supported || !has_any_bound) return null;
            Value* append_err = deduce_query_head_demand_cases_append_take(out_demands, &demand, interp);
            if (append_err != null) {
                deduce_rule_head_demand_release(&demand);
                return append_err;
            }
            *supported_out = true;
            return null;
        }
    }
}

fn Value* deduce_query_try_build_head_demand_disjunction_from_filter(
    Relation* rel,
    Value* filter_fn,
    DeduceQueryHeadDemandCases* out_demands,
    bool* supported_out,
    Interp* interp
) {
    if (out_demands == null || supported_out == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid disjunctive head demand state");
    }

    *out_demands = {};
    *supported_out = false;

    if (rel == null || filter_fn == null || filter_fn.tag != CLOSURE || filter_fn.closure_val == null) return null;
    if (filter_fn.closure_val.param_count != 1 || filter_fn.closure_val.has_rest || filter_fn.closure_val.params == null) {
        return null;
    }
    if (rel.col_count == 0) return null;

    Expr* body = filter_fn.closure_val.body;
    if (body == null) return null;
    if (body.tag == E_BLOCK) {
        if (body.block == null || body.block.expr_count == 0 || body.block.exprs == null) return null;
        body = body.block.exprs[body.block.expr_count - 1];
        if (body == null) return null;
    }

    Value* demand_err = deduce_query_collect_disjunctive_head_demands_from_expr(
        rel,
        body,
        filter_fn.closure_val.params[0],
        filter_fn.closure_val.env,
        out_demands,
        supported_out,
        interp
    );
    if (demand_err != null) {
        deduce_query_head_demand_cases_release(out_demands);
        return demand_err;
    }
    if (!*supported_out || out_demands.count < 2) {
        deduce_query_head_demand_cases_release(out_demands);
        *supported_out = false;
    }
    return null;
}

fn Value* deduce_query_build_union_head_demand_positions(
    Relation* rel,
    DeduceRuleHeadDemand* demands,
    usz demand_count,
    DeduceRuleHeadDemand* out_union,
    bool* has_any_bound_out,
    Interp* interp
) {
    if (out_union == null || has_any_bound_out == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid union head demand state");
    }

    *out_union = {};
    *has_any_bound_out = false;

    if (rel == null || rel.col_count == 0 || demands == null || demand_count == 0) return null;

    bool[] term_bound = mem::new_array(bool, rel.col_count);
    if (term_bound.ptr == null) {
        return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
    }
    Value*[] term_values = mem::new_array(Value*, rel.col_count);
    if (term_values.ptr == null) {
        mem::free(term_bound.ptr);
        return deduce_raise(interp, "deduce/query-out-of-memory", "deduce-query: out of memory");
    }

    bool has_any_bound = false;
    for (usz i = 0; i < rel.col_count; i++) {
        term_bound[i] = false;
        term_values[i] = null;
    }
    for (usz i = 0; i < demand_count; i++) {
        DeduceRuleHeadDemand* demand = &demands[i];
        if (demand.predicate != rel.name || demand.term_bound == null) continue;
        usz limit = demand.term_count < rel.col_count ? demand.term_count : rel.col_count;
        for (usz j = 0; j < limit; j++) {
            if (!demand.term_bound[j]) continue;
            term_bound[j] = true;
            has_any_bound = true;
        }
    }

    if (!has_any_bound) {
        mem::free(term_values.ptr);
        mem::free(term_bound.ptr);
        return null;
    }

    *out_union = {
        .predicate = rel.name,
        .term_bound = term_bound.ptr,
        .term_values = term_values.ptr,
        .term_count = rel.col_count,
    };
    *has_any_bound_out = true;
    return null;
}

fn bool deduce_query_head_demand_try_get_single_bound_position(
    DeduceRuleHeadDemand* demand,
    usz* out_position,
    Value** out_value
) {
    if (demand == null || out_position == null || out_value == null) return false;
    if (demand.term_bound == null || demand.term_values == null || demand.term_count == 0) return false;

    bool found = false;
    usz position = 0;
    Value* value = null;
    for (usz i = 0; i < demand.term_count; i++) {
        if (!demand.term_bound[i]) continue;
        if (found) return false;
        found = true;
        position = i;
        value = demand.term_values[i];
    }
    if (!found || value == null) return false;
    *out_position = position;
    *out_value = value;
    return true;
}

fn usz deduce_query_head_demand_bound_position_count(DeduceRuleHeadDemand* demand) {
    if (demand == null || demand.term_bound == null || demand.term_count == 0) return 0;

    usz count = 0;
    for (usz i = 0; i < demand.term_count; i++) {
        if (demand.term_bound[i]) count++;
    }
    return count;
}

fn bool deduce_query_recursive_component_requires_projected_head_demand_relaxation(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* projected_head_demand
) {
    if (db == null || plan == null || projected_head_demand == null) return false;
    if (plan.component_recursive.ptr == null || component_id >= plan.component_recursive.len) return false;
    if (!plan.component_recursive[component_id]) return false;
    if (deduce_query_head_demand_bound_position_count(projected_head_demand) <= 1) return false;

    bool can_use_for_query = deduce_component_can_use_relation_head_demand_for_query(
        db,
        plan,
        component_id,
        projected_head_demand
    );
    if (!can_use_for_query) return true;

    return !deduce_component_can_use_relation_head_demand_at_same_indices(
        db,
        plan,
        component_id,
        projected_head_demand
    );
}

fn bool deduce_query_recursive_component_requires_projected_head_demand_fallback(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* projected_head_demand
) {
    if (db == null || plan == null || projected_head_demand == null) return false;
    if (plan.component_recursive.ptr == null || component_id >= plan.component_recursive.len) return false;
    if (!plan.component_recursive[component_id]) return false;
    if (deduce_query_head_demand_bound_position_count(projected_head_demand) <= 1) return false;

    bool can_use_for_query = deduce_component_can_use_relation_head_demand_for_query(
        db,
        plan,
        component_id,
        projected_head_demand
    );
    if (!can_use_for_query) return false;

    if (deduce_component_can_use_relation_head_demand_at_same_indices(
            db,
            plan,
            component_id,
            projected_head_demand
        )) {
        return false;
    }

    if (deduce_component_head_rule_has_multiple_recursive_body_atoms(
            db,
            plan,
            component_id,
            projected_head_demand.predicate
        )) {
        return true;
    }

    for (usz i = 0; i < projected_head_demand.term_count; i++) {
        if (projected_head_demand.term_bound == null || !projected_head_demand.term_bound[i]) continue;
        if (deduce_component_position_has_same_index_recursive_carrier(
                db,
                plan,
                component_id,
                projected_head_demand.predicate,
                i
            )) {
            return false;
        }
    }
    return true;
}

fn bool deduce_query_relax_projected_head_demand_to_recursive_preserved_position(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleHeadDemand* projected_head_demand
) {
    if (projected_head_demand == null || projected_head_demand.term_bound == null || projected_head_demand.term_count == 0) {
        return false;
    }

    bool same_index_only = false;
    if (deduce_component_can_use_relation_head_demand_for_query(
            db,
            plan,
            component_id,
            projected_head_demand
        ) && !deduce_component_can_use_relation_head_demand_at_same_indices(
            db,
            plan,
            component_id,
            projected_head_demand
        )) {
        same_index_only = true;
    }

    usz kept_position = projected_head_demand.term_count;
    for (usz i = 0; i < projected_head_demand.term_count; i++) {
        if (!projected_head_demand.term_bound[i]) continue;
        if (same_index_only) {
            if (!deduce_component_position_has_same_index_recursive_carrier(
                    db,
                    plan,
                    component_id,
                    projected_head_demand.predicate,
                    i
                )) {
                continue;
            }
        } else if (!deduce_component_position_can_use_relation_head_demand_for_query(
                db,
                plan,
                component_id,
                projected_head_demand.predicate,
                i
            )) {
            continue;
        }
        kept_position = i;
        break;
    }
    if (kept_position >= projected_head_demand.term_count) return false;

    for (usz i = 0; i < projected_head_demand.term_count; i++) {
        if (i == kept_position || !projected_head_demand.term_bound[i]) continue;
        projected_head_demand.term_bound[i] = false;
        if (projected_head_demand.term_values != null) projected_head_demand.term_values[i] = null;
    }
    return true;
}

fn void deduce_query_append_list_to_reversed(Value* list, Value** reversed_out, Interp* interp) {
    if (reversed_out == null || interp == null) return;
    Value* current = list;
    while (current != null && current.tag == CONS) {
        *reversed_out = make_cons(interp, current.cons_val.car, *reversed_out);
        current = current.cons_val.cdr;
    }
}

fn Value* deduce_query_try_build_head_demand_from_filter(
    Relation* rel,
    Value* filter_fn,
    DeduceRuleHeadDemand* out_head_demand,
    bool[]* out_term_bound,
    Value*[]* out_term_values,
    bool* has_any_bound_out,
    bool* supported_out,
    Interp* interp
) {
    if (out_head_demand == null || out_term_bound == null || out_term_values == null ||
        has_any_bound_out == null || supported_out == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid head demand state");
    }

    *out_head_demand = {};
    *out_term_bound = {};
    *out_term_values = {};
    *has_any_bound_out = false;
    *supported_out = false;

    if (rel == null || filter_fn == null || filter_fn.tag != CLOSURE || filter_fn.closure_val == null) return null;
    if (filter_fn.closure_val.param_count != 1 || filter_fn.closure_val.has_rest || filter_fn.closure_val.params == null) {
        return null;
    }
    if (rel.col_count == 0) return null;

    Expr* body = filter_fn.closure_val.body;
    if (body == null) return null;
    if (body.tag == E_BLOCK) {
        if (body.block == null || body.block.expr_count == 0 || body.block.exprs == null) return null;
        body = body.block.exprs[body.block.expr_count - 1];
        if (body == null) return null;
    }

    Value* demand_err = deduce_query_build_head_demand_from_filter_expr(
        rel,
        body,
        filter_fn.closure_val.params[0],
        filter_fn.closure_val.env,
        out_head_demand,
        has_any_bound_out,
        supported_out,
        interp
    );
    if (demand_err != null || !*supported_out || out_head_demand.term_bound == null || out_head_demand.term_values == null) {
        return demand_err;
    }
    *out_term_bound = out_head_demand.term_bound[..out_head_demand.term_count];
    *out_term_values = out_head_demand.term_values[..out_head_demand.term_count];
    return null;
}

fn Value* deduce_query_scan_relation_in_txn(
    Relation* rel,
    Value* filter_fn,
    MdbTxn* txn,
    Interp* interp,
    DeduceGoalDirectedReadSubjectKeyCapture* subject_keys_out = null
) {
    if (rel == null || filter_fn == null || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/query-invalid-state", "deduce-query: invalid in-transaction query state");
    }
    if (subject_keys_out != null) deduce_goal_directed_read_subject_key_capture_release(subject_keys_out);

    DeduceTupleIterator it = {};
    if (!deduce_tuple_iterator_open_in_txn(&it, rel, interp, txn, null, 0, false, {}, {}, false)) {
        return make_nil(interp);
    }
    defer deduce_tuple_iterator_close(&it);

    uint dict_capacity = deduce_relation_row_dict_capacity(rel);
    Value* filtered_rev = make_nil(interp);
    while (deduce_tuple_iterator_next(&it)) {
        Value* row = deduce_relation_materialize_row_dict(rel, it.vals, interp, dict_capacity);
        Value* keep = jit_apply_value(filter_fn, row, interp);
        if (keep != null && !is_falsy(keep, interp)) {
            if (subject_keys_out != null) {
                char[4096] key_buf;
                usz? key_len = encode_tuple(it.vals, key_buf[..], key_buf.len);
                if (try encoded_row_key_len = key_len) {
                    deduce_goal_directed_read_subject_key_capture_record(
                        subject_keys_out,
                        &key_buf[0],
                        encoded_row_key_len
                    );
                } else {
                    subject_keys_out.complete = false;
                }
            }
            filtered_rev = make_cons(interp, row, filtered_rev);
        }
    }
    return deduce_reverse_list(filtered_rev, interp);
}
