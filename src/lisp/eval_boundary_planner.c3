module lisp;

import main;

fn BoundaryPlanDecision boundary_plan_commit_escape(
    Value* result,
    BoundaryReturnClassification classification,
    bool allow_scope_splice,
    main::ScopeRegion* releasing_scope
) @inline {
    BoundaryPlanDecision plan = {
        .route = BOUNDARY_PLAN_ROUTE_NULL_RESULT,
        .reason = BOUNDARY_PLAN_REASON_NULL_RESULT,
        .provenance = classification.provenance,
        .value_tag = VOID,
        .stable_publish_candidate = false,
        .stable_materialization_candidate = false,
        .transplant_candidate = false,
        .compatibility_candidate = false,
    };
    if (result == null) return plan;
    plan.value_tag = result.tag;

    switch (classification.provenance) {
        case BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN:
            plan.route = BOUNDARY_PLAN_ROUTE_STABLE_PUBLISH_REUSE;
            plan.reason = BOUNDARY_PLAN_REASON_REUSABLE_IN_TARGET_CHAIN;
            plan.stable_publish_candidate = true;
            return plan;

        case BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP:
            if (boundary_value_tag_is_stable_materialization_candidate(result.tag)) {
                plan.route = BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION;
                plan.reason = BOUNDARY_PLAN_REASON_RELEASING_TEMP_STABLE_MATERIALIZATION;
                plan.stable_materialization_candidate = true;
                plan.transplant_candidate =
                    allow_scope_splice && releasing_scope != null && releasing_scope.refcount == 1;
                plan.compatibility_candidate = result.tag == CONS;
            } else {
                plan.route = BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION;
                plan.reason = BOUNDARY_PLAN_REASON_RELEASING_TEMP_COMPATIBILITY;
                plan.compatibility_candidate = true;
            }
            return plan;

        case BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE:
            if (allow_scope_splice && releasing_scope != null && releasing_scope.refcount == 1) {
                plan.route = BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT;
                plan.reason = BOUNDARY_PLAN_REASON_RELEASING_ESCAPE_TRANSPLANT_CANDIDATE;
                plan.transplant_candidate = true;
                plan.stable_materialization_candidate =
                    boundary_value_tag_is_stable_materialization_candidate(result.tag);
                plan.compatibility_candidate =
                    boundary_value_tag_has_compatibility_destination(result.tag);
            } else {
                if (boundary_value_tag_is_stable_materialization_candidate(result.tag)) {
                    plan.route = BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION;
                    plan.reason = BOUNDARY_PLAN_REASON_RELEASING_ESCAPE_DESTINATION;
                    plan.stable_materialization_candidate = true;
                } else {
                    plan.route = BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION;
                    plan.reason = BOUNDARY_PLAN_REASON_RELEASING_ESCAPE_DESTINATION;
                    plan.compatibility_candidate = true;
                }
            }
            return plan;

        case BOUNDARY_RETURN_MIXED_UNCERTAIN:
            plan.route = BOUNDARY_PLAN_ROUTE_MIXED_DESTINATION;
            plan.reason = BOUNDARY_PLAN_REASON_MIXED_UNCERTAIN;
            plan.compatibility_candidate = true;
            return plan;
    }
}

fn bool boundary_plan_route_is_releasing_scope(BoundaryPlanRoute route) @inline {
    return route == BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION ||
        route == BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT ||
        route == BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION;
}

fn void boundary_commit_apply_initial_plan(
    BoundaryCommitEscapeResult* committed,
    BoundaryPlanDecision plan
) @inline {
    if (committed == null) return;
    committed.planned_route = plan.route;
    committed.planned_reason = plan.reason;
    committed.planned_value_tag = plan.value_tag;
    committed.selected_route = plan.route;
    committed.selected_reason = plan.reason;
    committed.selected_value_tag = plan.value_tag;
}

fn void boundary_commit_set_selected_plan(
    BoundaryCommitEscapeResult* committed,
    BoundaryPlanRoute route,
    BoundaryPlanReason reason
) @inline {
    if (committed == null) return;
    committed.selected_route = route;
    committed.selected_reason = reason;
}
