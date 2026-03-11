module lisp;

import std::io;
import main;

fn void run_memory_lifetime_boundary_return_classification_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: boundary return classification provides one explicit provenance class.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        main::ScopeRegion* foreign_scope = main::scope_create(saved_scope);
        if (target_scope == null || releasing_scope == null || foreign_scope == null) {
            if (foreign_scope != null) main::scope_release(foreign_scope);
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary return classification matrix");
            (*fail)++;
        } else {
            interp.current_scope = target_scope;
            Value* reusable = make_int(interp, 101);

            interp.current_scope = releasing_scope;
            Value* temp_owned = make_int(interp, 202);
            Value* escape_seed = make_int(interp, 303);
            Value* escape_owned = promote_to_escape(escape_seed, interp);

            interp.current_scope = foreign_scope;
            Value* mixed_uncertain = make_int(interp, 404);

            interp.current_scope = target_scope;
            interp.releasing_scope = releasing_scope;

            BoundaryReturnClassification reusable_class =
                boundary_classify_return_value(reusable, interp, target_scope, releasing_scope);
            BoundaryReturnClassification temp_class =
                boundary_classify_return_value(temp_owned, interp, target_scope, releasing_scope);
            BoundaryReturnClassification escape_class =
                boundary_classify_return_value(escape_owned, interp, target_scope, releasing_scope);
            BoundaryReturnClassification mixed_class =
                boundary_classify_return_value(mixed_uncertain, interp, target_scope, releasing_scope);

            bool reusable_ok =
                reusable_class.provenance == BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN &&
                reusable_class.in_target_scope_chain &&
                !reusable_class.in_releasing_scope;
            bool temp_ok =
                temp_class.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP &&
                !temp_class.in_target_scope_chain &&
                temp_class.in_releasing_scope;
            bool escape_ok =
                escape_class.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE &&
                !escape_class.in_target_scope_chain &&
                escape_class.in_releasing_scope;
            bool mixed_ok =
                mixed_class.provenance == BOUNDARY_RETURN_MIXED_UNCERTAIN &&
                !mixed_class.in_target_scope_chain &&
                !mixed_class.in_releasing_scope;

            interp.current_scope = saved_scope;
            interp.releasing_scope = null;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);
            main::scope_release(foreign_scope);

            bool ok = reusable_ok && temp_ok && escape_ok && mixed_ok;
            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary return classification matrix");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: boundary return classification matrix (reusable=%s temp=%s escape=%s mixed=%s tags=%s/%s/%s/%s)",
                    reusable_ok ? "yes" : "no",
                    temp_ok ? "yes" : "no",
                    escape_ok ? "yes" : "no",
                    mixed_ok ? "yes" : "no",
                    boundary_return_provenance_name(reusable_class.provenance),
                    boundary_return_provenance_name(temp_class.provenance),
                    boundary_return_provenance_name(escape_class.provenance),
                    boundary_return_provenance_name(mixed_class.provenance)
                );
                (*fail)++;
            }
        }
    }
}
