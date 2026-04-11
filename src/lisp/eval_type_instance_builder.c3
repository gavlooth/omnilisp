module lisp;

import std::io;
import main;

/**
 * Create an Instance value with a dedicated owner scope.
 *
 * owner_scope stores field payload lifetimes.
 * wrapper_scope stores the Value wrapper and destructor.
 */
<*
@require interp != null : "eval_copy_value_into_scope_site: null interp"
@require target_scope != null : "eval_copy_value_into_scope_site: null target_scope"
@require releasing_scope != null : "eval_copy_value_into_scope_site: null releasing_scope"
*>
fn BoundaryCopyResult eval_copy_value_into_scope_site(
    Value* value,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    uint site
) {
    if (value == null || target_scope == null) return boundary_copy_ok(value);
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;
    PromotionContext copy_ctx = {};
    boundary_promotion_context_begin(interp, &copy_ctx);
    defer boundary_promotion_context_end(interp, &copy_ctx);
    return boundary_copy_to_parent_site_ctx_checked(value, interp, site, &copy_ctx);
}

fn Value* make_instance_in_scope(Interp* interp, main::ScopeRegion* wrapper_scope,
    TypeId type_id, Value** fields, usz field_count) {
    if (field_count != 0 && field_count > usz.max / Value.sizeof) return null;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);

    main::ScopeRegion* owner_scope = main::scope_create(interp.root_scope);
    if (owner_scope == null) return null;

    Instance* inst = (Instance*)owner_scope.alloc(Instance.sizeof);
    if (inst == null) {
        main::scope_release(owner_scope);
        return null;
    }
    inst.owner_scope = owner_scope;
    inst.type_id = type_id;
    inst.field_count = field_count;
    inst.fields = field_count > 0 ? (Value*)owner_scope.alloc(Value.sizeof * field_count) : null;
    inst.type_arg_count = 0;
    inst.type_args = null;
    if (field_count > 0 && inst.fields == null) {
        main::scope_release(owner_scope);
        return null;
    }

    for (usz i = 0; i < field_count; i++) {
        inst.fields[i] = {};
        inst.fields[i].tag = NIL;
    }

    // Copy field payloads into owner_scope so Instance never points into caller scope.
    for (usz i = 0; i < field_count; i++) {
        if (fields != null && fields[i] != null) {
            BoundaryCopyResult copied = eval_copy_value_into_scope_site(
                fields[i],
                interp,
                owner_scope,
                session.current_scope,
                COPY_SITE_GENERIC
            );
            if (!boundary_copy_result_ok(copied)) {
                main::scope_release(owner_scope);
                char[192] msg_buf;
                if (try msg = io::bprintf(
                    &msg_buf,
                    "type constructor: boundary field copy failed at index %d (%s)",
                    (int)i,
                    (ZString)boundary_copy_fault_name(copied.fault_code)
                )) {
                    return raise_error(interp, msg);
                }
                return raise_error(interp, "type constructor: boundary field copy failed");
            }
            if (copied.value != null) inst.fields[i] = *copied.value;
        }
    }
    Value* v = boundary_alloc_value_in_scope(interp, wrapper_scope, true);
    if (v == null) {
        main::scope_release(owner_scope);
        return null;
    }
    v.tag = INSTANCE;
    v.instance_val = inst;
    return v;
}

fn Value* make_instance(Interp* interp, TypeId type_id, Value** fields, usz field_count) {
    return make_instance_in_scope(interp, interp.current_scope, type_id, fields, field_count);
}

// Root-scope wrapper variant for global constants (nullary union constructors, etc.)
fn Value* make_instance_root(Interp* interp, TypeId type_id, Value** fields, usz field_count) {
    return make_instance_in_scope(interp, interp.root_scope, type_id, fields, field_count);
}
