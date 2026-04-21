module lisp;

import std::core::mem;
import main;

fn Value* tensor_copy_expr_child_to_parent(Value* child, Interp* interp, BoundaryCopyFault* fault_out, char[] message) {
    if (child != null && tensor_should_force_expr_edge_fail(&g_tensor_force_expr_edge_copy_fail_nth)) {
        if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
            *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        }
        return fault_out != null ? null : raise_error(interp, message);
    }
    Value* copied = copy_to_parent_with_fault(child, interp, fault_out);
    if (child != null && (copied == null || is_error(copied))) {
        if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
            *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        }
        return fault_out != null
            ? null
            : (copied == null ? raise_error(interp, message) : copied);
    }
    return copied;
}

fn bool tensor_copy_expr_edges_to_parent(TensorVal* dst, TensorVal* src, Interp* interp, BoundaryCopyFault* fault_out = null) {
    if (dst == null || src == null) return true;
    switch (src.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return true;
        case TENSOR_PAYLOAD_MAP:
            dst.map_func = tensor_copy_expr_child_to_parent(src.map_func, interp, fault_out, "boundary: failed to copy tensor map function");
            if (src.map_func != null && (dst.map_func == null || is_error(dst.map_func))) return false;
            dst.map_left = tensor_copy_expr_child_to_parent(src.map_left, interp, fault_out, "boundary: failed to copy tensor map left input");
            if (src.map_left != null && (dst.map_left == null || is_error(dst.map_left))) return false;
            dst.map_right = tensor_copy_expr_child_to_parent(src.map_right, interp, fault_out, "boundary: failed to copy tensor map right input");
            if (src.map_right != null && (dst.map_right == null || is_error(dst.map_right))) return false;
            return true;
        case TENSOR_PAYLOAD_CONTRACT:
            dst.contract_left = tensor_copy_expr_child_to_parent(src.contract_left, interp, fault_out, "boundary: failed to copy tensor contract left input");
            if (src.contract_left != null && (dst.contract_left == null || is_error(dst.contract_left))) return false;
            dst.contract_right = tensor_copy_expr_child_to_parent(src.contract_right, interp, fault_out, "boundary: failed to copy tensor contract right input");
            if (src.contract_right != null && (dst.contract_right == null || is_error(dst.contract_right))) return false;
            return true;
        case TENSOR_PAYLOAD_VIEW:
            dst.view_source = tensor_copy_expr_child_to_parent(src.view_source, interp, fault_out, "boundary: failed to copy tensor view source");
            if (src.view_source != null && (dst.view_source == null || is_error(dst.view_source) || dst.view_source.tag != TENSOR || dst.view_source.tensor_val == null)) return false;
            if (dst.view_source != null) {
                TensorVal* source_tensor = dst.view_source.tensor_val;
                dst.data = source_tensor.data;
                dst.device_handle = source_tensor.device_handle;
                dst.device_finalizer = null;
            }
            return true;
    }
}

fn void tensor_cleanup_materialized_expr_edges(
    TensorVal* src,
    TensorVal* dst,
    main::ScopeRegion* target_scope,
    bool escape_lane = false
) {
    if (src == null || dst == null) return;
    switch (src.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return;
        case TENSOR_PAYLOAD_MAP:
            Value* copied_func = dst.map_func;
            boundary_cleanup_materialized_value(src.map_func, copied_func, target_scope, escape_lane);
            dst.map_func = null;

            Value* copied_left = dst.map_left;
            if (copied_left != copied_func) {
                boundary_cleanup_materialized_value(src.map_left, copied_left, target_scope, escape_lane);
            }
            dst.map_left = null;

            Value* copied_right = dst.map_right;
            if (copied_right != copied_func && copied_right != copied_left) {
                boundary_cleanup_materialized_value(src.map_right, copied_right, target_scope, escape_lane);
            }
            dst.map_right = null;
            return;
        case TENSOR_PAYLOAD_CONTRACT:
            Value* copied_left = dst.contract_left;
            boundary_cleanup_materialized_value(src.contract_left, copied_left, target_scope, escape_lane);
            dst.contract_left = null;

            Value* copied_right = dst.contract_right;
            if (copied_right != copied_left) {
                boundary_cleanup_materialized_value(src.contract_right, copied_right, target_scope, escape_lane);
            }
            dst.contract_right = null;
            return;
        case TENSOR_PAYLOAD_VIEW:
            Value* copied_source = dst.view_source;
            boundary_cleanup_materialized_value(src.view_source, copied_source, target_scope, escape_lane);
            dst.view_source = null;
            dst.data = null;
            dst.device_handle = null;
            dst.device_finalizer = null;
            return;
    }
}

fn void boundary_cleanup_materialized_tensor(
    Value* src,
    Value* copied,
    main::ScopeRegion* target_scope,
    bool escape_lane
) @inline {
    if (src == null || copied == null || src.tag != TENSOR || copied.tag != TENSOR) return;
    tensor_cleanup_materialized_expr_edges(src.tensor_val, copied.tensor_val, target_scope, escape_lane);
}

fn Value* tensor_promote_expr_child_to_escape(Value* child, Interp* interp, char[] message) {
    if (child != null && tensor_should_force_expr_edge_fail(&g_tensor_force_expr_edge_escape_fail_nth)) {
        return raise_error(interp, message);
    }
    if (child != null && interp != null && interp.current_scope != null &&
        child.scope_gen == interp.current_scope.escape_generation) {
        return child;
    }
    Value* promoted = boundary_promote_to_escape(child, interp);
    if (child != null && (promoted == null || is_error(promoted))) {
        return promoted == null ? raise_error(interp, message) : promoted;
    }
    return promoted;
}

fn bool tensor_promote_expr_edges_to_escape(TensorVal* dst, TensorVal* src, Interp* interp) {
    if (dst == null || src == null) return true;
    switch (src.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return true;
        case TENSOR_PAYLOAD_MAP:
            dst.map_func = tensor_promote_expr_child_to_escape(src.map_func, interp, "boundary: failed to promote tensor map function");
            if (src.map_func != null && (dst.map_func == null || is_error(dst.map_func))) return false;
            dst.map_left = tensor_promote_expr_child_to_escape(src.map_left, interp, "boundary: failed to promote tensor map left input");
            if (src.map_left != null && (dst.map_left == null || is_error(dst.map_left))) return false;
            dst.map_right = tensor_promote_expr_child_to_escape(src.map_right, interp, "boundary: failed to promote tensor map right input");
            if (src.map_right != null && (dst.map_right == null || is_error(dst.map_right))) return false;
            return true;
        case TENSOR_PAYLOAD_CONTRACT:
            dst.contract_left = tensor_promote_expr_child_to_escape(src.contract_left, interp, "boundary: failed to promote tensor contract left input");
            if (src.contract_left != null && (dst.contract_left == null || is_error(dst.contract_left))) return false;
            dst.contract_right = tensor_promote_expr_child_to_escape(src.contract_right, interp, "boundary: failed to promote tensor contract right input");
            if (src.contract_right != null && (dst.contract_right == null || is_error(dst.contract_right))) return false;
            return true;
        case TENSOR_PAYLOAD_VIEW:
            dst.view_source = tensor_promote_expr_child_to_escape(src.view_source, interp, "boundary: failed to promote tensor view source");
            if (src.view_source != null && (dst.view_source == null || is_error(dst.view_source) || dst.view_source.tag != TENSOR || dst.view_source.tensor_val == null)) return false;
            if (dst.view_source != null) {
                TensorVal* source_tensor = dst.view_source.tensor_val;
                dst.data = source_tensor.data;
                dst.device_handle = source_tensor.device_handle;
                dst.device_finalizer = null;
            }
            return true;
    }
}
