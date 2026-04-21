module lisp;

fn char[] tensor_capture_unary_op_name(uint op) {
    switch (op) {
        case 0: return "abs";
        case 1: return "neg";
        case 2: return "sqrt";
        case 3: return "identity";
        case 4: return "zero";
        case 5: return "sin";
        case 6: return "cos";
        case 7: return "tan";
        case 8: return "asin";
        case 9: return "acos";
        case 10: return "atan";
        case 11: return "sinh";
        case 12: return "cosh";
        case 13: return "tanh";
        case 14: return "exp";
        case 15: return "log";
        case 16: return "log10";
        case 19: return "normal-cdf";
        case 20: return "normal-quantile";
        default: return "unknown";
    }
}

fn char[] tensor_capture_binary_op_name(uint op) {
    switch (op) {
        case 0: return "add";
        case 1: return "sub";
        case 2: return "mul";
        case 3: return "div";
        case 4: return "min";
        case 5: return "max";
        default: return "unknown";
    }
}

fn bool tensor_capture_count_nodes(Value* value, int* out_count) {
    if (!is_tensor(value) || value.tensor_val == null || out_count == null) return false;
    TensorVal* tensor = value.tensor_val;
    switch (tensor.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            *out_count += 1;
            return true;
        case TENSOR_PAYLOAD_MAP:
            if (tensor.map_left != null && !tensor_capture_count_nodes(tensor.map_left, out_count)) return false;
            if (tensor.map_is_binary && tensor.map_right != null && !tensor_capture_count_nodes(tensor.map_right, out_count)) return false;
            *out_count += 1;
            return true;
        default:
            return false;
    }
}

fn Value* tensor_capture_shape(TensorVal* tensor, Interp* interp) {
    return kernel_capture_tensor_shape(tensor, interp);
}

fn Value* tensor_capture_ids_array(Interp* interp, int left_id, int right_id, bool has_right) {
    Value* out = make_array_checked(interp, has_right ? 2 : 1, "tensor/capture: out of memory");
    if (out == null || out.tag == ERROR) return out;
    if (out.array_val == null) return nn_oom(interp);
    out.array_val.items[0] = make_int(interp, (long)left_id);
    if (out.array_val.items[0] == null) return nn_oom(interp);
    out.array_val.length = 1;
    if (has_right) {
        out.array_val.items[1] = make_int(interp, (long)right_id);
        if (out.array_val.items[1] == null) return nn_oom(interp);
        out.array_val.length = 2;
    }
    return out;
}

fn Value* tensor_capture_set_scalar(Value* node, Interp* interp, char[] key, double scalar) {
    float scalar_f32 = (float)scalar;
    Value* value = make_float32(interp, scalar_f32);
    if (value == null) return nn_oom(interp);
    return nn_dict_set(node, interp, key, value);
}

fn Value* tensor_capture_append(Value* nodes, Value* node, int* next_id, int* out_id, Interp* interp) {
    if (nodes == null || nodes.tag != ARRAY || nodes.array_val == null || next_id == null || out_id == null) return nn_oom(interp);
    if (nodes.array_val.length >= nodes.array_val.capacity) return nn_oom(interp);
    int id = *next_id;
    if (Value* err = nn_dict_set(node, interp, "id", make_int(interp, (long)id))) return err;
    nodes.array_val.items[nodes.array_val.length] = node;
    nodes.array_val.length++;
    *out_id = id;
    *next_id = id + 1;
    return null;
}

fn Value* tensor_capture_leaf_node(TensorVal* tensor, Interp* interp) {
    if (tensor == null || !tensor_has_valid_metadata(tensor)) return tensor_runtime_raise(interp, "tensor/capture: tensor metadata is missing");
    if (tensor.kind != TENSOR_PAYLOAD_CONCRETE || tensor.device != TENSOR_DEVICE_VULKAN || tensor.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/capture: leaf tensors must be concrete Vulkan Float32 tensors", null);
    }
    if (!tensor_has_valid_device_storage(tensor, TENSOR_DEVICE_VULKAN)) return tensor_runtime_raise(interp, "tensor/capture: Vulkan tensor backing storage is missing");
    Value* node = nn_make_dict(interp, 12);
    if (node == null || node.tag == ERROR) return node;
    if (Value* err = nn_dict_set_symbol(node, interp, "kind", "tensor-source")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "backend", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "dtype", "Float32")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "device", "vulkan")) return err;
    if (Value* err = nn_dict_set(node, interp, "shape", tensor_capture_shape(tensor, interp))) return err;
    return node;
}

fn Value* tensor_capture_map_node(TensorVal* tensor, int left_id, int right_id, bool has_right_id, uint op, Interp* interp) {
    Value* node = nn_make_dict(interp, 18);
    if (node == null || node.tag == ERROR) return node;
    Value* inputs = tensor_capture_ids_array(interp, left_id, right_id, has_right_id);
    if (inputs == null || inputs.tag == ERROR) return inputs;
    char[] op_name = tensor.map_is_binary ? tensor_capture_binary_op_name(op) : tensor_capture_unary_op_name(op);
    if (Value* err = nn_dict_set_symbol(node, interp, "kind", "tensor-map")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "backend", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "dtype", "Float32")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "device", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "family", tensor.map_is_binary ? "binary-map-f32" : "unary-map-f32")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "operation", op_name)) return err;
    if (Value* err = nn_dict_set(node, interp, "arity", make_int(interp, tensor.map_is_binary ? 2 : 1))) return err;
    if (Value* err = nn_dict_set(node, interp, "inputs", inputs)) return err;
    if (Value* err = nn_dict_set(node, interp, "shape", tensor_capture_shape(tensor, interp))) return err;
    if (tensor.map_is_binary && tensor.map_left == null) {
        if (Value* err = tensor_capture_set_scalar(node, interp, "left-scalar", tensor.map_left_scalar)) return err;
    }
    if (tensor.map_is_binary && tensor.map_right == null) {
        if (Value* err = tensor_capture_set_scalar(node, interp, "right-scalar", tensor.map_right_scalar)) return err;
    }
    return node;
}

fn Value* tensor_capture_append_graph(Value* value, Value* nodes, int* next_id, int* out_id, Interp* interp) {
    if (!is_tensor(value) || value.tensor_val == null) return primitive_arg_type_raise(interp, "tensor/capture", 0, "Tensor", value);
    TensorVal* tensor = value.tensor_val;
    if (!tensor_has_valid_metadata(tensor)) return tensor_runtime_raise(interp, "tensor/capture: tensor metadata is missing");
    if (tensor.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/capture: currently supports Vulkan Float32 Tensor graphs", null);
    }
    switch (tensor.kind) {
        case TENSOR_PAYLOAD_CONCRETE: {
            Value* node = tensor_capture_leaf_node(tensor, interp);
            if (node == null || node.tag == ERROR) return node;
            return tensor_capture_append(nodes, node, next_id, out_id, interp);
        }
        case TENSOR_PAYLOAD_MAP: {
            uint op = 0;
            Value* preflight = tensor_map_vulkan_callable_op_or_raise(tensor.map_func, tensor.map_is_binary, tensor.dtype, &op, interp);
            if (preflight != null) return preflight;
            int left_id = -1;
            int right_id = -1;
            bool has_right_id = false;
            if (tensor.map_left != null) {
                if (Value* err = tensor_capture_append_graph(tensor.map_left, nodes, next_id, &left_id, interp)) return err;
            }
            if (tensor.map_is_binary && tensor.map_right != null) {
                if (Value* err = tensor_capture_append_graph(tensor.map_right, nodes, next_id, &right_id, interp)) return err;
                has_right_id = true;
            }
            if (tensor.map_left == null && tensor.map_right == null) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/capture: map graph must contain at least one Vulkan tensor operand", null);
            }
            if (tensor.map_left == null) {
                if (!has_right_id) return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/capture: scalar-only map graph is unsupported", null);
                left_id = right_id;
                has_right_id = false;
            }
            Value* node = tensor_capture_map_node(tensor, left_id, right_id, has_right_id, op, interp);
            if (node == null || node.tag == ERROR) return node;
            return tensor_capture_append(nodes, node, next_id, out_id, interp);
        }
        default:
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/capture: currently supports concrete and map Tensor graphs", null);
    }
}

fn Value* tensor_capture_plan(Value* source, Value* nodes, int output_id, Interp* interp) {
    TensorVal* tensor = source.tensor_val;
    Value* shape = tensor_capture_shape(tensor, interp);
    if (shape == null || shape.tag == ERROR) return shape;
    Value* key = make_array_checked(interp, 5, "tensor/capture: out of memory");
    if (key == null || key.tag == ERROR) return key;
    if (key.array_val == null) return nn_oom(interp);
    key.array_val.items[0] = kernel_capture_symbol(interp, "vulkan");
    key.array_val.items[1] = kernel_capture_symbol(interp, "Float32");
    key.array_val.items[2] = shape;
    key.array_val.items[3] = make_int(interp, (long)nodes.array_val.length);
    key.array_val.items[4] = make_int(interp, (long)output_id);
    for (usz i = 0; i < 5; i++) {
        if (key.array_val.items[i] == null || key.array_val.items[i].tag == ERROR) return key.array_val.items[i];
    }
    key.array_val.length = 5;

    Value* plan = nn_make_dict(interp, 18);
    if (plan == null || plan.tag == ERROR) return plan;
    if (Value* err = nn_dict_set_symbol(plan, interp, "kind", "tensor-graph")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "backend", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "dtype", "Float32")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "device", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "family", "map-expression")) return err;
    if (Value* err = nn_dict_set(plan, interp, "node-count", make_int(interp, (long)nodes.array_val.length))) return err;
    if (Value* err = nn_dict_set(plan, interp, "output", make_int(interp, (long)output_id))) return err;
    if (Value* err = nn_dict_set(plan, interp, "nodes", nodes)) return err;
    if (Value* err = nn_dict_set(plan, interp, "shape", shape)) return err;
    if (Value* err = nn_dict_set(plan, interp, "invalidation-key", key)) return err;
    return plan;
}

fn Value* prim_tensor_capture(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "tensor/capture", "1 argument: tensor", args.len);
    if (!is_tensor(args[0]) || args[0].tensor_val == null) return primitive_arg_type_raise(interp, "tensor/capture", 0, "Tensor", args[0]);

    TensorVal* source = args[0].tensor_val;
    if (!tensor_has_valid_metadata(source)) return tensor_runtime_raise(interp, "tensor/capture: tensor metadata is missing");
    if (source.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/capture: currently supports Vulkan Float32 Tensor graphs", null);
    }
    if (!tensor_expr_has_device(args[0], TENSOR_DEVICE_VULKAN) || tensor_expr_has_non_vulkan_device(args[0])) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/capture: graph must be fully backed by Vulkan tensors", null);
    }

    int count = 0;
    if (!tensor_capture_count_nodes(args[0], &count) || count <= 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/capture: currently supports concrete and map Tensor graphs", null);
    }
    Value* nodes = make_array_checked(interp, (usz)count, "tensor/capture: out of memory");
    if (nodes == null || nodes.tag == ERROR) return nodes;
    if (nodes.array_val == null) return nn_oom(interp);

    int next_id = 0;
    int output_id = -1;
    if (Value* err = tensor_capture_append_graph(args[0], nodes, &next_id, &output_id, interp)) return err;
    if (next_id != count || output_id < 0) return tensor_runtime_raise(interp, "tensor/capture: graph node count mismatch");
    return tensor_capture_plan(args[0], nodes, output_id, interp);
}
