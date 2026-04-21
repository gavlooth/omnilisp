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
        case TENSOR_PAYLOAD_CONTRACT:
            if (!tensor_capture_count_nodes(tensor.contract_left, out_count)) return false;
            if (!tensor_capture_count_nodes(tensor.contract_right, out_count)) return false;
            *out_count += 1;
            return true;
        case TENSOR_PAYLOAD_VIEW:
            if (!tensor_capture_count_nodes(tensor.view_source, out_count)) return false;
            *out_count += 1;
            return true;
        default:
            return false;
    }
}

fn Value* tensor_capture_shape(TensorVal* tensor, Interp* interp) {
    return kernel_capture_tensor_shape(tensor, interp);
}

fn Value* tensor_capture_usz_value(Interp* interp, usz value, char[] label) {
    if (value > (usz)long.max) return tensor_runtime_raise(interp, label);
    return make_int(interp, (long)value);
}

fn Value* tensor_capture_usz_array(Interp* interp, usz* items, usz count) {
    Value* out = make_array_checked(interp, count, "tensor/capture: out of memory");
    if (out == null || out.tag == ERROR) return out;
    if (out.array_val == null) return nn_oom(interp);
    for (usz i = 0; i < count; i++) {
        Value* item = tensor_capture_usz_value(interp, items[i], "tensor/capture: tensor metadata value is too large");
        if (item == null || item.tag == ERROR) return item;
        out.array_val.items[i] = item;
        out.array_val.length++;
    }
    return out;
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

fn Value* tensor_capture_axis_array(Interp* interp, usz* axes, usz axis_count) {
    return tensor_capture_usz_array(interp, axes, axis_count);
}

fn SymbolId tensor_capture_memory_allocation_symbol(TensorVal* tensor, Interp* interp) {
    if (tensor == null) return interp.symbols.intern("unknown");
    switch (tensor.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return interp.symbols.intern("external");
        case TENSOR_PAYLOAD_MAP:
        case TENSOR_PAYLOAD_CONTRACT:
            return interp.symbols.intern("transient");
        case TENSOR_PAYLOAD_VIEW:
            return interp.symbols.intern("alias");
        default:
            return interp.symbols.intern("unknown");
    }
}

fn SymbolId tensor_capture_memory_owner_symbol(TensorVal* tensor, Interp* interp) {
    if (tensor == null) return interp.symbols.intern("invalid");
    switch (tensor.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return interp.symbols.intern("self");
        case TENSOR_PAYLOAD_MAP:
        case TENSOR_PAYLOAD_CONTRACT:
            return interp.symbols.intern("planned-transient");
        case TENSOR_PAYLOAD_VIEW:
            return interp.symbols.intern("view-source");
        default:
            return interp.symbols.intern("invalid");
    }
}

fn SymbolId tensor_capture_memory_write_policy_symbol(TensorVal* tensor, Interp* interp) {
    if (tensor == null) return interp.symbols.intern("invalid");
    switch (tensor.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return interp.symbols.intern("read-only");
        case TENSOR_PAYLOAD_MAP:
        case TENSOR_PAYLOAD_CONTRACT:
            return interp.symbols.intern("planned-write-once");
        case TENSOR_PAYLOAD_VIEW:
            return interp.symbols.intern("read-only-view");
        default:
            return interp.symbols.intern("invalid");
    }
}

fn Value* tensor_capture_memory_fields(Value* node, TensorVal* tensor, Interp* interp) {
    if (Value* err = nn_dict_set(node, interp, "element-count", tensor_capture_usz_value(interp, tensor.element_count, "tensor/capture: tensor element-count is too large"))) return err;
    if (Value* err = nn_dict_set(node, interp, "byte-length", tensor_capture_usz_value(interp, tensor.byte_len, "tensor/capture: tensor byte-length is too large"))) return err;
    if (Value* err = nn_dict_set(node, interp, "storage-offset", tensor_capture_usz_value(interp, tensor.storage_offset, "tensor/capture: storage offset is too large"))) return err;
    if (Value* err = nn_dict_set(node, interp, "storage-elements", tensor_capture_usz_value(interp, tensor.storage_element_count, "tensor/capture: tensor storage element count is too large"))) return err;
    if (Value* err = nn_dict_set(node, interp, "storage-bytes", tensor_capture_usz_value(interp, tensor.storage_byte_len, "tensor/capture: tensor storage byte length is too large"))) return err;
    if (Value* err = nn_dict_set(node, interp, "allocation", make_symbol(interp, tensor_capture_memory_allocation_symbol(tensor, interp)))) return err;
    if (Value* err = nn_dict_set(node, interp, "owner", make_symbol(interp, tensor_capture_memory_owner_symbol(tensor, interp)))) return err;
    if (Value* err = nn_dict_set(node, interp, "write-policy", make_symbol(interp, tensor_capture_memory_write_policy_symbol(tensor, interp)))) return err;
    return null;
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
    Value* node = nn_make_dict(interp, 20);
    if (node == null || node.tag == ERROR) return node;
    if (Value* err = nn_dict_set_symbol(node, interp, "kind", "tensor-source")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "backend", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "dtype", "Float32")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "device", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "execution", "external-buffer")) return err;
    if (Value* err = nn_dict_set(node, interp, "shape", tensor_capture_shape(tensor, interp))) return err;
    if (Value* err = tensor_capture_memory_fields(node, tensor, interp)) return err;
    return node;
}

fn Value* tensor_capture_map_node(TensorVal* tensor, int left_id, int right_id, bool has_right_id, uint op, Interp* interp) {
    Value* node = nn_make_dict(interp, 24);
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
    if (Value* err = nn_dict_set_symbol(node, interp, "execution", "direct-helper")) return err;
    if (Value* err = nn_dict_set(node, interp, "arity", make_int(interp, tensor.map_is_binary ? 2 : 1))) return err;
    if (Value* err = nn_dict_set(node, interp, "inputs", inputs)) return err;
    if (Value* err = nn_dict_set(node, interp, "shape", tensor_capture_shape(tensor, interp))) return err;
    if (tensor.map_is_binary && tensor.map_left == null) {
        if (Value* err = tensor_capture_set_scalar(node, interp, "left-scalar", tensor.map_left_scalar)) return err;
    }
    if (tensor.map_is_binary && tensor.map_right == null) {
        if (Value* err = tensor_capture_set_scalar(node, interp, "right-scalar", tensor.map_right_scalar)) return err;
    }
    if (Value* err = tensor_capture_memory_fields(node, tensor, interp)) return err;
    return node;
}

fn Value* tensor_capture_contract_node(TensorVal* tensor, int left_id, int right_id, Interp* interp) {
    Value* node = nn_make_dict(interp, 24);
    if (node == null || node.tag == ERROR) return node;
    if (tensor.contract_axis_count > 0 && (tensor.contract_left_axes == null || tensor.contract_right_axes == null)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/capture: contract axes are missing", null);
    }
    Value* inputs = tensor_capture_ids_array(interp, left_id, right_id, true);
    if (inputs == null || inputs.tag == ERROR) return inputs;
    Value* left_axes = tensor_capture_axis_array(interp, tensor.contract_left_axes, tensor.contract_axis_count);
    if (left_axes == null || left_axes.tag == ERROR) return left_axes;
    Value* right_axes = tensor_capture_axis_array(interp, tensor.contract_right_axes, tensor.contract_axis_count);
    if (right_axes == null || right_axes.tag == ERROR) return right_axes;
    if (Value* err = nn_dict_set_symbol(node, interp, "kind", "tensor-contract")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "backend", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "dtype", "Float32")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "device", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "family", "contract-f32")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "execution", "direct-helper")) return err;
    if (Value* err = nn_dict_set(node, interp, "arity", make_int(interp, 2))) return err;
    if (Value* err = nn_dict_set(node, interp, "inputs", inputs)) return err;
    if (Value* err = nn_dict_set(node, interp, "left-axes", left_axes)) return err;
    if (Value* err = nn_dict_set(node, interp, "right-axes", right_axes)) return err;
    if (Value* err = nn_dict_set(node, interp, "axis-count", tensor_capture_usz_value(interp, tensor.contract_axis_count, "tensor/capture: contract axis-count is too large"))) return err;
    if (Value* err = nn_dict_set(node, interp, "shape", tensor_capture_shape(tensor, interp))) return err;
    if (Value* err = tensor_capture_memory_fields(node, tensor, interp)) return err;
    return node;
}

fn bool tensor_capture_is_transpose_view(TensorVal* view) {
    if (view == null || view.kind != TENSOR_PAYLOAD_VIEW || !view.is_view || view.device != TENSOR_DEVICE_VULKAN) return false;
    if (view.dtype != TENSOR_DTYPE_FLOAT32 || view.rank != 2 || view.shape == null || view.strides == null) return false;
    if (view.storage_offset != 0 || view.view_source == null || !is_tensor(view.view_source)) return false;
    TensorVal* source = view.view_source.tensor_val;
    if (source == null || source.kind != TENSOR_PAYLOAD_CONCRETE || source.device != TENSOR_DEVICE_VULKAN) return false;
    if (source.dtype != TENSOR_DTYPE_FLOAT32 || source.rank != 2 || source.shape == null || source.strides == null) return false;
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_VULKAN)) return false;
    if (view.device_handle != source.device_handle) return false;
    if (view.shape[0] != source.shape[1] || view.shape[1] != source.shape[0]) return false;
    if (view.strides[0] != source.strides[1] || view.strides[1] != source.strides[0]) return false;
    return true;
}

fn Value* tensor_capture_view_node(TensorVal* tensor, int source_id, Interp* interp) {
    if (!tensor_capture_is_transpose_view(tensor)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/capture: currently supports direct Vulkan Float32 transpose views", null);
    }
    Value* node = nn_make_dict(interp, 24);
    if (node == null || node.tag == ERROR) return node;
    Value* inputs = tensor_capture_ids_array(interp, source_id, -1, false);
    if (inputs == null || inputs.tag == ERROR) return inputs;
    Value* strides = tensor_capture_usz_array(interp, tensor.strides, tensor.rank);
    if (strides == null || strides.tag == ERROR) return strides;
    if (Value* err = nn_dict_set_symbol(node, interp, "kind", "tensor-view")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "backend", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "dtype", "Float32")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "device", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "family", "transpose-view-f32")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "operation", "transpose-view")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "execution", "metadata-only")) return err;
    if (Value* err = nn_dict_set(node, interp, "arity", make_int(interp, 1))) return err;
    if (Value* err = nn_dict_set(node, interp, "inputs", inputs)) return err;
    if (Value* err = nn_dict_set(node, interp, "shape", tensor_capture_shape(tensor, interp))) return err;
    if (Value* err = nn_dict_set(node, interp, "strides", strides)) return err;
    if (Value* err = nn_dict_set(node, interp, "storage-offset", tensor_capture_usz_value(interp, tensor.storage_offset, "tensor/capture: storage offset is too large"))) return err;
    if (Value* err = tensor_capture_memory_fields(node, tensor, interp)) return err;
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
        case TENSOR_PAYLOAD_CONTRACT: {
            int left_id = -1;
            int right_id = -1;
            if (Value* err = tensor_capture_append_graph(tensor.contract_left, nodes, next_id, &left_id, interp)) return err;
            if (Value* err = tensor_capture_append_graph(tensor.contract_right, nodes, next_id, &right_id, interp)) return err;
            Value* node = tensor_capture_contract_node(tensor, left_id, right_id, interp);
            if (node == null || node.tag == ERROR) return node;
            return tensor_capture_append(nodes, node, next_id, out_id, interp);
        }
        case TENSOR_PAYLOAD_VIEW: {
            int source_id = -1;
            if (Value* err = tensor_capture_append_graph(tensor.view_source, nodes, next_id, &source_id, interp)) return err;
            Value* node = tensor_capture_view_node(tensor, source_id, interp);
            if (node == null || node.tag == ERROR) return node;
            return tensor_capture_append(nodes, node, next_id, out_id, interp);
        }
        default:
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/capture: currently supports concrete, map, contract, and direct transpose-view Tensor graphs", null);
    }
}

fn char[] tensor_capture_plan_family(TensorPayloadKind kind) {
    switch (kind) {
        case TENSOR_PAYLOAD_MAP: return "map-expression";
        case TENSOR_PAYLOAD_CONTRACT: return "contract-expression";
        case TENSOR_PAYLOAD_VIEW: return "view-expression";
        default: return "tensor-expression";
    }
}

fn bool tensor_capture_node_launches(Value* node, Interp* interp) {
    Value* execution = nn_dict_get(node, interp, "execution");
    return nn_symbol_is(execution, interp, "direct-helper");
}

fn Value* tensor_capture_empty_ids(Interp* interp) {
    return make_array_checked(interp, 0, "tensor/capture: out of memory");
}

fn Value* tensor_capture_schedule_entry(Value* node, usz step, bool launch, Interp* interp) {
    if (node == null || node.tag != HASHMAP) return nn_oom(interp);
    Value* entry = nn_make_dict(interp, 10);
    if (entry == null || entry.tag == ERROR) return entry;
    Value* id = nn_dict_get(node, interp, "id");
    Value* kind = nn_dict_get(node, interp, "kind");
    Value* execution = nn_dict_get(node, interp, "execution");
    Value* inputs = nn_dict_get(node, interp, "inputs");
    if (id == null || kind == null || execution == null) return tensor_runtime_raise(interp, "tensor/capture: graph schedule metadata is missing");
    if (inputs == null) {
        inputs = tensor_capture_empty_ids(interp);
        if (inputs == null || inputs.tag == ERROR) return inputs;
    }
    if (Value* err = nn_dict_set(entry, interp, "step", tensor_capture_usz_value(interp, step, "tensor/capture: schedule step is too large"))) return err;
    if (Value* err = nn_dict_set(entry, interp, "node", id)) return err;
    if (Value* err = nn_dict_set(entry, interp, "kind", kind)) return err;
    if (Value* err = nn_dict_set(entry, interp, "execution", execution)) return err;
    if (Value* err = nn_dict_set(entry, interp, "depends-on", inputs)) return err;
    if (Value* err = nn_dict_set(entry, interp, "launch", make_bool_value(interp, launch))) return err;
    return entry;
}

fn Value* tensor_capture_array_push(Value* array, Value* value, Interp* interp) {
    if (array == null || array.tag != ARRAY || array.array_val == null) return nn_oom(interp);
    if (value == null) return nn_oom(interp);
    if (value.tag == ERROR) return value;
    if (array.array_val.length >= array.array_val.capacity) return nn_oom(interp);
    array.array_val.items[array.array_val.length] = value;
    array.array_val.length++;
    return null;
}

fn bool tensor_capture_int_array_contains(Value* array, long value) {
    if (array == null || array.tag != ARRAY || array.array_val == null) return false;
    for (usz i = 0; i < array.array_val.length; i++) {
        Value* item = array.array_val.items[i];
        if (item != null && item.tag == INT && item.int_val == value) return true;
    }
    return false;
}

fn Value* tensor_capture_command_dispatch(Value* node, usz index, Interp* interp) {
    if (node == null || node.tag != HASHMAP) return nn_oom(interp);
    Value* entry = nn_make_dict(interp, 16);
    if (entry == null || entry.tag == ERROR) return entry;
    Value* id = nn_dict_get(node, interp, "id");
    Value* kind = nn_dict_get(node, interp, "kind");
    Value* family = nn_dict_get(node, interp, "family");
    Value* operation = nn_dict_get(node, interp, "operation");
    Value* execution = nn_dict_get(node, interp, "execution");
    Value* inputs = nn_dict_get(node, interp, "inputs");
    Value* shape = nn_dict_get(node, interp, "shape");
    if (id == null || kind == null || execution == null) return tensor_runtime_raise(interp, "tensor/capture: graph command-batch metadata is missing");
    if (inputs == null) {
        inputs = tensor_capture_empty_ids(interp);
        if (inputs == null || inputs.tag == ERROR) return inputs;
    }
    if (Value* err = nn_dict_set(entry, interp, "index", tensor_capture_usz_value(interp, index, "tensor/capture: dispatch index is too large"))) return err;
    if (Value* err = nn_dict_set(entry, interp, "node", id)) return err;
    if (Value* err = nn_dict_set(entry, interp, "kind", kind)) return err;
    if (family != null) {
        if (Value* err = nn_dict_set(entry, interp, "family", family)) return err;
    }
    if (operation != null) {
        if (Value* err = nn_dict_set(entry, interp, "operation", operation)) return err;
    }
    if (Value* err = nn_dict_set(entry, interp, "execution", execution)) return err;
    if (Value* err = nn_dict_set(entry, interp, "inputs", inputs)) return err;
    if (Value* err = nn_dict_set(entry, interp, "output", id)) return err;
    if (shape != null) {
        if (Value* err = nn_dict_set(entry, interp, "shape", shape)) return err;
    }
    if (Value* err = nn_dict_set_symbol(entry, interp, "source-runtime", "direct-helper")) return err;
    if (Value* err = nn_dict_set_symbol(entry, interp, "recording-support", "not-implemented")) return err;
    return entry;
}

fn Value* tensor_capture_command_batches(Value* nodes, Interp* interp, int* out_batch_count) {
    if (nodes == null || nodes.tag != ARRAY || nodes.array_val == null || out_batch_count == null) return nn_oom(interp);
    usz launch_count = 0;
    for (usz i = 0; i < nodes.array_val.length; i++) {
        if (tensor_capture_node_launches(nodes.array_val.items[i], interp)) launch_count++;
    }
    *out_batch_count = launch_count > 0 ? 1 : 0;
    Value* batches = make_array_checked(interp, launch_count > 0 ? 1 : 0, "tensor/capture: out of memory");
    if (batches == null || batches.tag == ERROR) return batches;
    if (batches.array_val == null) return nn_oom(interp);
    if (launch_count == 0) return batches;

    Value* batch = nn_make_dict(interp, 20);
    if (batch == null || batch.tag == ERROR) return batch;
    Value* batch_nodes = make_array_checked(interp, launch_count, "tensor/capture: out of memory");
    Value* schedule_steps = make_array_checked(interp, launch_count, "tensor/capture: out of memory");
    Value* dispatches = make_array_checked(interp, launch_count, "tensor/capture: out of memory");
    Value* depends_on = make_array_checked(interp, nodes.array_val.length, "tensor/capture: out of memory");
    if (batch_nodes == null || batch_nodes.tag == ERROR) return batch_nodes;
    if (schedule_steps == null || schedule_steps.tag == ERROR) return schedule_steps;
    if (dispatches == null || dispatches.tag == ERROR) return dispatches;
    if (depends_on == null || depends_on.tag == ERROR) return depends_on;
    if (batch_nodes.array_val == null || schedule_steps.array_val == null || dispatches.array_val == null || depends_on.array_val == null) return nn_oom(interp);

    usz dispatch_index = 0;
    for (usz i = 0; i < nodes.array_val.length; i++) {
        Value* node = nodes.array_val.items[i];
        if (!tensor_capture_node_launches(node, interp)) continue;
        Value* id = nn_dict_get(node, interp, "id");
        if (id == null || id.tag != INT) return tensor_runtime_raise(interp, "tensor/capture: command-batch node id is missing");
        if (Value* err = tensor_capture_array_push(batch_nodes, id, interp)) return err;
        if (Value* err = tensor_capture_array_push(schedule_steps, tensor_capture_usz_value(interp, i, "tensor/capture: command-batch schedule step is too large"), interp)) return err;
        Value* dispatch = tensor_capture_command_dispatch(node, dispatch_index, interp);
        if (dispatch == null || dispatch.tag == ERROR) return dispatch;
        if (Value* err = tensor_capture_array_push(dispatches, dispatch, interp)) return err;
        dispatch_index++;
    }

    for (usz i = 0; i < nodes.array_val.length; i++) {
        Value* node = nodes.array_val.items[i];
        if (!tensor_capture_node_launches(node, interp)) continue;
        Value* inputs = nn_dict_get(node, interp, "inputs");
        if (inputs == null || inputs.tag != ARRAY || inputs.array_val == null) continue;
        for (usz j = 0; j < inputs.array_val.length; j++) {
            Value* dep = inputs.array_val.items[j];
            if (dep == null || dep.tag != INT) continue;
            if (tensor_capture_int_array_contains(batch_nodes, dep.int_val)) continue;
            if (tensor_capture_int_array_contains(depends_on, dep.int_val)) continue;
            if (Value* err = tensor_capture_array_push(depends_on, dep, interp)) return err;
        }
    }

    if (Value* err = nn_dict_set(batch, interp, "id", make_int(interp, 0))) return err;
    if (Value* err = nn_dict_set_symbol(batch, interp, "backend", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(batch, interp, "device", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(batch, interp, "dtype", "Float32")) return err;
    if (Value* err = nn_dict_set_symbol(batch, interp, "execution", "command-buffer-candidate")) return err;
    if (Value* err = nn_dict_set_symbol(batch, interp, "submission", "not-submitted")) return err;
    if (Value* err = nn_dict_set(batch, interp, "nodes", batch_nodes)) return err;
    if (Value* err = nn_dict_set(batch, interp, "schedule-steps", schedule_steps)) return err;
    if (Value* err = nn_dict_set(batch, interp, "depends-on", depends_on)) return err;
    if (Value* err = nn_dict_set(batch, interp, "dispatches", dispatches)) return err;
    if (Value* err = nn_dict_set(batch, interp, "launch-count", tensor_capture_usz_value(interp, launch_count, "tensor/capture: command-batch launch-count is too large"))) return err;
    if (Value* err = nn_dict_set(batch, interp, "dispatch-count", tensor_capture_usz_value(interp, launch_count, "tensor/capture: command-batch dispatch-count is too large"))) return err;
    if (Value* err = nn_dict_set(batch, interp, "barrier-count", tensor_capture_usz_value(interp, launch_count > 0 ? launch_count - 1 : 0, "tensor/capture: command-batch barrier-count is too large"))) return err;
    if (Value* err = nn_dict_set_symbol(batch, interp, "barrier-policy", "shader-write-to-shader-read-between-dispatches")) return err;
    if (Value* err = nn_dict_set_symbol(batch, interp, "fusion", "none")) return err;
    if (Value* err = tensor_capture_array_push(batches, batch, interp)) return err;
    return batches;
}

fn Value* tensor_capture_schedule(Value* nodes, Interp* interp, int* out_launch_count) {
    if (nodes == null || nodes.tag != ARRAY || nodes.array_val == null || out_launch_count == null) return nn_oom(interp);
    *out_launch_count = 0;
    Value* schedule = make_array_checked(interp, nodes.array_val.length, "tensor/capture: out of memory");
    if (schedule == null || schedule.tag == ERROR) return schedule;
    if (schedule.array_val == null) return nn_oom(interp);
    for (usz i = 0; i < nodes.array_val.length; i++) {
        Value* node = nodes.array_val.items[i];
        bool launch = tensor_capture_node_launches(node, interp);
        if (launch) *out_launch_count += 1;
        Value* entry = tensor_capture_schedule_entry(node, i, launch, interp);
        if (entry == null || entry.tag == ERROR) return entry;
        schedule.array_val.items[i] = entry;
        schedule.array_val.length++;
    }
    return schedule;
}

fn Value* tensor_capture_memory_plan_bytes(Value* nodes, Interp* interp, usz* external_bytes, usz* transient_bytes) {
    if (nodes == null || nodes.tag != ARRAY || nodes.array_val == null || external_bytes == null || transient_bytes == null) return nn_oom(interp);
    *external_bytes = 0;
    *transient_bytes = 0;
    for (usz i = 0; i < nodes.array_val.length; i++) {
        Value* node = nodes.array_val.items[i];
        if (node == null || node.tag != HASHMAP) return nn_oom(interp);
        Value* allocation = nn_dict_get(node, interp, "allocation");
        Value* storage_bytes = nn_dict_get(node, interp, "storage-bytes");
        Value* byte_length = nn_dict_get(node, interp, "byte-length");
        if (allocation == null || allocation.tag != SYMBOL) continue;
        if (nn_symbol_is(allocation, interp, "external")) {
            if (storage_bytes != null && storage_bytes.tag == INT && storage_bytes.int_val >= 0) {
                usz bytes = (usz)storage_bytes.int_val;
                if (*external_bytes > (usz)long.max - bytes) return tensor_runtime_raise(interp, "tensor/capture: external-bytes is too large");
                *external_bytes += bytes;
            }
        } else if (nn_symbol_is(allocation, interp, "transient")) {
            if (byte_length != null && byte_length.tag == INT && byte_length.int_val >= 0) {
                usz bytes = (usz)byte_length.int_val;
                if (*transient_bytes > (usz)long.max - bytes) return tensor_runtime_raise(interp, "tensor/capture: transient-bytes is too large");
                *transient_bytes += bytes;
            }
        }
    }
    return null;
}

fn Value* tensor_capture_memory_plan(Value* nodes, usz external_bytes, usz transient_bytes, Interp* interp) {
    Value* plan = nn_make_dict(interp, 10);
    if (plan == null || plan.tag == ERROR) return plan;
    if (Value* err = nn_dict_set_symbol(plan, interp, "kind", "tensor-memory-plan")) return err;
    if (Value* err = nn_dict_set(plan, interp, "version", make_int(interp, 1))) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "backend", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "dtype", "Float32")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "policy", "metadata-only")) return err;
    if (Value* err = nn_dict_set(plan, interp, "allocates", make_bool_value(interp, false))) return err;
    if (Value* err = nn_dict_set(plan, interp, "retains-handles", make_bool_value(interp, false))) return err;
    if (Value* err = nn_dict_set(plan, interp, "node-memory", nodes)) return err;
    if (Value* err = nn_dict_set(plan, interp, "external-bytes", tensor_capture_usz_value(interp, external_bytes, "tensor/capture: external-bytes is too large"))) return err;
    if (Value* err = nn_dict_set(plan, interp, "transient-bytes", tensor_capture_usz_value(interp, transient_bytes, "tensor/capture: transient-bytes is too large"))) return err;
    return plan;
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
    key.array_val.items[3] = tensor_capture_usz_value(interp, nodes.array_val.length, "tensor/capture: invalidation node-count is too large");
    key.array_val.items[4] = make_int(interp, (long)output_id);
    for (usz i = 0; i < 5; i++) {
        if (key.array_val.items[i] == null || key.array_val.items[i].tag == ERROR) return key.array_val.items[i];
    }
    key.array_val.length = 5;
    int launch_count = 0;
    Value* schedule = tensor_capture_schedule(nodes, interp, &launch_count);
    if (schedule == null || schedule.tag == ERROR) return schedule;
    int command_batch_count = 0;
    Value* command_batches = tensor_capture_command_batches(nodes, interp, &command_batch_count);
    if (command_batches == null || command_batches.tag == ERROR) return command_batches;
    usz external_bytes = 0;
    usz transient_bytes = 0;
    if (Value* err = tensor_capture_memory_plan_bytes(nodes, interp, &external_bytes, &transient_bytes)) return err;
    Value* memory_plan = tensor_capture_memory_plan(nodes, external_bytes, transient_bytes, interp);
    if (memory_plan == null || memory_plan.tag == ERROR) return memory_plan;
    Value* fusion_plan = tensor_capture_fusion_plan(nodes, interp);
    if (fusion_plan == null || fusion_plan.tag == ERROR) return fusion_plan;

    Value* plan = nn_make_dict(interp, 30);
    if (plan == null || plan.tag == ERROR) return plan;
    if (Value* err = nn_dict_set_symbol(plan, interp, "kind", "tensor-graph")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "backend", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "dtype", "Float32")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "device", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "family", tensor_capture_plan_family(tensor.kind))) return err;
    if (Value* err = nn_dict_set(plan, interp, "node-count", tensor_capture_usz_value(interp, nodes.array_val.length, "tensor/capture: node-count is too large"))) return err;
    if (Value* err = nn_dict_set(plan, interp, "output", make_int(interp, (long)output_id))) return err;
    if (Value* err = nn_dict_set(plan, interp, "nodes", nodes)) return err;
    if (Value* err = nn_dict_set(plan, interp, "schedule", schedule)) return err;
    if (Value* err = nn_dict_set(plan, interp, "launch-count", tensor_capture_usz_value(interp, (usz)launch_count, "tensor/capture: launch-count is too large"))) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "execution", "not-launched")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "command-batching", command_batch_count > 0 ? "metadata" : "none")) return err;
    if (Value* err = nn_dict_set(plan, interp, "command-batch-count", tensor_capture_usz_value(interp, (usz)command_batch_count, "tensor/capture: command-batch-count is too large"))) return err;
    if (Value* err = nn_dict_set(plan, interp, "command-batches", command_batches)) return err;
    if (Value* err = nn_dict_set(plan, interp, "dispatch-count", tensor_capture_usz_value(interp, (usz)launch_count, "tensor/capture: dispatch-count is too large"))) return err;
    if (Value* err = nn_dict_set(plan, interp, "barrier-count", tensor_capture_usz_value(interp, launch_count > 0 ? (usz)(launch_count - 1) : 0, "tensor/capture: barrier-count is too large"))) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "batching-policy", command_batch_count > 0 ? "serial-single-command-buffer-candidate" : "none")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "submission", "not-submitted")) return err;
    if (Value* err = nn_dict_set(plan, interp, "requires-runtime-support", make_bool_value(interp, command_batch_count > 0))) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "fusion", "none")) return err;
    if (Value* err = nn_dict_set(plan, interp, "fusion-plan", fusion_plan)) return err;
    if (Value* err = nn_dict_set(plan, interp, "memory-plan", memory_plan)) return err;
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
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/capture: currently supports concrete, map, contract, and direct transpose-view Tensor graphs", null);
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
