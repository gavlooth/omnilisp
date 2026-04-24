module lisp;

import std::io;

const char[] NN_CKPT_TAG = "omni-nn-checkpoint";
const long NN_CKPT_VERSION = 1;

<*
@require interp != null : "nn_ckpt_invalid: null interp"
*>
fn Value* nn_ckpt_invalid(Interp* interp, char[] path, char[] expected, Value* actual) @inline {
    return nn_invalid(interp, path, expected, actual);
}

<*
@require interp != null : "nn_ckpt_io_error: null interp"
*>
fn Value* nn_ckpt_io_error(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "nn", "nn/checkpoint-io", message, null);
}

fn bool nn_ckpt_string_is(Value* value, char[] expected) @inline {
    return value != null && value.tag == STRING && ffi_char_slice_equals(value.str_chars[:value.str_len], expected);
}

<*
@require interp != null : "nn_ckpt_string_field: null interp"
*>
fn Value* nn_ckpt_string_field(Value* dict, Interp* interp, char[] key) {
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) return null;
    return hashmap_get(dict.hashmap_val, make_string(interp, key));
}

<*
@require interp != null : "nn_ckpt_set: null interp"
*>
fn Value* nn_ckpt_set(Value* dict, Interp* interp, char[] key, Value* value) {
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) return nn_oom(interp);
    Value* key_value = make_string(interp, key);
    if (key_value == null || key_value.tag == ERROR) return key_value;
    if (value == null || value.tag == ERROR) return value;
    if (!hashmap_set_checked(dict.hashmap_val, key_value, value, interp)) return nn_oom(interp);
    return null;
}

<*
@require interp != null : "nn_ckpt_obj: null interp"
*>
fn Value* nn_ckpt_obj(Interp* interp, char[] tag, uint capacity) {
    Value* obj = nn_make_dict(interp, capacity);
    if (obj == null || obj.tag == ERROR) return obj;
    if (Value* err = nn_ckpt_set(obj, interp, "tag", make_string(interp, tag))) return err;
    return obj;
}

<*
@require interp != null : "nn_ckpt_pair: null interp"
@require key != null : "nn_ckpt_pair: null key"
@require value != null : "nn_ckpt_pair: null value"
*>
fn Value* nn_ckpt_pair(Interp* interp, Value* key, Value* value) {
    Value* pair = make_array_checked(interp, 2, "nn/checkpoint: out of memory");
    if (pair == null || pair.tag == ERROR) return pair;
    if (pair.tag != ARRAY || pair.array_val == null) return nn_oom(interp);
    pair.array_val.items[0] = key;
    pair.array_val.items[1] = value;
    pair.array_val.length = 2;
    return pair;
}

<*
@require value != null : "nn_ckpt_encode_array: null value"
@require value.tag == ARRAY : "nn_ckpt_encode_array: value must be Array"
@require value.array_val != null : "nn_ckpt_encode_array: missing array payload"
@require interp != null : "nn_ckpt_encode_array: null interp"
*>
fn Value* nn_ckpt_encode_array(Value* value, Interp* interp) {
    Value* items = make_array_checked(interp, value.array_val.length, "nn/checkpoint: out of memory");
    if (items == null || items.tag == ERROR) return items;
    if (items.tag != ARRAY || items.array_val == null) return nn_oom(interp);
    foreach (item : value.array_val.items[0:value.array_val.length]) {
        Value* encoded = nn_ckpt_encode_value(item, interp);
        if (encoded == null || encoded.tag == ERROR) return encoded;
        items.array_val.items[items.array_val.length++] = encoded;
    }
    Value* obj = nn_ckpt_obj(interp, "array", 4);
    if (obj == null || obj.tag == ERROR) return obj;
    if (Value* err = nn_ckpt_set(obj, interp, "items", items)) return err;
    return obj;
}

<*
@require value != null : "nn_ckpt_encode_list: null value"
@require value.tag == CONS : "nn_ckpt_encode_list: value must be Cons"
@require interp != null : "nn_ckpt_encode_list: null interp"
*>
fn Value* nn_ckpt_encode_list(Value* value, Interp* interp) {
    usz count = 0;
    Value* scan = value;
    while (is_cons(scan)) {
        if (count == usz.max) return nn_ckpt_invalid(interp, "checkpoint.list", "bounded proper list", value);
        count++;
        scan = scan.cons_val.cdr;
    }
    if (!is_nil(scan)) return nn_ckpt_invalid(interp, "checkpoint.list", "proper list", scan);

    Value* items = make_array_checked(interp, count, "nn/checkpoint: out of memory");
    if (items == null || items.tag == ERROR) return items;
    if (items.tag != ARRAY || items.array_val == null) return nn_oom(interp);
    Value* cur = value;
    while (is_cons(cur)) {
        Value* encoded = nn_ckpt_encode_value(cur.cons_val.car, interp);
        if (encoded == null || encoded.tag == ERROR) return encoded;
        items.array_val.items[items.array_val.length++] = encoded;
        cur = cur.cons_val.cdr;
    }
    Value* obj = nn_ckpt_obj(interp, "list", 4);
    if (obj == null || obj.tag == ERROR) return obj;
    if (Value* err = nn_ckpt_set(obj, interp, "items", items)) return err;
    return obj;
}

<*
@require value != null : "nn_ckpt_encode_dict: null value"
@require value.tag == HASHMAP : "nn_ckpt_encode_dict: value must be Dictionary"
@require value.hashmap_val != null : "nn_ckpt_encode_dict: missing dictionary payload"
@require interp != null : "nn_ckpt_encode_dict: null interp"
*>
fn Value* nn_ckpt_encode_dict(Value* value, Interp* interp) {
    HashMap* hm = value.hashmap_val;
    Value* entries = make_array_checked(interp, (usz)hm.count, "nn/checkpoint: out of memory");
    if (entries == null || entries.tag == ERROR) return entries;
    if (entries.tag != ARRAY || entries.array_val == null) return nn_oom(interp);
    foreach (entry : hm.entries[0:hm.capacity]) {
        if (entry.key == null) continue;
        Value* key = nn_ckpt_encode_value(entry.key, interp);
        if (key == null || key.tag == ERROR) return key;
        Value* item = nn_ckpt_encode_value(entry.value, interp);
        if (item == null || item.tag == ERROR) return item;
        Value* pair = nn_ckpt_pair(interp, key, item);
        if (pair == null || pair.tag == ERROR) return pair;
        entries.array_val.items[entries.array_val.length++] = pair;
    }
    Value* obj = nn_ckpt_obj(interp, "dict", 4);
    if (obj == null || obj.tag == ERROR) return obj;
    if (Value* err = nn_ckpt_set(obj, interp, "entries", entries)) return err;
    return obj;
}

<*
@require value != null : "nn_ckpt_encode_tensor: null value"
@require value.tag == TENSOR : "nn_ckpt_encode_tensor: value must be Tensor"
@require interp != null : "nn_ckpt_encode_tensor: null interp"
*>
fn Value* nn_ckpt_encode_tensor(Value* value, Interp* interp) {
    TensorVal* tensor = value.tensor_val;
    if (tensor == null || !tensor_has_valid_metadata(tensor)) return nn_ckpt_invalid(interp, "checkpoint.tensor", "valid Tensor", value);
    if (tensor.dtype != TENSOR_DTYPE_DOUBLE && tensor.dtype != TENSOR_DTYPE_FLOAT32) {
        return nn_ckpt_invalid(interp, "checkpoint.tensor.dtype", "Float64 or Float32 Tensor", value);
    }

    Value* cpu_value = value;
    if (tensor.device != TENSOR_DEVICE_CPU) {
        Value*[2] to_cpu_args = { value, make_symbol(interp, interp.symbols.intern("cpu")) };
        cpu_value = prim_to_device(to_cpu_args[0:2], null, interp);
        if (cpu_value == null || cpu_value.tag == ERROR) return cpu_value;
    }
    Value*[1] shape_args = { cpu_value };
    Value* shape = prim_shape(shape_args[0:1], null, interp);
    if (shape == null || shape.tag == ERROR) return shape;
    Value* data = tensor_to_flat_array_value(cpu_value, interp);
    if (data == null || data.tag == ERROR) return data;

    Value* obj = nn_ckpt_obj(interp, "tensor", 10);
    if (obj == null || obj.tag == ERROR) return obj;
    if (Value* err = nn_ckpt_set(obj, interp, "dtype", make_string(interp, tensor_dtype_name(tensor.dtype)))) return err;
    if (Value* err = nn_ckpt_set(obj, interp, "device", make_string(interp, interp.symbols.get_name(tensor_device_symbol(tensor.device, interp))))) return err;
    if (Value* err = nn_ckpt_set(obj, interp, "shape", nn_ckpt_encode_value(shape, interp))) return err;
    if (Value* err = nn_ckpt_set(obj, interp, "data", nn_ckpt_encode_value(data, interp))) return err;
    return obj;
}

<*
@require interp != null : "nn_ckpt_encode_value: null interp"
*>
fn Value* nn_ckpt_encode_value(Value* value, Interp* interp) {
    if (value == null || value.tag == NIL) return nn_ckpt_obj(interp, "nil", 2);
    Value* obj = null;
    switch (value.tag) {
        case INT:
            obj = nn_ckpt_obj(interp, "int", 4);
            if (obj == null || obj.tag == ERROR) return obj;
            if (Value* err = nn_ckpt_set(obj, interp, "value", value)) return err;
            return obj;
        case DOUBLE:
            obj = nn_ckpt_obj(interp, "float64", 4);
            if (obj == null || obj.tag == ERROR) return obj;
            if (Value* err = nn_ckpt_set(obj, interp, "value", value)) return err;
            return obj;
        case FLOAT32:
            obj = nn_ckpt_obj(interp, "float32", 4);
            if (obj == null || obj.tag == ERROR) return obj;
            if (Value* err = nn_ckpt_set(obj, interp, "value", value)) return err;
            return obj;
        case STRING:
            obj = nn_ckpt_obj(interp, "string", 4);
            if (obj == null || obj.tag == ERROR) return obj;
            if (Value* err = nn_ckpt_set(obj, interp, "value", value)) return err;
            return obj;
        case SYMBOL:
            obj = nn_ckpt_obj(interp, "symbol", 4);
            if (obj == null || obj.tag == ERROR) return obj;
            if (Value* err = nn_ckpt_set(obj, interp, "name", make_string(interp, interp.symbols.get_name(value.sym_val)))) return err;
            return obj;
        case ARRAY:
            if (value.array_val == null) return nn_ckpt_invalid(interp, "checkpoint.array", "valid Array", value);
            return nn_ckpt_encode_array(value, interp);
        case CONS:
            return nn_ckpt_encode_list(value, interp);
        case HASHMAP:
            if (value.hashmap_val == null) return nn_ckpt_invalid(interp, "checkpoint.dict", "valid Dictionary", value);
            return nn_ckpt_encode_dict(value, interp);
        case TENSOR:
            return nn_ckpt_encode_tensor(value, interp);
        case NIL:
            return nn_ckpt_obj(interp, "nil", 2);
        case VOID:
        case BIG_INTEGER:
        case BIG_FLOAT:
        case BIG_COMPLEX:
        case COMPLEX128:
        case COMPLEX64:
        case CLOSURE:
        case CONTINUATION:
        case PRIMITIVE:
        case PARTIAL_PRIM:
        case ERROR:
        case SET:
        case FFI_HANDLE:
        case TYPE_INFO:
        case INSTANCE:
        case METHOD_TABLE:
        case MODULE:
        case ITERATOR:
        case COROUTINE:
        case TIME_POINT:
            return nn_ckpt_invalid(interp, "checkpoint.value", "serializable DataSpec value", value);
    }
}

fn bool nn_ckpt_number_to_double(Value* value, double* out) {
    if (value == null || out == null) return false;
    if (value.tag == INT) {
        *out = (double)value.int_val;
        return true;
    }
    if (value.tag == DOUBLE) {
        *out = value.double_val;
        return true;
    }
    if (value.tag == FLOAT32) {
        *out = (double)value.float32_val;
        return true;
    }
    return false;
}

<*
@require interp != null : "nn_ckpt_decode_items_array: null interp"
*>
fn Value* nn_ckpt_decode_items_array(Value* encoded, Interp* interp, char[] field_name) {
    Value* items = nn_ckpt_string_field(encoded, interp, field_name);
    if (items == null || items.tag != ARRAY || items.array_val == null) {
        return nn_ckpt_invalid(interp, "checkpoint.items", "encoded Array", items);
    }
    Value* out = make_array_checked(interp, items.array_val.length, "nn/checkpoint: out of memory");
    if (out == null || out.tag == ERROR) return out;
    if (out.tag != ARRAY || out.array_val == null) return nn_oom(interp);
    foreach (item : items.array_val.items[0:items.array_val.length]) {
        Value* decoded = nn_ckpt_decode_value(item, interp);
        if (decoded == null || decoded.tag == ERROR) return decoded;
        out.array_val.items[out.array_val.length++] = decoded;
    }
    return out;
}

<*
@require interp != null : "nn_ckpt_decode_dict: null interp"
*>
fn Value* nn_ckpt_decode_dict(Value* encoded, Interp* interp) {
    Value* entries = nn_ckpt_string_field(encoded, interp, "entries");
    if (entries == null || entries.tag != ARRAY || entries.array_val == null) {
        return nn_ckpt_invalid(interp, "checkpoint.entries", "encoded dictionary entries", entries);
    }
    if (entries.array_val.length > ((usz)uint.max - (usz)4) / (usz)2) {
        return nn_ckpt_invalid(interp, "checkpoint.entries", "dictionary entry count within capacity limit", entries);
    }
    Value* out = nn_make_dict(interp, (uint)(entries.array_val.length * 2 + 4));
    if (out == null || out.tag == ERROR) return out;
    foreach (item : entries.array_val.items[0:entries.array_val.length]) {
        Value* pair = item;
        if (pair == null || pair.tag != ARRAY || pair.array_val == null || pair.array_val.length != 2) {
            return nn_ckpt_invalid(interp, "checkpoint.entries", "key/value pair", pair);
        }
        Value* key = nn_ckpt_decode_value(pair.array_val.items[0], interp);
        if (key == null || key.tag == ERROR) return key;
        Value* value = nn_ckpt_decode_value(pair.array_val.items[1], interp);
        if (value == null || value.tag == ERROR) return value;
        if (!hashmap_set_checked(out.hashmap_val, key, value, interp)) return nn_oom(interp);
}
    return out;
}

<*
@require interp != null : "nn_ckpt_decode_list: null interp"
*>
fn Value* nn_ckpt_decode_list(Value* encoded, Interp* interp) {
    Value* items = nn_ckpt_decode_items_array(encoded, interp, "items");
    if (items == null || items.tag == ERROR) return items;
    Value* list = make_nil(interp);
    for (usz i = items.array_val.length; i > 0; i--) {
        list = make_cons(interp, items.array_val.items[i - 1], list);
        if (list == null || list.tag == ERROR) return list;
    }
    return list;
}

<*
@require interp != null : "nn_ckpt_decode_tensor: null interp"
*>
fn Value* nn_ckpt_decode_tensor(Value* encoded, Interp* interp) {
    Value* dtype = nn_ckpt_string_field(encoded, interp, "dtype");
    Value* device = nn_ckpt_string_field(encoded, interp, "device");
    if (!nn_ckpt_string_is(dtype, "Float64") && !nn_ckpt_string_is(dtype, "Float32")) {
        return nn_ckpt_invalid(interp, "checkpoint.tensor.dtype", "Float64 or Float32", dtype);
    }
    if (!nn_ckpt_string_is(device, "cpu") && !nn_ckpt_string_is(device, "cuda") && !nn_ckpt_string_is(device, "vulkan")) {
        return nn_ckpt_invalid(interp, "checkpoint.tensor.device", "cpu, cuda, or vulkan", device);
    }
    Value* shape = nn_ckpt_decode_value(nn_ckpt_string_field(encoded, interp, "shape"), interp);
    if (shape == null || shape.tag == ERROR) return shape;
    Value* data = nn_ckpt_decode_value(nn_ckpt_string_field(encoded, interp, "data"), interp);
    if (data == null || data.tag == ERROR) return data;
    SymbolId dtype_sym = interp.symbols.intern(dtype.str_chars[:dtype.str_len]);
    if (dtype_sym == INVALID_SYMBOL_ID) return nn_oom(interp);
    Value*[3] tensor_args = { make_symbol(interp, dtype_sym), shape, data };
    Value* tensor = prim_tensor(tensor_args[0:3], null, interp);
    if (tensor == null || tensor.tag == ERROR) return tensor;
    if (nn_ckpt_string_is(device, "cpu")) return tensor;
    SymbolId device_sym = interp.symbols.intern(device.str_chars[:device.str_len]);
    if (device_sym == INVALID_SYMBOL_ID) return nn_oom(interp);
    Value*[2] to_device_args = { tensor, make_symbol(interp, device_sym) };
    return prim_to_device(to_device_args[0:2], null, interp);
}

<*
@require interp != null : "nn_ckpt_decode_value: null interp"
*>
fn Value* nn_ckpt_decode_value(Value* encoded, Interp* interp) {
    if (encoded == null || encoded.tag != HASHMAP || encoded.hashmap_val == null) {
        return nn_ckpt_invalid(interp, "checkpoint.value", "encoded checkpoint object", encoded);
    }
    Value* tag = nn_ckpt_string_field(encoded, interp, "tag");
    if (tag == null || tag.tag != STRING) return nn_ckpt_invalid(interp, "checkpoint.tag", "string tag", tag);
    if (nn_ckpt_string_is(tag, "nil")) return make_nil(interp);
    if (nn_ckpt_string_is(tag, "int")) {
        Value* value = nn_ckpt_string_field(encoded, interp, "value");
        return value != null && value.tag == INT ? value : nn_ckpt_invalid(interp, "checkpoint.int", "integer value", value);
    }
    if (nn_ckpt_string_is(tag, "float64")) {
        double number = 0.0;
        Value* value = nn_ckpt_string_field(encoded, interp, "value");
        if (!nn_ckpt_number_to_double(value, &number)) return nn_ckpt_invalid(interp, "checkpoint.float64", "number value", value);
        return make_double(interp, number);
    }
    if (nn_ckpt_string_is(tag, "float32")) {
        double number = 0.0;
        float narrowed = 0.0f;
        Value* value = nn_ckpt_string_field(encoded, interp, "value");
        if (!nn_ckpt_number_to_double(value, &number)) return nn_ckpt_invalid(interp, "checkpoint.float32", "number value", value);
        if (!conversion_try_double_to_float32(number, &narrowed)) return nn_ckpt_invalid(interp, "checkpoint.float32", "number value representable as Float32", value);
        return make_float32(interp, narrowed);
    }
    if (nn_ckpt_string_is(tag, "string")) {
        Value* value = nn_ckpt_string_field(encoded, interp, "value");
        return value != null && value.tag == STRING ? value : nn_ckpt_invalid(interp, "checkpoint.string", "string value", value);
    }
    if (nn_ckpt_string_is(tag, "symbol")) {
        Value* name = nn_ckpt_string_field(encoded, interp, "name");
        if (name == null || name.tag != STRING) return nn_ckpt_invalid(interp, "checkpoint.symbol", "symbol name", name);
        SymbolId sym = interp.symbols.intern(name.str_chars[:name.str_len]);
        if (sym == INVALID_SYMBOL_ID) return nn_oom(interp);
        return make_symbol(interp, sym);
    }
    if (nn_ckpt_string_is(tag, "array")) return nn_ckpt_decode_items_array(encoded, interp, "items");
    if (nn_ckpt_string_is(tag, "list")) return nn_ckpt_decode_list(encoded, interp);
    if (nn_ckpt_string_is(tag, "dict")) return nn_ckpt_decode_dict(encoded, interp);
    if (nn_ckpt_string_is(tag, "tensor")) return nn_ckpt_decode_tensor(encoded, interp);
    return nn_ckpt_invalid(interp, "checkpoint.tag", "known checkpoint tag", tag);
}

<*
@require interp != null : "nn_ckpt_wrap: null interp"
*>
fn Value* nn_ckpt_wrap(Value* payload, Interp* interp, char[] kind) {
    Value* encoded = nn_ckpt_encode_value(payload, interp);
    if (encoded == null || encoded.tag == ERROR) return encoded;
    Value* root = nn_make_dict(interp, 8);
    if (root == null || root.tag == ERROR) return root;
    if (Value* err = nn_ckpt_set(root, interp, "tag", make_string(interp, NN_CKPT_TAG))) return err;
    if (Value* err = nn_ckpt_set(root, interp, "version", make_int(interp, NN_CKPT_VERSION))) return err;
    if (Value* err = nn_ckpt_set(root, interp, "payload-kind", make_string(interp, kind))) return err;
    if (Value* err = nn_ckpt_set(root, interp, "payload", encoded)) return err;
    Value*[1] emit_args = { root };
    return prim_json_emit_pretty(emit_args[0:1], null, interp);
}

fn bool nn_ckpt_source_is_json(Value* source) {
    if (source == null || source.tag != STRING) return false;
    foreach (c : source.str_chars[0:source.str_len]) {
        if (c == ' ' || c == '\n' || c == '\r' || c == '\t') continue;
        return c == '{';
    }
    return false;
}

<*
@require interp != null : "nn_ckpt_load_source: null interp"
*>
fn Value* nn_ckpt_load_source(Value* source, Interp* interp, char[] prim_name, char[] read_error_message) {
    if (source == null || source.tag != STRING) return primitive_arg_type_raise(interp, prim_name, 0, "JSON checkpoint string or path", source);
    if (nn_ckpt_source_is_json(source)) return source;
    char[] path = source.str_chars[:source.str_len];
    if (try content = io::file::load_temp((String)path)) return make_string(interp, content);
    return nn_ckpt_io_error(interp, read_error_message);
}

<*
@require interp != null : "nn_ckpt_parse_root: null interp"
*>
fn Value* nn_ckpt_parse_root(Value* source, Interp* interp, char[] expected_kind, char[] prim_name, char[] read_error_message) {
    Value* text = nn_ckpt_load_source(source, interp, prim_name, read_error_message);
    if (text == null || text.tag == ERROR) return text;
    Value*[1] parse_args = { text };
    Value* root = prim_json_parse(parse_args[0:1], null, interp);
    if (root == null || root.tag == ERROR) return nn_ckpt_invalid(interp, "checkpoint", "valid checkpoint JSON", root);
    if (root.tag != HASHMAP || root.hashmap_val == null) return nn_ckpt_invalid(interp, "checkpoint", "checkpoint object", root);
    if (!nn_ckpt_string_is(nn_ckpt_string_field(root, interp, "tag"), NN_CKPT_TAG)) {
        return nn_ckpt_invalid(interp, "checkpoint.tag", NN_CKPT_TAG, nn_ckpt_string_field(root, interp, "tag"));
    }
    Value* version = nn_ckpt_string_field(root, interp, "version");
    if (version == null || version.tag != INT || version.int_val != NN_CKPT_VERSION) {
        return nn_ckpt_invalid(interp, "checkpoint.version", "supported checkpoint version", version);
    }
    Value* payload_kind = nn_ckpt_string_field(root, interp, "payload-kind");
    if (!nn_ckpt_string_is(payload_kind, expected_kind)) {
        return nn_ckpt_invalid(interp, "checkpoint.payload-kind", expected_kind, payload_kind);
    }
    return nn_ckpt_string_field(root, interp, "payload");
}

<*
@require interp != null : "nn_ckpt_save_text_or_path: null interp"
*>
fn Value* nn_ckpt_save_text_or_path(Value* json, Value*[] args, Interp* interp, char[] prim_name) {
    if (json == null || json.tag == ERROR) return json;
    if (args.len == 1) return json;
    if (args.len != 2) return primitive_arity_raise(interp, prim_name, "1 or 2 arguments: value [path]", args.len);
    if (args[1] == null || args[1].tag != STRING) return primitive_arg_type_raise(interp, prim_name, 1, "path string", args[1]);
    int fd = fs_uv_open(args[1].str_chars, 577, 420);
    if (fd < 0) return nn_ckpt_io_error(interp, "checkpoint: failed to open checkpoint path");
    usz written = 0;
    bool ok = true;
    while (written < json.str_len) {
        long n = fs_uv_write(fd, json.str_chars + written, json.str_len - written);
        if (n <= 0) {
            ok = false;
            break;
        }
        written += (usz)n;
    }
    int close_status = fs_uv_close(fd);
    if (!ok || close_status < 0) return nn_ckpt_io_error(interp, "checkpoint: failed to write checkpoint path");
    return make_void(interp);
}

<*
@require interp != null
*>
fn Value* prim_nn_save_spec(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 2) return primitive_arity_raise(interp, "nn/save-spec", "1 or 2 arguments: spec [path]", args.len);
    if (Value* err = nn_validate_dataspec(args[0], interp)) return err;
    if (nn_kind_is(args[0], interp, "model")) return nn_ckpt_invalid(interp, "spec.kind", "non-model DataSpec", nn_dict_get(args[0], interp, "kind"));
    Value* json = nn_ckpt_wrap(args[0], interp, "spec");
    return nn_ckpt_save_text_or_path(json, args, interp, "nn/save-spec");
}

<*
@require interp != null
*>
fn Value* prim_nn_load_spec(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "nn/load-spec", "1 argument: JSON checkpoint string or path", args.len);
    Value* payload = nn_ckpt_parse_root(args[0], interp, "spec", "nn/load-spec", "nn/load-spec: failed to read checkpoint path");
    if (payload == null || payload.tag == ERROR) return payload;
    Value* spec = nn_ckpt_decode_value(payload, interp);
    if (spec == null || spec.tag == ERROR) return spec;
    if (Value* err = nn_validate_dataspec(spec, interp)) return err;
    if (nn_kind_is(spec, interp, "model")) return nn_ckpt_invalid(interp, "spec.kind", "non-model DataSpec", nn_dict_get(spec, interp, "kind"));
    return spec;
}

<*
@require interp != null
*>
fn Value* prim_nn_save(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 2) return primitive_arity_raise(interp, "nn/save", "1 or 2 arguments: model [path]", args.len);
    if (Value* err = nn_require_model(args[0], interp)) return err;
    Value* json = nn_ckpt_wrap(args[0], interp, "model");
    return nn_ckpt_save_text_or_path(json, args, interp, "nn/save");
}

<*
@require interp != null
*>
fn Value* prim_nn_load(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "nn/load", "1 argument: JSON checkpoint string or path", args.len);
    Value* payload = nn_ckpt_parse_root(args[0], interp, "model", "nn/load", "nn/load: failed to read checkpoint path");
    if (payload == null || payload.tag == ERROR) return payload;
    Value* model = nn_ckpt_decode_value(payload, interp);
    if (model == null || model.tag == ERROR) return model;
    if (Value* err = nn_require_model(model, interp)) return err;
    return model;
}
