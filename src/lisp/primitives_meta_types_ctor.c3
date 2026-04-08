module lisp;

import std::core::mem;
import std::io;
import main;

/**
 * (^ type-form value) -> value
 * Explicit constructor annotation checker for forms like:
 *   (^(Box Integer) (Box 42))
 */
fn Value* prim_ctor_type_apply(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) {
        return ctor_type_raise(interp, "type/ctor-arity-mismatch", "^: expected type form and value", null);
    }

    CtorTypeForm expected = {};
    if (!ctor_form_parse(args[0], &expected)) {
        return ctor_type_raise(interp, "type/ctor-type-arg-mismatch",
            "^: expected constructor type form (symbol or list)", null);
    }
    defer ctor_form_destroy(&expected);

    Value* value = args[1];
    if (value == null || value.tag != INSTANCE || value.instance_val == null) {
        Value* expected_args = ctor_expected_args_to_list(expected.args, expected.arg_count, interp);
        Value* actual_args = make_nil(interp);
        Value* data = ctor_mismatch_data(interp, expected.ctor, expected_args, actual_args, false, 0, null);
        char[256] buf;
        char[] msg = io::bprintf(&buf, "^: expected instance of %s",
            (ZString)interp.symbols.get_name(expected.ctor))!!;
        return ctor_type_raise(interp, "type/ctor-type-arg-mismatch", msg, data);
    }

    Instance* inst = value.instance_val;
    TypeInfo* actual_ti = interp.types.get(inst.type_id);
    if (actual_ti == null) {
        return ctor_type_raise(interp,
            "type/ctor-missing-instance-type-info",
            "internal error: missing instance type info",
            null);
    }

    Value* expected_args = ctor_expected_args_to_list(expected.args, expected.arg_count, interp);
    Value* actual_args = ctor_type_args_to_symbol_list(inst.type_args, inst.type_arg_count, interp);

    if ((uint)actual_ti.name != (uint)expected.ctor) {
        Value* data = ctor_mismatch_data(interp, expected.ctor, expected_args, actual_args, false, 0, null);
        char[256] buf;
        char[] msg = io::bprintf(&buf, "^: constructor mismatch, expected %s got %s",
            (ZString)interp.symbols.get_name(expected.ctor),
            (ZString)interp.symbols.get_name(actual_ti.name))!!;
        return ctor_type_raise(interp, "type/ctor-type-arg-mismatch", msg, data);
    }

    if (expected.arg_count != inst.type_arg_count) {
        Value* data = ctor_mismatch_data(interp, expected.ctor, expected_args, actual_args, false, 0, null);
        char[256] buf;
        char[] msg = io::bprintf(&buf, "^: %s expected %d type arg(s), got %d",
            (ZString)interp.symbols.get_name(expected.ctor),
            (int)expected.arg_count,
            (int)inst.type_arg_count)!!;
        return ctor_type_raise(interp, "type/ctor-arity-mismatch", msg, data);
    }

    CtorArgMismatchPath mismatch = {};
    for (usz i = 0; i < expected.arg_count; i++) {
        Value* runtime_field = ctor_type_param_runtime_value(inst, actual_ti, i);
        if (!ctor_match_type_form(expected.args[i], inst.type_args[i], runtime_field, interp, &mismatch, 0)) {
            ctor_path_prepend(&mismatch, i);
            usz arg_index = mismatch.len > 0 ? mismatch.items[0] : i;
            Value* data = ctor_mismatch_data(interp, expected.ctor, expected_args, actual_args, true, arg_index, &mismatch);
            char[256] buf;
            char[] msg = io::bprintf(&buf, "^: %s type arg mismatch at index %d",
                (ZString)interp.symbols.get_name(expected.ctor), (int)arg_index)!!;
            return ctor_type_raise(interp, "type/ctor-type-arg-mismatch", msg, data);
        }
    }

    return value;
}
