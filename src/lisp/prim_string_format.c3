module lisp;

import main;

// =============================================================================
// SECTION: STRING FORMATTING + CL-FORMAT
// =============================================================================
<*
@require interp != null
*>
fn Value* prim_format(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != STRING) return runtime_raise(interp, "type/arg-mismatch", "format: expected format string");
    char[] fmt = args[0].str_chars[:args[0].str_len];
    StringVal* builder = strval_new(fmt.len);
    if (builder == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "format: failed to allocate builder");
    }
    usz arg_idx = 1;

    for (usz i = 0; i < fmt.len; i++) {
        if (fmt[i] == '%' && i + 1 < fmt.len) {
            i++; // skip '%'
            Value* err = format_directive(builder, fmt, &i, args, &arg_idx, interp);
            if (err == SPECIAL_BREAK) break;
            if (err == SPECIAL_CONTINUE) continue;
            if (err != null) {
                strval_dispose(builder);
                return err;
            }
        } else {
            if (!strval_push(builder, fmt[i])) {
                return strval_raise_oom(builder, interp, "format: failed to grow result buffer");
            }
        }
    }
    return strval_materialize_result(builder, interp, "format: failed to allocate result value");
}

<*
@require interp != null
*>
fn Value* prim_cl_format(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != STRING) return runtime_raise(interp, "type/arg-mismatch", "cl-format: expected format string");
    char[] fmt = args[0].str_chars[:args[0].str_len];
    StringVal* builder = strval_new(fmt.len);
    if (builder == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/out-of-memory", "cl-format: failed to allocate builder");
    }
    usz arg_idx = 1;
    for (usz i = 0; i < fmt.len; i++) {
        if (fmt[i] == '~' && i + 1 < fmt.len) {
            i++;
            if (Value* err = cl_format_directive(builder, fmt[i], args, &arg_idx, interp)) {
                strval_dispose(builder);
                return err;
            }
        } else {
            if (!strval_push(builder, fmt[i])) {
                return strval_raise_oom(builder, interp, "cl-format: failed to grow result buffer");
            }
        }
    }
    return strval_materialize_result(builder, interp, "cl-format: failed to allocate result value");
}

