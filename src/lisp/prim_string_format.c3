module lisp;

import main;

// =============================================================================
// SECTION: STRING FORMATTING + CL-FORMAT
// =============================================================================
fn Value* prim_format(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != STRING) return raise_error(interp, "format: expected format string");
    char[] fmt = args[0].str_chars[:args[0].str_len];
    StringVal* builder = strval_new(fmt.len);
    usz arg_idx = 1;

    for (usz i = 0; i < fmt.len; i++) {
        if (fmt[i] == '%' && i + 1 < fmt.len) {
            i++; // skip '%'
            Value* err = format_directive(builder, fmt, &i, args, &arg_idx, interp);
            if (err == SPECIAL_BREAK) break;
            if (err == SPECIAL_CONTINUE) continue;
            if (err != null) return err;
        } else {
            strval_push(builder, fmt[i]);
        }
    }
    builder.chars[builder.len] = 0;
    return strval_materialize_result(builder, interp, "format: failed to allocate result value");
}

fn Value* prim_cl_format(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args[0].tag != STRING) return raise_error(interp, "cl-format: expected format string");
    char[] fmt = args[0].str_chars[:args[0].str_len];
    StringVal* builder = strval_new(fmt.len);
    usz arg_idx = 1;
    for (usz i = 0; i < fmt.len; i++) {
        if (fmt[i] == '~' && i + 1 < fmt.len) {
            i++;
            cl_format_directive(builder, fmt[i], args, &arg_idx, interp);
        } else {
            strval_push(builder, fmt[i]);
        }
    }
    builder.chars[builder.len] = 0;
    return strval_materialize_result(builder, interp, "cl-format: failed to allocate result value");
}

