module lisp;


fn Value* parse_json_emit_option_entry(Value* key, Value* value, JsonEmitOptions* opts, Interp* interp) {
    if (!is_symbol(key)) {
        return data_format_raise(interp, "type/arg-mismatch", "json-emit: options must be key/value pairs with symbol keys");
    }

    char[] key_name = interp.symbols.get_name(key.sym_val);

    if (string_equals(key_name, "pretty")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "pretty", "json-emit", interp);
        if (err != null) return err;
        opts.pretty = parsed;
        opts.has_pretty_override = true;
        return null;
    }
    if (string_equals(key_name, "pretty-two-spaces")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "pretty-two-spaces", "json-emit", interp);
        if (err != null) return err;
        if (parsed) {
            opts.flags |= JSON_WRITE_OPTION_PRETTY_TWO_SPACES;
            opts.flags &= ~JSON_WRITE_OPTION_PRETTY;
            return null;
        }
        opts.flags &= ~JSON_WRITE_OPTION_PRETTY_TWO_SPACES;
        return null;
    }
    if (string_equals(key_name, "escape-unicode")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "escape-unicode", "json-emit", interp);
        if (err != null) return err;
        if (parsed) {
            opts.flags |= JSON_WRITE_OPTION_ESCAPE_UNICODE;
        } else {
            opts.flags &= ~JSON_WRITE_OPTION_ESCAPE_UNICODE;
        }
        return null;
    }
    if (string_equals(key_name, "escape-slashes")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "escape-slashes", "json-emit", interp);
        if (err != null) return err;
        if (parsed) {
            opts.flags |= JSON_WRITE_OPTION_ESCAPE_SLASHES;
        } else {
            opts.flags &= ~JSON_WRITE_OPTION_ESCAPE_SLASHES;
        }
        return null;
    }
    if (string_equals(key_name, "allow-invalid-unicode")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "allow-invalid-unicode", "json-emit", interp);
        if (err != null) return err;
        if (parsed) {
            opts.flags |= JSON_WRITE_OPTION_ALLOW_INVALID_UNICODE;
        } else {
            opts.flags &= ~JSON_WRITE_OPTION_ALLOW_INVALID_UNICODE;
        }
        return null;
    }
    if (string_equals(key_name, "allow-inf-and-nan")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "allow-inf-and-nan", "json-emit", interp);
        if (err != null) return err;
        if (parsed) {
            opts.flags |= JSON_WRITE_OPTION_ALLOW_INF_AND_NAN;
            if ((opts.flags & JSON_WRITE_OPTION_INF_AND_NAN_AS_NULL) != 0) {
                opts.flags &= ~JSON_WRITE_OPTION_INF_AND_NAN_AS_NULL;
            }
        } else {
            opts.flags &= ~JSON_WRITE_OPTION_ALLOW_INF_AND_NAN;
        }
        return null;
    }
    if (string_equals(key_name, "inf-and-nan-as-null")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "inf-and-nan-as-null", "json-emit", interp);
        if (err != null) return err;
        if (parsed) {
            opts.flags |= JSON_WRITE_OPTION_INF_AND_NAN_AS_NULL;
            if ((opts.flags & JSON_WRITE_OPTION_ALLOW_INF_AND_NAN) != 0) {
                opts.flags &= ~JSON_WRITE_OPTION_ALLOW_INF_AND_NAN;
            }
        } else {
            opts.flags &= ~JSON_WRITE_OPTION_INF_AND_NAN_AS_NULL;
        }
        return null;
    }
    if (string_equals(key_name, "newline-at-end")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "newline-at-end", "json-emit", interp);
        if (err != null) return err;
        if (parsed) {
            opts.flags |= JSON_WRITE_OPTION_NEWLINE_AT_END;
        } else {
            opts.flags &= ~JSON_WRITE_OPTION_NEWLINE_AT_END;
        }
        return null;
    }
    if (string_equals(key_name, "fp-to-float")) {
        bool parsed = false;
        Value* err = parse_bool_option_value(value, &parsed, "fp-to-float", "json-emit", interp);
        if (err != null) return err;
        if (parsed) {
            opts.flags |= (usz)(1 << (32 - 5));
        } else {
            opts.flags &= ~((usz)(1 << (32 - 5)));
        }
        return null;
    }
    if (string_equals(key_name, "precision") || string_equals(key_name, "fp-precision")) {
        if (value == null || value.tag != INT) {
            return data_format_raise(interp, "type/arg-mismatch",
    "json-emit: precision must be an integer between 0 and 15");
        }
        if (value.int_val < 0 || value.int_val > 15) {
            return data_format_raise(interp, "type/arg-mismatch",
    "json-emit: precision must be an integer between 0 and 15");
        }
        if (value.int_val == 0) {
            opts.has_precision = false;
            return null;
        }
        opts.has_precision = true;
        opts.precision = (int)value.int_val;
        return null;
    }

    return data_format_raise(interp, "type/arg-mismatch",
    "json-emit: unknown option key (supported: pretty, pretty-two-spaces, escape-unicode, escape-slashes, allow-invalid-unicode, allow-inf-and-nan, inf-and-nan-as-null, newline-at-end, fp-to-float, precision)");
}

fn Value* parse_json_emit_options(Value* options, JsonEmitOptions* opts, Interp* interp) {
    if (options == null || options.tag == NIL) return null;
    if (!is_cons(options)) {
        return data_format_raise(interp, "type/arg-mismatch",
    "json-emit: expected second argument to be an option list");
    }

    Value* cur = options;
    while (is_cons(cur)) {
        Value* entry = car(cur);
        Value* next = cdr(cur);

        if (is_cons(entry)) {
            Value* key = car(entry);
            Value* rest = cdr(entry);
            if (!is_cons(rest) || !is_nil(cdr(rest))) {
                return data_format_raise(interp, "type/arg-mismatch",
    "json-emit: options list must be proper key/value pairs");
            }
            Value* value = car(rest);
            Value* err = parse_json_emit_option_entry(key, value, opts, interp);
            if (err != null) return err;
        } else {
            if (!is_cons(next)) {
                return data_format_raise(interp, "type/arg-mismatch",
    "json-emit: options list must be proper key/value pairs");
            }
            Value* key = entry;
            Value* value = car(next);
            Value* err = parse_json_emit_option_entry(key, value, opts, interp);
            if (err != null) return err;
            next = cdr(next);
        }

        cur = next;
    }
    if (!is_nil(cur)) {
        return data_format_raise(interp, "type/arg-mismatch",
    "json-emit: options list must be a proper list");
    }

    return null;
}

fn Value* parse_json_options(Value* options, usz* flags_out, Interp* interp) {
    if (options == null || options.tag == NIL) return null;
    if (!is_cons(options)) {
        return data_format_raise(interp, "type/arg-mismatch",
    "json-parse: expected second argument to be a list of option symbols");
    }

    usz flags = 0;
    Value* cur = options;
    while (is_cons(cur)) {
        Value* opt = car(cur);
        if (!is_symbol(opt)) {
            return data_format_raise(interp, "type/arg-mismatch",
                "json-parse: options list must contain only symbols");
        }
        usz option_flag = json_parse_option_flag(interp.symbols.get_name(opt.sym_val));
        if (option_flag == 0) {
            return data_format_raise(interp, "type/arg-mismatch",
                "json-parse: unknown option symbol (supported: allow-comments, allow-trailing-commas, allow-nan-inf)");
        }
        flags |= option_flag;
        cur = cdr(cur);
    }
    if (!is_nil(cur)) {
        return data_format_raise(interp, "type/arg-mismatch",
            "json-parse: options list must be a proper list");
    }

    *flags_out = flags;
    return null;
}
