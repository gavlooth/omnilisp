module lisp;


struct TomlParseOptions {
    bool check_utf8;
}

fn bool toml_is_symbol_name(Value* v, char[] name, Interp* interp) {
    return csv_is_symbol_name(v, name, interp);
}

fn Value* parse_toml_option_entry(Value* key, Value* value, TomlParseOptions* opts, Interp* interp) {
    if (toml_is_symbol_name(key, "check-utf8", interp)) {
        if (value == null || is_nil(value) || is_symbol(value)) {
            bool parsed;
            if (value == null || is_nil(value)) {
                parsed = false;
            } else if (value.sym_val == interp.sym_true) {
                parsed = true;
            } else if (value.sym_val == interp.sym_false) {
                parsed = false;
            } else {
                return data_format_raise(interp, "type/arg-mismatch", "toml-parse: check-utf8 must be a boolean (true/false)");
            }

            opts.check_utf8 = parsed;
            return null;
        }

        return data_format_raise(interp, "type/arg-mismatch", "toml-parse: check-utf8 must be a boolean (true/false)");
    }

    return data_format_raise(interp, "type/arg-mismatch", "toml-parse: unknown option key (supported: check-utf8)");
}

fn Value* parse_toml_parse_options(Value* options, TomlParseOptions* opts, Interp* interp) {
    if (options == null || is_nil(options)) return null;
    if (!is_cons(options)) {
        return data_format_raise(interp, "type/arg-mismatch", "toml-parse: options must be a proper list");
    }

    Value* cur = options;
    while (is_cons(cur)) {
        Value* entry = car(cur);
        Value* next = cdr(cur);

        if (is_cons(entry)) {
            Value* key = car(entry);
            Value* rest = cdr(entry);
            if (!is_cons(rest) || !is_nil(cdr(rest))) {
                return data_format_raise(interp, "type/arg-mismatch", "toml-parse: options must be proper key/value pairs");
            }
            Value* value = car(rest);
            Value* err = parse_toml_option_entry(key, value, opts, interp);
            if (err != null) return err;
        } else {
            if (!is_cons(next)) {
                return data_format_raise(interp, "type/arg-mismatch", "toml-parse: options must be proper key/value pairs");
            }
            Value* key = entry;
            Value* value = car(next);
            Value* err = parse_toml_option_entry(key, value, opts, interp);
            if (err != null) return err;
            next = cdr(next);
        }

        cur = next;
    }
    if (!is_nil(cur)) {
        return data_format_raise(interp, "type/arg-mismatch", "toml-parse: options must be a proper list");
    }

    return null;
}
