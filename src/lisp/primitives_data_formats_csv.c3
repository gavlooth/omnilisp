module lisp;



import std::core::mem;
import std::io;
import main;

fn Value* parse_csv_parse_options(Value* options, CsvParseOptions* opts, Interp* interp) {
    if (options == null || is_nil(options)) return null;
    if (is_string(options)) {
        return parse_csv_char_option(options, &opts.delimiter, "delimiter", interp, "csv-parse");
    }
    if (!is_cons(options)) {
        return data_format_raise(interp, "type/arg-mismatch", "csv-parse: options must be either delimiter string or option list");
    }

    Value* cur = options;
    while (is_cons(cur)) {
        Value* entry = car(cur);
        Value* next = cdr(cur);
        if (is_cons(entry)) {
            Value* key = car(entry);
            Value* rest = cdr(entry);
            if (!is_cons(rest) || !is_nil(cdr(rest))) {
                return data_format_raise(interp, "type/arg-mismatch", "csv-parse: options list must be proper key/value pairs");
            }
            Value* value = car(rest);
            Value* err = parse_csv_parse_option_entry(key, value, opts, interp);
            if (err != null) return err;
        } else {
            if (!is_cons(next)) {
                return data_format_raise(interp, "type/arg-mismatch", "csv-parse: options list must be proper key/value pairs");
            }
            Value* key = entry;
            Value* value = car(next);
            Value* err = parse_csv_parse_option_entry(key, value, opts, interp);
            if (err != null) return err;
            next = cdr(next);
        }

        cur = next;
    }
    if (!is_nil(cur)) {
        return data_format_raise(interp, "type/arg-mismatch", "csv-parse: options list must be a proper list");
    }

    return null;
}

fn Value* parse_csv_emit_options(Value* options, CsvEmitOptions* opts, Interp* interp) {
    if (options == null || is_nil(options)) return null;
    if (is_string(options)) {
        return parse_csv_char_option(options, &opts.delimiter, "delimiter", interp, "csv-emit");
    }
    if (!is_cons(options)) {
        return data_format_raise(interp, "type/arg-mismatch", "csv-emit: options must be either delimiter string or option list");
    }

    Value* cur = options;
    while (is_cons(cur)) {
        Value* entry = car(cur);
        Value* next = cdr(cur);
        if (is_cons(entry)) {
            Value* key = car(entry);
            Value* rest = cdr(entry);
            if (!is_cons(rest) || !is_nil(cdr(rest))) {
                return data_format_raise(interp, "type/arg-mismatch", "csv-emit: options list must be proper key/value pairs");
            }
            Value* value = car(rest);
            Value* err = parse_csv_emit_option_entry(key, value, opts, interp);
            if (err != null) return err;
        } else {
            if (!is_cons(next)) {
                return data_format_raise(interp, "type/arg-mismatch", "csv-emit: options list must be proper key/value pairs");
            }
            Value* key = entry;
            Value* value = car(next);
            Value* err = parse_csv_emit_option_entry(key, value, opts, interp);
            if (err != null) return err;
            next = cdr(next);
        }

        cur = next;
    }
    if (!is_nil(cur)) {
        return data_format_raise(interp, "type/arg-mismatch", "csv-emit: options list must be a proper list");
    }

    return null;
}
