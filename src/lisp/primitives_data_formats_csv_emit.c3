module lisp;



import std::core::mem;
import std::io;
import main;

fn bool csv_slice_matches(char[] text, usz start, char[] needle) {
    if (needle.len == 0 || start > text.len || needle.len > text.len - start) return false;
    for (usz i = 0; i < needle.len; i++) {
        if (text[start + i] != needle[i]) return false;
    }
    return true;
}

fn bool csv_string_needs_quotes(char[] text, char[] delimiter, char[] quote_char) {
    for (usz i = 0; i < text.len; i++) {
        char ch = text[i];
        if (ch == '\n' || ch == '\r') return true;
        if (csv_slice_matches(text, i, delimiter) || csv_slice_matches(text, i, quote_char)) return true;
    }
    return false;
}

fn void csv_emit_escaped_cell(
    DString* out,
    char[] text,
    char[] delimiter,
    char[] quote_char,
    int quote_style
) {
    bool needs_quote = csv_string_needs_quotes(text, delimiter, quote_char);
    bool quote_cell = false;

    switch (quote_style) {
    case CSV_QUOTE_NONE:
        out.append_string((String)text);
        return;

    case CSV_QUOTE_ALWAYS:
        quote_cell = true;
        break;

    case CSV_QUOTE_MINIMAL:
    default:
        quote_cell = needs_quote;
        break;
    }

    if (!quote_cell) {
        out.append_string((String)text);
        return;
    }

    out.append_string((String)quote_char);
    for (usz i = 0; i < text.len; i++) {
        if (csv_slice_matches(text, i, quote_char)) {
            out.append_string((String)quote_char);
            out.append_string((String)quote_char);
            i += quote_char.len - 1;
        } else {
            out.append_char(text[i]);
        }
    }
    out.append_string((String)quote_char);
}

fn Value* csv_emit_cell(DString* out, Value* cell, CsvEmitOptions* opts, Interp* interp) {
    if (cell == null || is_nil(cell)) {
        if (opts.strict && opts.quote_style == CSV_QUOTE_NONE && csv_string_needs_quotes(opts.nil_as, opts.delimiter, opts.quote_char)) {
            return data_format_raise(interp, "type/arg-mismatch", "csv-emit: strict quote-style none cannot emit ambiguous cell text");
        }
        csv_emit_escaped_cell(out, opts.nil_as, opts.delimiter, opts.quote_char, opts.quote_style);
        return null;
    }

    if (is_string(cell)) {
        char[] text = cell.str_chars[:cell.str_len];
        if (opts.strict && opts.quote_style == CSV_QUOTE_NONE && csv_string_needs_quotes(text, opts.delimiter, opts.quote_char)) {
            return data_format_raise(interp, "type/arg-mismatch", "csv-emit: strict quote-style none cannot emit ambiguous cell text");
        }
        csv_emit_escaped_cell(out, text, opts.delimiter, opts.quote_char, opts.quote_style);
        return null;
    }

    if (is_int(cell)) {
        char[32] nbuf;
        char[] text = int_to_string(cell.int_val, &nbuf);
        if (opts.strict && opts.quote_style == CSV_QUOTE_NONE && csv_string_needs_quotes(text, opts.delimiter, opts.quote_char)) {
            return data_format_raise(interp, "type/arg-mismatch", "csv-emit: strict quote-style none cannot emit ambiguous cell text");
        }
        csv_emit_escaped_cell(out, text, opts.delimiter, opts.quote_char, opts.quote_style);
        return null;
    }

    if (is_double(cell)) {
        char[64] dbuf;
        char[] text = double_to_string(cell.double_val, &dbuf);
        if (opts.strict && opts.quote_style == CSV_QUOTE_NONE && csv_string_needs_quotes(text, opts.delimiter, opts.quote_char)) {
            return data_format_raise(interp, "type/arg-mismatch", "csv-emit: strict quote-style none cannot emit ambiguous cell text");
        }
        csv_emit_escaped_cell(out, text, opts.delimiter, opts.quote_char, opts.quote_style);
        return null;
    }

    if (is_float32(cell)) {
        char[64] fbuf;
        char[] text = double_to_string((double)cell.float32_val, &fbuf);
        if (opts.strict && opts.quote_style == CSV_QUOTE_NONE && csv_string_needs_quotes(text, opts.delimiter, opts.quote_char)) {
            return data_format_raise(interp, "type/arg-mismatch", "csv-emit: strict quote-style none cannot emit ambiguous cell text");
        }
        csv_emit_escaped_cell(out, text, opts.delimiter, opts.quote_char, opts.quote_style);
        return null;
    }

    if (is_symbol(cell)) {
        char[] text = interp.symbols.get_name(cell.sym_val);
        if (opts.strict && opts.quote_style == CSV_QUOTE_NONE && csv_string_needs_quotes(text, opts.delimiter, opts.quote_char)) {
            return data_format_raise(interp, "type/arg-mismatch", "csv-emit: strict quote-style none cannot emit ambiguous cell text");
        }
        csv_emit_escaped_cell(out, text, opts.delimiter, opts.quote_char, opts.quote_style);
        return null;
    }

    return data_format_raise(interp, "type/arg-mismatch", "csv-emit: row cells must be string/int/double/symbol/nil");
}

fn Value* csv_emit_row_from_array(DString* out, Value* row, CsvEmitOptions* opts, Interp* interp) {
    for (usz i = 0; i < row.array_val.length; i++) {
        if (i > 0) out.append_string((String)opts.delimiter);
        if (Value* err = csv_emit_cell(out, row.array_val.items[i], opts, interp)) return err;
    }
    return null;
}

fn Value* csv_emit_row_from_list(DString* out, Value* row, CsvEmitOptions* opts, Interp* interp) {
    bool first = true;
    Value* curr = row;
    while (is_cons(curr)) {
        if (!first) out.append_string((String)opts.delimiter);
        first = false;

        if (Value* err = csv_emit_cell(out, car(curr), opts, interp)) return err;
        curr = cdr(curr);
    }

    if (!is_nil(curr)) {
        return data_format_raise(interp, "type/arg-mismatch", "csv-emit: expected each row to be an array or proper list");
    }
    return null;
}

fn Value* csv_emit_row(DString* out, Value* row, CsvEmitOptions* opts, Interp* interp) {
    if (row == null || is_nil(row)) return null;
    if (row.tag == ARRAY) return csv_emit_row_from_array(out, row, opts, interp);
    if (is_cons(row)) return csv_emit_row_from_list(out, row, opts, interp);
    return data_format_raise(interp, "type/arg-mismatch", "csv-emit: expected each row to be an array or list");
}

<*
@require interp != null
*>
fn Value* prim_csv_emit(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 2) {
        return data_format_raise(interp, "type/arity", "csv-emit: expected 1-2 arguments");
    }

    CsvEmitOptions opts;
    opts.delimiter = ",";
    opts.line_ending = "\r\n";
    opts.quote_char = "\"";
    opts.quote_style = CSV_QUOTE_MINIMAL;
    opts.nil_as = "";
    opts.strict = true;
    if (args.len == 2) {
        if (Value* err = parse_csv_emit_options(args[1], &opts, interp)) return err;
    }

    if (opts.strict && string_equals(opts.quote_char, opts.delimiter)) {
        return data_format_raise(interp, "type/arg-mismatch", "csv-emit: quote-char cannot equal delimiter");
    }

    Value* rows_v = args[0];
    if (rows_v == null || is_nil(rows_v)) return make_string(interp, "");

    DString out;
    out.init(mem);
    defer out.free();

    if (rows_v.tag == ARRAY) {
        for (usz i = 0; i < rows_v.array_val.length; i++) {
            if (i > 0) out.append_string((String)opts.line_ending);
            if (Value* err = csv_emit_row(&out, rows_v.array_val.items[i], &opts, interp)) return err;
        }
        return make_string(interp, out.str_view());
    }

    if (is_cons(rows_v)) {
        Value* curr = rows_v;
        bool first = true;
        while (is_cons(curr)) {
            if (!first) out.append_string((String)opts.line_ending);
            first = false;

            if (Value* err = csv_emit_row(&out, car(curr), &opts, interp)) return err;
            curr = cdr(curr);
        }
        if (!is_nil(curr)) {
            return data_format_raise(interp, "type/arg-mismatch", "csv-emit: expected rows to be an array or proper list");
        }
        return make_string(interp, out.str_view());
    }

    return data_format_raise(interp, "type/arg-mismatch", "csv-emit: expected rows to be an array or list");
}
