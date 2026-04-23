module lisp;



import std::core::mem;
import std::io;
import main;

fn bool csv_string_needs_quotes(char[] text, char delimiter, char quote_char) {
    for (usz i = 0; i < text.len; i++) {
        char ch = text[i];
        if (ch == delimiter || ch == quote_char || ch == '\n' || ch == '\r') return true;
    }
    return false;
}

fn void csv_emit_escaped_cell(
    DString* out,
    char[] text,
    char delimiter,
    char quote_char,
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

    out.append_char(quote_char);
    for (usz i = 0; i < text.len; i++) {
        if (text[i] == quote_char) {
            out.append_char(quote_char);
            out.append_char(quote_char);
        } else {
            out.append_char(text[i]);
        }
    }
    out.append_char(quote_char);
}

fn Value* csv_emit_cell(DString* out, Value* cell, CsvEmitOptions* opts, Interp* interp) {
    if (cell == null || is_nil(cell)) {
        csv_emit_escaped_cell(out, opts.nil_as, opts.delimiter, opts.quote_char, opts.quote_style);
        return null;
    }

    if (is_string(cell)) {
        csv_emit_escaped_cell(out, cell.str_chars[:cell.str_len], opts.delimiter, opts.quote_char, opts.quote_style);
        return null;
    }

    if (is_int(cell)) {
        char[32] nbuf;
        csv_emit_escaped_cell(out, int_to_string(cell.int_val, &nbuf), opts.delimiter, opts.quote_char, opts.quote_style);
        return null;
    }

    if (is_double(cell)) {
        char[64] dbuf;
        csv_emit_escaped_cell(out, double_to_string(cell.double_val, &dbuf), opts.delimiter, opts.quote_char, opts.quote_style);
        return null;
    }

    if (is_float32(cell)) {
        char[64] fbuf;
        csv_emit_escaped_cell(out, double_to_string((double)cell.float32_val, &fbuf), opts.delimiter, opts.quote_char, opts.quote_style);
        return null;
    }

    if (is_symbol(cell)) {
        csv_emit_escaped_cell(out, interp.symbols.get_name(cell.sym_val), opts.delimiter, opts.quote_char, opts.quote_style);
        return null;
    }

    return data_format_raise(interp, "type/arg-mismatch", "csv-emit: row cells must be string/int/double/symbol/nil");
}

fn Value* csv_emit_row_from_array(DString* out, Value* row, CsvEmitOptions* opts, Interp* interp) {
    for (usz i = 0; i < row.array_val.length; i++) {
        if (i > 0) out.append_char(opts.delimiter);
        if (Value* err = csv_emit_cell(out, row.array_val.items[i], opts, interp)) return err;
    }
    return null;
}

fn Value* csv_emit_row_from_list(DString* out, Value* row, CsvEmitOptions* opts, Interp* interp) {
    bool first = true;
    Value* curr = row;
    while (is_cons(curr)) {
        if (!first) out.append_char(opts.delimiter);
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

fn Value* prim_csv_emit(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 2) {
        return data_format_raise(interp, "type/arity", "csv-emit: expected 1-2 arguments");
    }

    CsvEmitOptions opts;
    opts.delimiter = ',';
    opts.line_ending = "\r\n";
    opts.quote_char = '"';
    opts.quote_style = CSV_QUOTE_MINIMAL;
    opts.nil_as = "";
    opts.strict = true;
    if (args.len == 2) {
        if (Value* err = parse_csv_emit_options(args[1], &opts, interp)) return err;
    }

    if (opts.strict && opts.quote_char == opts.delimiter) {
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
