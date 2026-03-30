module lisp;


import std::core::mem;
import std::io;
import main;

fn bool csv_line_ending_is_crlf(char ch, char next) @inline {
    return ch == '\r' && next == '\n';
}

fn bool csv_field_init(CsvFieldBuf* fb, usz initial_cap) {
    if (initial_cap < 32) initial_cap = 32;
    fb.data = (char*)mem::malloc(initial_cap);
    if (fb.data == null) {
        fb.len = 0;
        fb.cap = 0;
        return false;
    }
    fb.len = 0;
    fb.cap = initial_cap;
    return true;
}

fn void csv_field_free(CsvFieldBuf* fb) {
    if (fb.data != null) mem::free(fb.data);
    fb.data = null;
    fb.len = 0;
    fb.cap = 0;
}

fn void csv_field_clear(CsvFieldBuf* fb) @inline {
    fb.len = 0;
}

fn bool csv_field_push(CsvFieldBuf* fb, char ch) {
    if (fb.len + 1 > fb.cap) {
        usz new_cap = fb.cap == 0 ? 32 : fb.cap * 2;
        char* next = (char*)mem::realloc(fb.data, new_cap);
        if (next == null) return false;
        fb.data = next;
        fb.cap = new_cap;
    }
    fb.data[fb.len++] = ch;
    return true;
}

fn bool csv_array_push(Value* arr_v, Value* val, Interp* interp) {
    if (arr_v == null || arr_v.tag != ARRAY || arr_v.array_val == null) return false;
    Array* arr = arr_v.array_val;
    if (arr.length >= arr.capacity) {
        usz new_cap = arr.capacity < 4 ? 4 : arr.capacity * 2;
        Value** next_items = (Value**)mem::realloc(arr.items, Value*.sizeof * new_cap);
        if (next_items == null) return false;
        arr.items = next_items;
        arr.capacity = new_cap;
    }
    Value* promoted = boundary_promote_to_root(val, interp);
    if (promoted == null) return false;
    arr.items[arr.length++] = promoted;
    return true;
}

fn Value* csv_row_push_field(Value* row, CsvFieldBuf* field, Interp* interp) {
    Value* cell = make_string(interp, field.data[:field.len]);
    if (!csv_array_push(row, cell, interp)) {
        return raise_error(interp, "csv-parse: out of memory");
    }
    csv_field_clear(field);
    return null;
}

fn Value* csv_rows_push_row(Value* rows, Value* row, Interp* interp) {
    if (!csv_array_push(rows, row, interp)) {
        return raise_error(interp, "csv-parse: out of memory");
    }
    return null;
}

fn Value* csv_flush_field(Value* row, CsvFieldBuf* field, Interp* interp) {
    if (field.len == 0 && row.array_val.length == 0) {
        return null;
    }

    return csv_row_push_field(row, field, interp);
}

fn Value* csv_flush_row(Value* rows, Value* row, CsvFieldBuf* field, Interp* interp) {
    if (field.len == 0 && row.array_val.length == 0) return null;

    Value* err = csv_row_push_field(row, field, interp);
    if (err != null) return err;
    return csv_rows_push_row(rows, row, interp);
}

fn Value* prim_csv_parse(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 2) {
        return raise_error(interp, "csv-parse: expected 1-2 arguments");
    }
    if (!is_string(args[0])) {
        return raise_error(interp, "csv-parse: expected string argument");
    }

    CsvParseOptions opts;
    opts.delimiter = ',';
    opts.strict = true;
    if (args.len == 2) {
        if (Value* err = parse_csv_parse_options(args[1], &opts, interp)) return err;
    }

    char[] src = args[0].str_chars[:args[0].str_len];
    if (src.len == 0) return make_array(interp, 0);

    Value* rows = make_array(interp, 4);
    Value* row = make_array(interp, 4);

    CsvFieldBuf field;
    if (!csv_field_init(&field, 32)) {
        return raise_error(interp, "csv-parse: out of memory");
    }
    defer csv_field_free(&field);

    bool in_quotes = false;
    bool after_quote = false;
    bool strict = opts.strict;
    char delimiter = opts.delimiter;

    usz i = 0;
    while (i <= src.len) {
        bool at_end = i == src.len;
        char ch = at_end ? '\0' : src[i];
        char next = i + 1 < src.len ? src[i + 1] : '\0';

        if (at_end) {
            if (in_quotes) {
                return raise_error(interp, "csv-parse: unterminated quoted field");
            }

            if (Value* err = csv_flush_row(rows, row, &field, interp)) return err;
            break;
        }

        if (after_quote) {
            if (ch == delimiter) {
                after_quote = false;
                if (Value* err = csv_flush_field(row, &field, interp)) return err;
                i++;
                continue;
            }

            if (csv_char_is_line_ending(ch)) {
                after_quote = false;
                if (strict && !csv_line_ending_is_crlf(ch, next)) {
                    return raise_error(interp, "csv-parse: strict mode requires CRLF line endings");
                }
                if (Value* err = csv_flush_field(row, &field, interp)) return err;

                if (Value* err = csv_rows_push_row(rows, row, interp)) return err;
                row = make_array(interp, 4);

                if (ch == '\r' && next == '\n') {
                    i += 2;
                } else {
                    i++;
                }
                continue;
            }

            if (!strict) {
                if (!csv_field_push(&field, '"')) return raise_error(interp, "csv-parse: out of memory");
                after_quote = false;
                continue;
            }

            return raise_error(interp, "csv-parse: unexpected characters after closing quote");
        }

        if (in_quotes) {
            if (ch == '"') {
                if (i + 1 < src.len && src[i + 1] == '"') {
                    if (!csv_field_push(&field, '"')) return raise_error(interp, "csv-parse: out of memory");
                    i += 2;
                    continue;
                }
                in_quotes = false;
                after_quote = true;
                i++;
                continue;
            }

            if (!csv_field_push(&field, ch)) return raise_error(interp, "csv-parse: out of memory");
            i++;
            continue;
        }

        if (ch == '\r') {
            if (next == '\n') {
                if (Value* err = csv_flush_field(row, &field, interp)) return err;
                if (row.array_val.length > 0) {
                    if (Value* err = csv_rows_push_row(rows, row, interp)) return err;
                }

                row = make_array(interp, 4);
                i += 2;
                continue;
            }
            if (!strict) {
                if (Value* err = csv_flush_field(row, &field, interp)) return err;
                if (row.array_val.length > 0) {
                    if (Value* err = csv_rows_push_row(rows, row, interp)) return err;
                }
                row = make_array(interp, 4);
                i++;
                continue;
            }

            return raise_error(interp, "csv-parse: strict mode requires CRLF line endings");
        }

        if (ch == '\n') {
            if (strict) {
                return raise_error(interp, "csv-parse: strict mode requires CRLF line endings");
            }
            if (Value* err = csv_flush_field(row, &field, interp)) return err;
            if (row.array_val.length > 0) {
                if (Value* err = csv_rows_push_row(rows, row, interp)) return err;
            }

            row = make_array(interp, 4);
            i++;
            continue;
        }

        if (ch == '"') {
            if (field.len != 0) {
                if (strict) return raise_error(interp, "csv-parse: quote not allowed in unquoted field");
                if (!csv_field_push(&field, '"')) return raise_error(interp, "csv-parse: out of memory");
                i++;
                continue;
            }
            in_quotes = true;
            i++;
            continue;
        }

        if (ch == delimiter) {
            if (Value* err = csv_row_push_field(row, &field, interp)) return err;
            i++;
            continue;
        }

        if (!csv_field_push(&field, ch)) return raise_error(interp, "csv-parse: out of memory");
        i++;
    }

    return rows;
}
