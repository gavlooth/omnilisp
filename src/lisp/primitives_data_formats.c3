module lisp;

import std::core::mem;
import std::io;
import main;

alias OmniTomlParse = void;
alias OmniTomlDatum = void;

extern fn OmniTomlParse* omni_toml_parse(char* src, usz len, char* err_buf, usz err_cap) @extern("omni_toml_parse");
extern fn OmniTomlParse* omni_toml_parse_with_options(char* src, usz len, char* err_buf, usz err_cap, bool check_utf8) @extern("omni_toml_parse_with_options");
extern fn void omni_toml_result_free(OmniTomlParse* result) @extern("omni_toml_result_free");
extern fn OmniTomlDatum* omni_toml_result_root(OmniTomlParse* result) @extern("omni_toml_result_root");
extern fn int omni_toml_datum_type(OmniTomlDatum* datum) @extern("omni_toml_datum_type");
extern fn char* omni_toml_datum_str_ptr(OmniTomlDatum* datum) @extern("omni_toml_datum_str_ptr");
extern fn int omni_toml_datum_str_len(OmniTomlDatum* datum) @extern("omni_toml_datum_str_len");
extern fn long omni_toml_datum_int64(OmniTomlDatum* datum) @extern("omni_toml_datum_int64");
extern fn double omni_toml_datum_fp64(OmniTomlDatum* datum) @extern("omni_toml_datum_fp64");
extern fn int omni_toml_datum_bool(OmniTomlDatum* datum) @extern("omni_toml_datum_bool");
extern fn int omni_toml_datum_table_size(OmniTomlDatum* datum) @extern("omni_toml_datum_table_size");
extern fn char* omni_toml_datum_table_key(OmniTomlDatum* datum, int index) @extern("omni_toml_datum_table_key");
extern fn int omni_toml_datum_table_key_len(OmniTomlDatum* datum, int index) @extern("omni_toml_datum_table_key_len");
extern fn OmniTomlDatum* omni_toml_datum_table_value(OmniTomlDatum* datum, int index) @extern("omni_toml_datum_table_value");
extern fn int omni_toml_datum_array_size(OmniTomlDatum* datum) @extern("omni_toml_datum_array_size");
extern fn OmniTomlDatum* omni_toml_datum_array_elem(OmniTomlDatum* datum, int index) @extern("omni_toml_datum_array_elem");
extern fn int omni_toml_datum_timestamp_parts(OmniTomlDatum* datum, int* year, int* month, int* day, int* hour, int* minute, int* second, int* usec, int* tz) @extern("omni_toml_datum_timestamp_parts");

const int TOML_UNKNOWN = 0;
const int TOML_STRING = 1;
const int TOML_INT64 = 2;
const int TOML_FP64 = 3;
const int TOML_BOOLEAN = 4;
const int TOML_DATE = 5;
const int TOML_TIME = 6;
const int TOML_DATETIME = 7;
const int TOML_DATETIMETZ = 8;
const int TOML_ARRAY = 9;
const int TOML_TABLE = 10;

fn char[] toml_timestamp_to_string(OmniTomlDatum* datum, int typ, char[] out_buf) {
    int year;
    int month;
    int day;
    int hour;
    int minute;
    int second;
    int usec;
    int tz;

    if (!omni_toml_datum_timestamp_parts(datum, &year, &month, &day, &hour, &minute, &second, &usec, &tz)) {
        return "";
    }

    switch (typ) {
        case TOML_DATE:
            return io::bprintf(out_buf, "%04d-%02d-%02d", year, month, day)!!;
        case TOML_TIME:
            if (usec > 0) {
                return io::bprintf(out_buf, "%02d:%02d:%02d.%06d", hour, minute, second, usec)!!;
            }
            return io::bprintf(out_buf, "%02d:%02d:%02d", hour, minute, second)!!;
        case TOML_DATETIME:
            if (usec > 0) {
                return io::bprintf(out_buf, "%04d-%02d-%02dT%02d:%02d:%02d.%06d", year, month, day, hour, minute, second, usec)!!;
            }
            return io::bprintf(out_buf, "%04d-%02d-%02dT%02d:%02d:%02d", year, month, day, hour, minute, second)!!;
        case TOML_DATETIMETZ: {
            if (tz == 0) {
                if (usec > 0) {
                    return io::bprintf(out_buf, "%04d-%02d-%02dT%02d:%02d:%02d.%06d", year, month, day, hour, minute, second, usec)!!;
                }
                return io::bprintf(out_buf, "%04d-%02d-%02dT%02d:%02d:%02d", year, month, day, hour, minute, second)!!;
            }

            int tzm = tz < 0 ? -tz : tz;
            int tzh = tzm / 60;
            int tzm_only = tzm % 60;
            char sign = tz < 0 ? '-' : '+';
            if (usec > 0) {
                return io::bprintf(out_buf, "%04d-%02d-%02dT%02d:%02d:%02d.%06d%c%02d:%02d",
                                   year, month, day, hour, minute, second, usec, sign, tzh, tzm_only)!!;
            }
            return io::bprintf(out_buf, "%04d-%02d-%02dT%02d:%02d:%02d%c%02d:%02d",
                               year, month, day, hour, minute, second, sign, tzh, tzm_only)!!;
        }
        default:
            return "";
    }
}

fn Value* toml_datum_to_omni(OmniTomlDatum* datum, Interp* interp) {
    if (datum == null) return make_nil(interp);
    int year;
    int month;
    int day;
    int hour;
    int minute;
    int second;
    int usec;
    int tz;

    int typ = omni_toml_datum_type(datum);
    switch (typ) {
        case TOML_STRING: {
            char* s = omni_toml_datum_str_ptr(datum);
            int len = omni_toml_datum_str_len(datum);
            if (s == null || len < 0) return make_string(interp, "");
            return make_string(interp, s[:(usz)len]);
        }
        case TOML_INT64:
            return make_int(interp, omni_toml_datum_int64(datum));
        case TOML_FP64:
            return make_double(interp, omni_toml_datum_fp64(datum));
        case TOML_BOOLEAN:
            return omni_toml_datum_bool(datum) != 0 ? make_symbol(interp, interp.sym_true) : make_nil(interp);
        case TOML_ARRAY: {
            usz count = (usz)omni_toml_datum_array_size(datum);
            Value* arr = make_array(interp, count);
            for (usz i = 0; i < count; i++) {
                Value* v = toml_datum_to_omni(omni_toml_datum_array_elem(datum, (int)i), interp);
                arr.array_val.items[i] = v;
                arr.array_val.length++;
            }
            return arr;
        }
        case TOML_TABLE: {
            usz count = (usz)omni_toml_datum_table_size(datum);
            Value* dict = make_hashmap(interp, (uint)(count < 8 ? 8 : count * 2));
            for (usz i = 0; i < count; i++) {
                char* key = omni_toml_datum_table_key(datum, (int)i);
                int key_len = omni_toml_datum_table_key_len(datum, (int)i);
                if (key == null || key_len < 0) {
                    key = "";
                    key_len = 0;
                }
                Value* key_v = make_string(interp, key[:(usz)key_len]);
                Value* v = toml_datum_to_omni(omni_toml_datum_table_value(datum, (int)i), interp);
                hashmap_set(dict.hashmap_val, key_v, v, interp);
            }
            return dict;
        }
        case TOML_DATE:
        case TOML_TIME:
        case TOML_DATETIME:
        case TOML_DATETIMETZ: {
            if (!omni_toml_datum_timestamp_parts(datum, &year, &month, &day, &hour, &minute, &second, &usec, &tz)) {
                return make_nil(interp);
            }

            TimePointKind kind = TIME_POINT_DATETIME;
            if (typ == TOML_DATE) {
                kind = TIME_POINT_DATE;
                return make_time_point(interp, kind, year, month, day, 0, 0, 0, 0, 0);
            }
            if (typ == TOML_TIME) {
                kind = TIME_POINT_TIME;
                return make_time_point(interp, kind, 0, 0, 0, hour, minute, second, usec, 0);
            }

            if (typ == TOML_DATETIMETZ) {
                if (tz != 0) {
                    kind = TIME_POINT_DATETIMETZ;
                }
                return make_time_point(interp, kind, year, month, day, hour, minute, second, usec, tz);
            }
            return make_time_point(interp, TIME_POINT_DATETIME, year, month, day, hour, minute, second, usec, 0);
        }
        default:
            return make_nil(interp);
    }
}

struct CsvFieldBuf {
    char* data;
    usz   len;
    usz   cap;
}

struct CsvParseOptions {
    char delimiter;
    bool strict;
}

const int CSV_QUOTE_MINIMAL = 0;
const int CSV_QUOTE_ALWAYS = 1;
const int CSV_QUOTE_NONE = 2;

struct TomlParseOptions {
    bool check_utf8;
}

struct CsvEmitOptions {
    char delimiter;
    char[] line_ending;
    char quote_char;
    int quote_style;
    char[] nil_as;
    bool strict;
}

fn bool csv_is_symbol_name(Value* v, char[] name, Interp* interp) {
    if (v == null || v.tag != SYMBOL) return false;
    char[] sym_name = interp.symbols.get_name(v.sym_val);
    if (sym_name.len != name.len) return false;
    for (usz i = 0; i < name.len; i++) {
        if (sym_name[i] != name[i]) return false;
    }
    return true;
}

fn bool toml_is_symbol_name(Value* v, char[] name, Interp* interp) {
    return csv_is_symbol_name(v, name, interp);
}

fn Value* parse_csv_char_option(
    Value* value,
    char* out_char,
    char[] option_name,
    Interp* interp,
    char[] context
) {
    if (value == null || value.tag != STRING || value.str_len != 1) {
        if (string_equals(context, "csv-parse") && string_equals(option_name, "delimiter")) {
            return raise_error(interp, "csv-parse: delimiter must be a one-character string");
        }
        if (string_equals(context, "csv-emit") && string_equals(option_name, "delimiter")) {
            return raise_error(interp, "csv-emit: delimiter must be a one-character string");
        }
        if (string_equals(context, "csv-emit") && string_equals(option_name, "quote-char")) {
            return raise_error(interp, "csv-emit: quote-char must be a one-character string");
        }

        return raise_error(interp, "option must be a one-character string");
    }

    *out_char = value.str_chars[0];
    return null;
}

fn Value* parse_csv_bool_option(
    Value* value,
    bool* out_value,
    char[] option_name,
    Interp* interp,
    char[] context
) {
    if (is_nil(value)) {
        *out_value = false;
        return null;
    }
    if (is_symbol(value) && value.sym_val == interp.sym_true) {
        *out_value = true;
        return null;
    }
    if (is_symbol(value) && value.sym_val == interp.sym_false) {
        *out_value = false;
        return null;
    }

    char[200] msg_buf;
    char[] msg = io::bprintf(&msg_buf, "%s: %s must be a boolean (true/false)", context, option_name)!!;
    return raise_error(interp, msg);
}

fn Value* parse_csv_line_ending(Value* value, char[]* out_line_ending, Interp* interp) {
    if (value == null || value.tag != STRING) {
        return raise_error(interp, "csv-emit: line-ending must be a string");
    }

    if (value.str_len == 1 && (value.str_chars[0] == '\n' || value.str_chars[0] == '\r')) {
        *out_line_ending = value.str_chars[:value.str_len];
        return null;
    }

    if (value.str_len == 2 && value.str_chars[0] == '\r' && value.str_chars[1] == '\n') {
        *out_line_ending = value.str_chars[:value.str_len];
        return null;
    }

    return raise_error(interp, "csv-emit: line-ending must be \"\\n\", \"\\r\", or \"\\r\\n\"");
}

fn Value* parse_csv_quote_style_option(Value* value, int* out_style, Interp* interp) {
    if (value == null || value.tag != SYMBOL) {
        if (is_cons(value)) {
            Value* quote_sym = car(value);
            if (!is_nil(quote_sym) && is_symbol(quote_sym) && csv_is_symbol_name(quote_sym, "quote", interp)) {
                Value* quote_arg = cdr(value);
                if (!is_cons(quote_arg) || !is_nil(cdr(quote_arg))) {
                    return raise_error(interp, "csv-emit: quote-style must be one of 'minimal, 'always, or 'none");
                }
                value = car(quote_arg);
            }
        }
    }
    if (value == null || value.tag != SYMBOL) {
        return raise_error(interp, "csv-emit: quote-style must be one of 'minimal, 'always, or 'none");
    }

    if (csv_is_symbol_name(value, "minimal", interp)) {
        *out_style = CSV_QUOTE_MINIMAL;
        return null;
    }
    if (csv_is_symbol_name(value, "always", interp)) {
        *out_style = CSV_QUOTE_ALWAYS;
        return null;
    }
    if (csv_is_symbol_name(value, "none", interp)) {
        *out_style = CSV_QUOTE_NONE;
        return null;
    }

    return raise_error(interp, "csv-emit: quote-style must be one of 'minimal, 'always, or 'none");
}

fn Value* parse_csv_parse_option_entry(Value* key, Value* value, CsvParseOptions* opts, Interp* interp) {
    if (csv_is_symbol_name(key, "delimiter", interp)) {
        return parse_csv_char_option(value, &opts.delimiter, "delimiter", interp, "csv-parse");
    }
    if (csv_is_symbol_name(key, "strict", interp)) {
        return parse_csv_bool_option(value, &opts.strict, "strict", interp, "csv-parse");
    }
    return raise_error(interp, "csv-parse: unknown option key");
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
                return raise_error(interp, "toml-parse: check-utf8 must be a boolean (true/false)");
            }

            opts.check_utf8 = parsed;
            return null;
        }

        return raise_error(interp, "toml-parse: check-utf8 must be a boolean (true/false)");
    }

    return raise_error(interp, "toml-parse: unknown option key (supported: check-utf8)");
}

fn Value* parse_toml_parse_options(Value* options, TomlParseOptions* opts, Interp* interp) {
    if (options == null || is_nil(options)) return null;
    if (!is_cons(options)) {
        return raise_error(interp, "toml-parse: options must be a proper list");
    }

    Value* cur = options;
    while (is_cons(cur)) {
        Value* entry = car(cur);
        Value* next = cdr(cur);

        if (is_cons(entry)) {
            Value* key = car(entry);
            Value* rest = cdr(entry);
            if (!is_cons(rest) || !is_nil(cdr(rest))) {
                return raise_error(interp, "toml-parse: options must be proper key/value pairs");
            }
            Value* value = car(rest);
            Value* err = parse_toml_option_entry(key, value, opts, interp);
            if (err != null) return err;
        } else {
            if (!is_cons(next)) {
                return raise_error(interp, "toml-parse: options must be proper key/value pairs");
            }
            Value* key = entry;
            Value* value = car(next);
            Value* err = parse_toml_option_entry(key, value, opts, interp);
            if (err != null) return err;
            next = cdr(next);
        }

        cur = next;
    }

    return null;
}

fn Value* parse_csv_emit_option_entry(Value* key, Value* value, CsvEmitOptions* opts, Interp* interp) {
    if (csv_is_symbol_name(key, "delimiter", interp)) {
        return parse_csv_char_option(value, &opts.delimiter, "delimiter", interp, "csv-emit");
    }
    if (csv_is_symbol_name(key, "line-ending", interp)) {
        return parse_csv_line_ending(value, &opts.line_ending, interp);
    }
    if (csv_is_symbol_name(key, "quote-char", interp)) {
        return parse_csv_char_option(value, &opts.quote_char, "quote-char", interp, "csv-emit");
    }
    if (csv_is_symbol_name(key, "quote-style", interp)) {
        return parse_csv_quote_style_option(value, &opts.quote_style, interp);
    }
    if (csv_is_symbol_name(key, "nil-as", interp)) {
        if (value == null || value.tag == NIL) {
            opts.nil_as = "";
        } else if (value.tag == STRING) {
            opts.nil_as = value.str_chars[:value.str_len];
        } else if (is_symbol(value)) {
            opts.nil_as = interp.symbols.get_name(value.sym_val);
        } else {
            return raise_error(interp, "csv-emit: nil-as must be a string or symbol");
        }
        return null;
    }
    if (csv_is_symbol_name(key, "strict", interp)) {
        return parse_csv_bool_option(value, &opts.strict, "strict", interp, "csv-emit");
    }
    return raise_error(interp, "csv-emit: unknown option key");
}

fn bool csv_char_is_line_ending(char ch) {
    return ch == '\n' || ch == '\r';
}

fn Value* parse_csv_parse_options(Value* options, CsvParseOptions* opts, Interp* interp) {
    if (options == null || is_nil(options)) return null;
    if (is_string(options)) {
        return parse_csv_char_option(options, &opts.delimiter, "delimiter", interp, "csv-parse");
    }
    if (!is_cons(options)) {
        return raise_error(interp, "csv-parse: options must be either delimiter string or option list");
    }

    Value* cur = options;
    while (is_cons(cur)) {
        Value* entry = car(cur);
        Value* next = cdr(cur);
        if (is_cons(entry)) {
            Value* key = car(entry);
            Value* rest = cdr(entry);
            if (!is_cons(rest) || !is_nil(cdr(rest))) {
                return raise_error(interp, "csv-parse: options list must be proper key/value pairs");
            }
            Value* value = car(rest);
            Value* err = parse_csv_parse_option_entry(key, value, opts, interp);
            if (err != null) return err;
        } else {
            if (!is_cons(next)) {
                return raise_error(interp, "csv-parse: options list must be proper key/value pairs");
            }
            Value* key = entry;
            Value* value = car(next);
            Value* err = parse_csv_parse_option_entry(key, value, opts, interp);
            if (err != null) return err;
            next = cdr(next);
        }

        cur = next;
    }

    return null;
}

fn Value* parse_csv_emit_options(Value* options, CsvEmitOptions* opts, Interp* interp) {
    if (options == null || is_nil(options)) return null;
    if (is_string(options)) {
        return parse_csv_char_option(options, &opts.delimiter, "delimiter", interp, "csv-emit");
    }
    if (!is_cons(options)) {
        return raise_error(interp, "csv-emit: options must be either delimiter string or option list");
    }

    Value* cur = options;
    while (is_cons(cur)) {
        Value* entry = car(cur);
        Value* next = cdr(cur);
        if (is_cons(entry)) {
            Value* key = car(entry);
            Value* rest = cdr(entry);
            if (!is_cons(rest) || !is_nil(cdr(rest))) {
                return raise_error(interp, "csv-emit: options list must be proper key/value pairs");
            }
            Value* value = car(rest);
            Value* err = parse_csv_emit_option_entry(key, value, opts, interp);
            if (err != null) return err;
        } else {
            if (!is_cons(next)) {
                return raise_error(interp, "csv-emit: options list must be proper key/value pairs");
            }
            Value* key = entry;
            Value* value = car(next);
            Value* err = parse_csv_emit_option_entry(key, value, opts, interp);
            if (err != null) return err;
            next = cdr(next);
        }

        cur = next;
    }

    return null;
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

fn bool csv_array_push(Value* arr_v, Value* val) {
    if (arr_v == null || arr_v.tag != ARRAY || arr_v.array_val == null) return false;
    Array* arr = arr_v.array_val;
    if (arr.length >= arr.capacity) {
        usz new_cap = arr.capacity < 4 ? 4 : arr.capacity * 2;
        Value** next_items = (Value**)mem::realloc(arr.items, Value*.sizeof * new_cap);
        if (next_items == null) return false;
        arr.items = next_items;
        arr.capacity = new_cap;
    }
    arr.items[arr.length++] = val;
    return true;
}

fn Value* csv_row_push_field(Value* row, CsvFieldBuf* field, Interp* interp) {
    Value* cell = make_string(interp, field.data[:field.len]);
    if (!csv_array_push(row, cell)) {
        return raise_error(interp, "csv-parse: out of memory");
    }
    csv_field_clear(field);
    return null;
}

fn Value* csv_rows_push_row(Value* rows, Value* row, Interp* interp) {
    if (!csv_array_push(rows, row)) {
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
            if (next == '\0' || !strict) {
                if (Value* err = csv_flush_field(row, &field, interp)) return err;
                if (row.array_val.length > 0) {
                    if (Value* err = csv_rows_push_row(rows, row, interp)) return err;
                }
                row = make_array(interp, 4);
                i++;
                continue;
            }

            return raise_error(interp, "csv-parse: invalid CR line terminator");
        }

        if (ch == '\n') {
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

    switch (quote_style) {
    case CSV_QUOTE_NONE:
        out.append_string((String)text);
        return;

    case CSV_QUOTE_ALWAYS:
        // Compatibility mode: treat as "quote when required" to match existing RFC-4180-style emission contract.
        if (!needs_quote) {
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
        return;

    case CSV_QUOTE_MINIMAL:
    default:
        if (!needs_quote) {
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
        return;
    }
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

    if (is_symbol(cell)) {
        csv_emit_escaped_cell(out, interp.symbols.get_name(cell.sym_val), opts.delimiter, opts.quote_char, opts.quote_style);
        return null;
    }

    return raise_error(interp, "csv-emit: row cells must be string/int/double/symbol/nil");
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
        return raise_error(interp, "csv-emit: expected each row to be an array or proper list");
    }
    return null;
}

fn Value* csv_emit_row(DString* out, Value* row, CsvEmitOptions* opts, Interp* interp) {
    if (row == null || is_nil(row)) return null;
    if (row.tag == ARRAY) return csv_emit_row_from_array(out, row, opts, interp);
    if (is_cons(row)) return csv_emit_row_from_list(out, row, opts, interp);
    return raise_error(interp, "csv-emit: expected each row to be an array or list");
}

fn Value* prim_csv_emit(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 2) {
        return raise_error(interp, "csv-emit: expected 1-2 arguments");
    }

    CsvEmitOptions opts;
    opts.delimiter = ',';
    opts.line_ending = "\n";
    opts.quote_char = '"';
    opts.quote_style = CSV_QUOTE_MINIMAL;
    opts.nil_as = "";
    opts.strict = true;
    if (args.len == 2) {
        if (Value* err = parse_csv_emit_options(args[1], &opts, interp)) return err;
    }

    if (opts.strict && opts.quote_char == opts.delimiter) {
        return raise_error(interp, "csv-emit: quote-char cannot equal delimiter");
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
            return raise_error(interp, "csv-emit: expected rows to be an array or proper list");
        }
        return make_string(interp, out.str_view());
    }

    return raise_error(interp, "csv-emit: expected rows to be an array or list");
}

fn Value* prim_toml_parse(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 2) return raise_error(interp, "toml-parse: expected 1-2 arguments");
    if (!is_string(args[0])) return raise_error(interp, "toml-parse: expected string argument");

    TomlParseOptions opts;
    opts.check_utf8 = false;

    if (args.len > 1) {
        Value* err = parse_toml_parse_options(args[1], &opts, interp);
        if (err != null) return err;
    }

    char[] src = args[0].str_chars[:args[0].str_len];

    char[256] err_buf;
    err_buf[0] = 0;

    OmniTomlParse* handle = omni_toml_parse_with_options(src.ptr, src.len, &err_buf, err_buf.len, opts.check_utf8);
    if (handle == null) {
        if (err_buf[0] == 0) {
            return raise_error(interp, "toml-parse: invalid TOML");
        }

        char[320] msg_buf;
        char[] msg = io::bprintf(&msg_buf, "toml-parse: %s", (ZString)&err_buf)!!;
        return raise_error(interp, msg);
    }

    defer omni_toml_result_free(handle);

    OmniTomlDatum* root = omni_toml_result_root(handle);
    return toml_datum_to_omni(root, interp);
}

fn bool symbol_matches(Value* v, char[] name, Interp* interp) {
    if (v == null || v.tag != SYMBOL) return false;
    char[] sym_name = interp.symbols.get_name(v.sym_val);
    if (sym_name.len != name.len) return false;
    for (usz i = 0; i < name.len; i++) {
        if (sym_name[i] != name[i]) return false;
    }
    return true;
}

fn Value* prim_time_point(Value*[] args, Env* env, Interp* interp) {
    if (args.len == 0) return raise_error(interp, "time-point: expected kind symbol");
    if (args[0] == null || args[0].tag != SYMBOL) {
        return raise_error(interp, "time-point: expected kind symbol");
    }

    if (symbol_matches(args[0], "date", interp)) {
        if (args.len != 4) return raise_error(interp, "time-point: date expects 4 arguments: kind year month day");
        if (!is_int(args[1]) || !is_int(args[2]) || !is_int(args[3])) {
            return raise_error(interp, "time-point: expected date arguments to be integers");
        }
        return make_time_point(
            interp,
            TIME_POINT_DATE,
            (int)args[1].int_val,
            (int)args[2].int_val,
            (int)args[3].int_val,
            0,
            0,
            0,
            0,
            0
        );
    }

    if (symbol_matches(args[0], "time", interp)) {
        if (args.len != 5) return raise_error(interp, "time-point: time expects 5 arguments: kind hour minute second microsecond");
        if (!is_int(args[1]) || !is_int(args[2]) || !is_int(args[3]) || !is_int(args[4])) {
            return raise_error(interp, "time-point: expected time arguments to be integers");
        }
        return make_time_point(
            interp,
            TIME_POINT_TIME,
            0,
            0,
            0,
            (int)args[1].int_val,
            (int)args[2].int_val,
            (int)args[3].int_val,
            (int)args[4].int_val,
            0
        );
    }

    if (symbol_matches(args[0], "datetime", interp)) {
        if (args.len != 8) return raise_error(interp, "time-point: datetime expects 8 arguments: kind year month day hour minute second microsecond");
        if (!is_int(args[1]) || !is_int(args[2]) || !is_int(args[3]) || !is_int(args[4]) || !is_int(args[5]) || !is_int(args[6]) || !is_int(args[7])) {
            return raise_error(interp, "time-point: expected datetime arguments to be integers");
        }
        return make_time_point(
            interp,
            TIME_POINT_DATETIME,
            (int)args[1].int_val,
            (int)args[2].int_val,
            (int)args[3].int_val,
            (int)args[4].int_val,
            (int)args[5].int_val,
            (int)args[6].int_val,
            (int)args[7].int_val,
            0
        );
    }

    if (symbol_matches(args[0], "datetime-tz", interp) || symbol_matches(args[0], "datetimetz", interp)) {
        if (args.len != 9) return raise_error(interp, "time-point: datetime-tz expects 9 arguments: kind year month day hour minute second microsecond tz-offset");
        if (!is_int(args[1]) || !is_int(args[2]) || !is_int(args[3]) || !is_int(args[4]) || !is_int(args[5]) || !is_int(args[6]) || !is_int(args[7]) || !is_int(args[8])) {
            return raise_error(interp, "time-point: expected datetime-tz arguments to be integers");
        }
        return make_time_point(
            interp,
            TIME_POINT_DATETIMETZ,
            (int)args[1].int_val,
            (int)args[2].int_val,
            (int)args[3].int_val,
            (int)args[4].int_val,
            (int)args[5].int_val,
            (int)args[6].int_val,
            (int)args[7].int_val,
            (int)args[8].int_val
        );
    }

    return raise_error(interp, "time-point: unsupported kind, expected one of 'date, 'time, 'datetime, 'datetime-tz");
}
