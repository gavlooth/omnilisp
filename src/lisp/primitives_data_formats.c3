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
            return omni_toml_datum_bool(datum) != 0
                ? make_symbol(interp, interp.sym_true)
                : make_symbol(interp, interp.sym_false);
        case TOML_ARRAY: {
            usz count = (usz)omni_toml_datum_array_size(datum);
            Value* arr = make_array(interp, count);
            for (usz i = 0; i < count; i++) {
                Value* v = toml_datum_to_omni(omni_toml_datum_array_elem(datum, (int)i), interp);
                arr.array_val.items[i] = boundary_promote_to_root(v, interp);
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
