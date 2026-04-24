module lisp;

fn Value* data_format_raise(Interp* interp, String code, char[] message) @inline {
    return raise_error_with_payload_names(interp, "type", code, (String)message, null);
}


import main;

fn bool symbol_matches(Value* v, char[] name, Interp* interp) {
    if (v == null || v.tag != SYMBOL) return false;
    char[] sym_name = interp.symbols.get_name(v.sym_val);
    if (sym_name.len != name.len) return false;
    for (usz i = 0; i < name.len; i++) {
        if (sym_name[i] != name[i]) return false;
    }
    return true;
}

fn bool time_point_arg_in_range(Value* v, long min, long max) @inline {
    return v != null && v.tag == INT && v.int_val >= min && v.int_val <= max;
}

fn int data_hex_digit_value(char c) @inline {
    if (c >= '0' && c <= '9') return (int)(c - '0');
    if (c >= 'a' && c <= 'f') return 10 + (int)(c - 'a');
    if (c >= 'A' && c <= 'F') return 10 + (int)(c - 'A');
    return -1;
}

fn bool data_is_ascii_space(char c) @inline {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

fn int data_base64_value(char c) @inline {
    if (c >= 'A' && c <= 'Z') return (int)(c - 'A');
    if (c >= 'a' && c <= 'z') return 26 + (int)(c - 'a');
    if (c >= '0' && c <= '9') return 52 + (int)(c - '0');
    if (c == '+') return 62;
    if (c == '/') return 63;
    return -1;
}

<*
@require interp != null
*>
fn Value* prim_hex(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1 || args[0] == null || args[0].tag != STRING) {
        return data_format_raise(interp, "type/arg-mismatch", "hex: expected 1 string argument");
    }
    char[] text = args[0].str_chars[:args[0].str_len];
    usz digit_count = 0;
    for (usz i = 0; i < text.len; i++) {
        if (data_is_ascii_space(text[i])) continue;
        if (data_hex_digit_value(text[i]) < 0) {
            return data_format_raise(interp, "type/arg-mismatch", "hex: invalid hex digit");
        }
        digit_count++;
    }
    if (digit_count % 2 != 0) {
        return data_format_raise(interp, "type/arg-mismatch", "hex: expected an even number of hex digits");
    }

    usz byte_count = digit_count / 2;
    Value* arr = make_array(interp, byte_count);
    if (arr == null || arr.tag != ARRAY || arr.array_val == null) return arr;

    int high = -1;
    usz out_i = 0;
    for (usz i = 0; i < text.len; i++) {
        if (data_is_ascii_space(text[i])) continue;
        int digit = data_hex_digit_value(text[i]);
        if (high < 0) {
            high = digit;
            continue;
        }
        Value* byte_v = make_int(interp, (long)(high * 16 + digit));
        Value* promoted = array_promote_or_error(byte_v, interp);
        if (promoted == null || promoted.tag == ERROR) return promoted;
        arr.array_val.items[out_i++] = promoted;
        high = -1;
    }
    arr.array_val.length = byte_count;
    return arr;
}

<*
@require interp != null
*>
fn Value* prim_base64(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1 || args[0] == null || args[0].tag != STRING) {
        return data_format_raise(interp, "type/arg-mismatch", "base64: expected 1 string argument");
    }
    char[] text = args[0].str_chars[:args[0].str_len];
    usz clean_count = 0;
    usz padding_count = 0;
    bool padding_seen = false;
    for (usz i = 0; i < text.len; i++) {
        char c = text[i];
        if (data_is_ascii_space(c)) continue;
        if (c == '=') {
            padding_seen = true;
            padding_count++;
            if (padding_count > 2) {
                return data_format_raise(interp, "type/arg-mismatch", "base64: invalid padding");
            }
        } else {
            if (padding_seen) {
                return data_format_raise(interp, "type/arg-mismatch", "base64: data after padding");
            }
            if (data_base64_value(c) < 0) {
                return data_format_raise(interp, "type/arg-mismatch", "base64: invalid base64 character");
            }
        }
        clean_count++;
    }
    if (clean_count % 4 != 0) {
        return data_format_raise(interp, "type/arg-mismatch", "base64: expected input length to be a multiple of 4");
    }

    usz byte_count = clean_count == 0 ? 0 : (clean_count / 4) * 3 - padding_count;
    Value* arr = make_array(interp, byte_count);
    if (arr == null || arr.tag != ARRAY || arr.array_val == null) return arr;

    int[4] vals;
    bool[4] pads;
    usz q = 0;
    usz out_i = 0;
    for (usz i = 0; i < text.len; i++) {
        char c = text[i];
        if (data_is_ascii_space(c)) continue;
        pads[q] = c == '=';
        vals[q] = pads[q] ? 0 : data_base64_value(c);
        q++;
        if (q < 4) continue;

        if (pads[0] || pads[1] || (pads[2] && !pads[3])) {
            return data_format_raise(interp, "type/arg-mismatch", "base64: invalid padding");
        }

        int b0 = (vals[0] << 2) | (vals[1] >> 4);
        Value* byte0 = make_int(interp, (long)b0);
        Value* promoted0 = array_promote_or_error(byte0, interp);
        if (promoted0 == null || promoted0.tag == ERROR) return promoted0;
        arr.array_val.items[out_i++] = promoted0;

        if (!pads[2]) {
            int b1 = ((vals[1] & 15) << 4) | (vals[2] >> 2);
            Value* byte1 = make_int(interp, (long)b1);
            Value* promoted1 = array_promote_or_error(byte1, interp);
            if (promoted1 == null || promoted1.tag == ERROR) return promoted1;
            arr.array_val.items[out_i++] = promoted1;
        }
        if (!pads[3]) {
            int b2 = ((vals[2] & 3) << 6) | vals[3];
            Value* byte2 = make_int(interp, (long)b2);
            Value* promoted2 = array_promote_or_error(byte2, interp);
            if (promoted2 == null || promoted2.tag == ERROR) return promoted2;
            arr.array_val.items[out_i++] = promoted2;
        }
        q = 0;
    }
    arr.array_val.length = byte_count;
    return arr;
}

fn bool data_uuid_char_valid(char c) @inline {
    return data_hex_digit_value(c) >= 0;
}

<*
@require interp != null
*>
fn Value* prim_uuid(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1 || args[0] == null || args[0].tag != STRING) {
        return data_format_raise(interp, "type/arg-mismatch", "uuid: expected 1 string argument");
    }
    char[] text = args[0].str_chars[:args[0].str_len];
    if (text.len != 36) return data_format_raise(interp, "type/arg-mismatch", "uuid: expected canonical 36-character UUID");
    for (usz i = 0; i < text.len; i++) {
        bool dash_pos = i == 8 || i == 13 || i == 18 || i == 23;
        if (dash_pos) {
            if (text[i] != '-') return data_format_raise(interp, "type/arg-mismatch", "uuid: invalid dash position");
        } else if (!data_uuid_char_valid(text[i])) {
            return data_format_raise(interp, "type/arg-mismatch", "uuid: invalid hex digit");
        }
    }
    return make_string(interp, text);
}

fn bool data_parse_2_digits(char[] text, usz offset, int* out) {
    if (offset + 1 >= text.len) return false;
    if (text[offset] < '0' || text[offset] > '9' || text[offset + 1] < '0' || text[offset + 1] > '9') return false;
    *out = (int)(text[offset] - '0') * 10 + (int)(text[offset + 1] - '0');
    return true;
}

fn bool data_parse_4_digits(char[] text, usz offset, int* out) {
    int hi;
    int lo;
    if (!data_parse_2_digits(text, offset, &hi)) return false;
    if (!data_parse_2_digits(text, offset + 2, &lo)) return false;
    *out = hi * 100 + lo;
    return true;
}

fn Value* prim_time_parse_iso_utc(Value* arg, Interp* interp) {
    if (arg == null || arg.tag != STRING) return data_format_raise(interp, "type/arg-mismatch", "time: expected an ISO-8601 string");
    char[] text = arg.str_chars[:arg.str_len];
    if (text.len != 20 || text[4] != '-' || text[7] != '-' || text[10] != 'T' ||
        text[13] != ':' || text[16] != ':' || text[19] != 'Z') {
        return data_format_raise(interp, "type/arg-mismatch", "time: expected YYYY-MM-DDTHH:MM:SSZ");
    }
    int year;
    int month;
    int day;
    int hour;
    int minute;
    int second;
    if (!data_parse_4_digits(text, 0, &year) ||
        !data_parse_2_digits(text, 5, &month) ||
        !data_parse_2_digits(text, 8, &day) ||
        !data_parse_2_digits(text, 11, &hour) ||
        !data_parse_2_digits(text, 14, &minute) ||
        !data_parse_2_digits(text, 17, &second)) {
        return data_format_raise(interp, "type/arg-mismatch", "time: invalid ISO-8601 digits");
    }
    if (month < 1 || month > 12 || day < 1 || day > 31 ||
        hour < 0 || hour > 23 || minute < 0 || minute > 59 || second < 0 || second > 60) {
        return data_format_raise(interp, "type/arg-mismatch", "time: ISO-8601 components out of range");
    }
    return make_time_point(interp, TIME_POINT_DATETIMETZ, year, month, day, hour, minute, second, 0, 0);
}

fn bool time_point_date_args_in_range(Value* year, Value* month, Value* day) @inline {
    return time_point_arg_in_range(year, 0, 9999) &&
        time_point_arg_in_range(month, 1, 12) &&
        time_point_arg_in_range(day, 1, 31);
}

fn bool time_point_time_args_in_range(Value* hour, Value* minute, Value* second, Value* microsecond) @inline {
    return time_point_arg_in_range(hour, 0, 23) &&
        time_point_arg_in_range(minute, 0, 59) &&
        time_point_arg_in_range(second, 0, 60) &&
        time_point_arg_in_range(microsecond, 0, 999999);
}

<*
@require interp != null
*>
fn Value* prim_time_point(Value*[] args, Env* env, Interp* interp) {
    if (args.len == 0) return data_format_raise(interp, "type/arg-mismatch", "time-point: expected kind symbol");
    if (args[0] == null || args[0].tag != SYMBOL) {
        return data_format_raise(interp, "type/arg-mismatch", "time-point: expected kind symbol");
    }

    if (symbol_matches(args[0], "date", interp)) {
        if (args.len != 4) return data_format_raise(interp, "type/arg-mismatch", "time-point: date expects 4 arguments: kind year month day");
        if (!is_int(args[1]) || !is_int(args[2]) || !is_int(args[3])) {
            return data_format_raise(interp, "type/arity", "time-point: expected date arguments to be integers");
        }
        if (!time_point_date_args_in_range(args[1], args[2], args[3])) {
            return data_format_raise(interp, "type/arg-mismatch", "time-point: date arguments out of range");
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
        if (args.len != 5) return data_format_raise(interp, "type/arg-mismatch", "time-point: time expects 5 arguments: kind hour minute second microsecond");
        if (!is_int(args[1]) || !is_int(args[2]) || !is_int(args[3]) || !is_int(args[4])) {
            return data_format_raise(interp, "type/arity", "time-point: expected time arguments to be integers");
        }
        if (!time_point_time_args_in_range(args[1], args[2], args[3], args[4])) {
            return data_format_raise(interp, "type/arg-mismatch", "time-point: time arguments out of range");
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
        if (args.len != 8) return data_format_raise(interp, "type/arg-mismatch", "time-point: datetime expects 8 arguments: kind year month day hour minute second microsecond");
        if (!is_int(args[1]) || !is_int(args[2]) || !is_int(args[3]) || !is_int(args[4]) || !is_int(args[5]) || !is_int(args[6]) || !is_int(args[7])) {
            return data_format_raise(interp, "type/arity", "time-point: expected datetime arguments to be integers");
        }
        if (!time_point_date_args_in_range(args[1], args[2], args[3]) ||
            !time_point_time_args_in_range(args[4], args[5], args[6], args[7])) {
            return data_format_raise(interp, "type/arg-mismatch", "time-point: datetime arguments out of range");
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
        if (args.len != 9) return data_format_raise(interp, "type/arg-mismatch", "time-point: datetime-tz expects 9 arguments: kind year month day hour minute second microsecond tz-offset");
        if (!is_int(args[1]) || !is_int(args[2]) || !is_int(args[3]) || !is_int(args[4]) || !is_int(args[5]) || !is_int(args[6]) || !is_int(args[7]) || !is_int(args[8])) {
            return data_format_raise(interp, "type/arity", "time-point: expected datetime-tz arguments to be integers");
        }
        if (!time_point_date_args_in_range(args[1], args[2], args[3]) ||
            !time_point_time_args_in_range(args[4], args[5], args[6], args[7]) ||
            !time_point_arg_in_range(args[8], -1440, 1440)) {
            return data_format_raise(interp, "type/arg-mismatch", "time-point: datetime-tz arguments out of range");
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

    return data_format_raise(interp, "type/arg-mismatch", "time-point: unsupported kind, expected one of 'date, 'time, 'datetime, 'datetime-tz");
}
