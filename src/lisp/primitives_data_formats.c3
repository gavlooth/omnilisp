module lisp;

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
        if (!time_point_date_args_in_range(args[1], args[2], args[3])) {
            return raise_error(interp, "time-point: date arguments out of range");
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
        if (!time_point_time_args_in_range(args[1], args[2], args[3], args[4])) {
            return raise_error(interp, "time-point: time arguments out of range");
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
        if (!time_point_date_args_in_range(args[1], args[2], args[3]) ||
            !time_point_time_args_in_range(args[4], args[5], args[6], args[7])) {
            return raise_error(interp, "time-point: datetime arguments out of range");
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
        if (!time_point_date_args_in_range(args[1], args[2], args[3]) ||
            !time_point_time_args_in_range(args[4], args[5], args[6], args[7]) ||
            !time_point_arg_in_range(args[8], -1440, 1440)) {
            return raise_error(interp, "time-point: datetime-tz arguments out of range");
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
