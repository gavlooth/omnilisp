module lisp;

import std::collections::list;
import std::io;
import main;

// =============================================================================
// String conversion helpers (shared across primitives, eval, value printing)
// =============================================================================

/**
 * Convert integer to string in caller-provided buffer. Returns slice of written chars.
 */
fn char[] int_to_string(long val, char[] buffer) {
    if (val == 0) { buffer[0] = '0'; return buffer[:1]; }
    usz len = 0;
    bool negative = val < 0;
    if (negative) val = -val;
    while (val > 0 && len < buffer.len - 1) {
        buffer[len++] = (char)('0' + (val % 10));
        val /= 10;
    }
    if (negative) buffer[len++] = '-';
    // Reverse in-place
    for (usz j = 0; j < len / 2; j++) {
        char tmp = buffer[j];
        buffer[j] = buffer[len - 1 - j];
        buffer[len - 1 - j] = tmp;
    }
    return buffer[:len];
}

fn usz append_padded_int(char[] dst, long value, usz width) {
    char[32] num_buf;
    char[] num = int_to_string(value, &num_buf);
    usz pos = 0;
    usz pad = width > num.len ? width - num.len : 0;
    for (usz i = 0; i < pad; i++) dst[pos++] = '0';
    for (usz i = 0; i < num.len; i++) dst[pos++] = num[i];
    return pos;
}

/**
 * Format a TimePoint into caller-provided buffer. Returns slice of written chars.
 */
fn char[] time_point_to_string(TimePoint tp, char[] buffer) {
    if (buffer.len == 0) return buffer[:0];

    usz pos = 0;
    if (tp.kind == TIME_POINT_DATE) {
        pos += append_padded_int(buffer[pos..], tp.year, 4);
        buffer[pos++] = '-';
        pos += append_padded_int(buffer[pos..], tp.month, 2);
        buffer[pos++] = '-';
        pos += append_padded_int(buffer[pos..], tp.day, 2);
        return buffer[:pos];
    }

    if (tp.kind == TIME_POINT_TIME) {
        pos += append_padded_int(buffer[pos..], tp.hour, 2);
        buffer[pos++] = ':';
        pos += append_padded_int(buffer[pos..], tp.minute, 2);
        buffer[pos++] = ':';
        pos += append_padded_int(buffer[pos..], tp.second, 2);
        if (tp.microsecond > 0) {
            buffer[pos++] = '.';
            pos += append_padded_int(buffer[pos..], tp.microsecond, 6);
        }
        return buffer[:pos];
    }

    if (tp.kind == TIME_POINT_DATETIMETZ && tp.tz_offset != 0) {
        int tza = tp.tz_offset < 0 ? -tp.tz_offset : tp.tz_offset;
        int tzh = tza / 60;
        int tzm = tza % 60;
        char sign = tp.tz_offset < 0 ? '-' : '+';
        pos += append_padded_int(buffer[pos..], tp.year, 4);
        buffer[pos++] = '-';
        pos += append_padded_int(buffer[pos..], tp.month, 2);
        buffer[pos++] = '-';
        pos += append_padded_int(buffer[pos..], tp.day, 2);
        buffer[pos++] = 'T';
        pos += append_padded_int(buffer[pos..], tp.hour, 2);
        buffer[pos++] = ':';
        pos += append_padded_int(buffer[pos..], tp.minute, 2);
        buffer[pos++] = ':';
        pos += append_padded_int(buffer[pos..], tp.second, 2);
        if (tp.microsecond > 0) {
            buffer[pos++] = '.';
            pos += append_padded_int(buffer[pos..], tp.microsecond, 6);
        }
        buffer[pos++] = sign;
        pos += append_padded_int(buffer[pos..], tzh, 2);
        buffer[pos++] = ':';
        pos += append_padded_int(buffer[pos..], tzm, 2);
        return buffer[:pos];
    }

    if (tp.microsecond > 0) {
        pos += append_padded_int(buffer[pos..], tp.year, 4);
        buffer[pos++] = '-';
        pos += append_padded_int(buffer[pos..], tp.month, 2);
        buffer[pos++] = '-';
        pos += append_padded_int(buffer[pos..], tp.day, 2);
        buffer[pos++] = 'T';
        pos += append_padded_int(buffer[pos..], tp.hour, 2);
        buffer[pos++] = ':';
        pos += append_padded_int(buffer[pos..], tp.minute, 2);
        buffer[pos++] = ':';
        pos += append_padded_int(buffer[pos..], tp.second, 2);
        buffer[pos++] = '.';
        pos += append_padded_int(buffer[pos..], tp.microsecond, 6);
        return buffer[:pos];
    }
    pos += append_padded_int(buffer[pos..], tp.year, 4);
    buffer[pos++] = '-';
    pos += append_padded_int(buffer[pos..], tp.month, 2);
    buffer[pos++] = '-';
    pos += append_padded_int(buffer[pos..], tp.day, 2);
    buffer[pos++] = 'T';
    pos += append_padded_int(buffer[pos..], tp.hour, 2);
    buffer[pos++] = ':';
    pos += append_padded_int(buffer[pos..], tp.minute, 2);
    buffer[pos++] = ':';
    pos += append_padded_int(buffer[pos..], tp.second, 2);
    return buffer[:pos];
}

/**
 * Format double to string in caller-provided buffer, ensuring decimal point.
 * Handles NaN, +inf, -inf. Buffer must be at least 64 bytes.
 */
fn char[] double_to_string(double val, char[] buffer) {
    if (val != val) {
        char[] nan = "+nan.0";
        for (usz i = 0; i < nan.len; i++) buffer[i] = nan[i];
        return buffer[:nan.len];
    }
    if (val == 1.0/0.0) {
        char[] inf = "+inf.0";
        for (usz i = 0; i < inf.len; i++) buffer[i] = inf[i];
        return buffer[:inf.len];
    }
    if (val == -1.0/0.0) {
        char[] ninf = "-inf.0";
        for (usz i = 0; i < ninf.len; i++) buffer[i] = ninf[i];
        return buffer[:ninf.len];
    }
    char[64] fmt_buf;
    usz len = 0;
    if (try rendered_double = io::bprintf(&fmt_buf, "%.15g", val)) {
        for (usz i = 0; i < rendered_double.len; i++) buffer[i] = rendered_double[i];
        len = rendered_double.len;
    } else {
        char[] fallback = "0.0";
        for (usz i = 0; i < fallback.len; i++) buffer[i] = fallback[i];
        len = fallback.len;
    }
    bool has_dot = false;
    for (usz i = 0; i < len; i++) {
        if (buffer[i] == '.' || buffer[i] == 'e' || buffer[i] == 'E') { has_dot = true; break; }
    }
    if (!has_dot && len + 2 <= buffer.len) {
        buffer[len] = '.';
        buffer[len + 1] = '0';
        len += 2;
    }
    return buffer[:len];
}
