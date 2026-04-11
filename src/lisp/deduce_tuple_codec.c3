module lisp;

// ============================================================
// Tuple encoding: Values → bytes for LMDB storage
// ============================================================
// Encode a tuple of Values into a byte buffer. Format: [tag byte][data]...
// Tags: 0=nil, 1=int(8 bytes LE), 2=double(8 bytes), 3=string(2 byte len + data), 4=symbol(4 byte id), 5=time-point(1 kind byte + 8 ints LE)
fn usz write_i32_le(char* buf, usz pos, int value) {
    buf[pos++] = (char)(value & 0xFF);
    buf[pos++] = (char)((value >> 8) & 0xFF);
    buf[pos++] = (char)((value >> 16) & 0xFF);
    buf[pos++] = (char)((value >> 24) & 0xFF);
    return pos;
}

fn int read_i32_le(char* buf, usz pos) {
    return ((int)(uint)(char)buf[pos]) |
        (((int)(uint)(char)buf[pos + 1]) << 8) |
        (((int)(uint)(char)buf[pos + 2]) << 16) |
        (((int)(uint)(char)buf[pos + 3]) << 24);
}

fn usz? encode_tuple(Value*[] vals, char* buf, usz buf_size) {
    usz pos = 0;
    for (usz i = 0; i < vals.len; i++) {
        Value* v = vals[i];
        if (v == null || v.tag == NIL) {
            if (pos >= buf_size) return WRITE_FAILED~;
            buf[pos++] = 0;
        } else if (v.tag == INT) {
            if (pos + 9 > buf_size) return WRITE_FAILED~;
            buf[pos++] = 1;
            long val = v.int_val;
            for (usz j = 0; j < 8; j++) {
                buf[pos++] = (char)(val & 0xFF);
                val >>= 8;
            }
        } else if (v.tag == DOUBLE) {
            if (pos + 9 > buf_size) return WRITE_FAILED~;
            buf[pos++] = 2;
            double dval = v.double_val;
            char* dp = (char*)&dval;
            for (usz j = 0; j < 8; j++) buf[pos++] = dp[j];
        } else if (v.tag == STRING) {
            usz slen = v.str_len;
            if (slen > 0xFFFF) slen = 0xFFFF;
            if (pos + 3 + slen > buf_size) return WRITE_FAILED~;
            buf[pos++] = 3;
            buf[pos++] = (char)(slen & 0xFF);
            buf[pos++] = (char)((slen >> 8) & 0xFF);
            for (usz j = 0; j < slen; j++) buf[pos++] = v.str_chars[j];
        } else if (v.tag == SYMBOL) {
            if (v.sym_val == INVALID_SYMBOL_ID) return WRITE_FAILED~;
            if (pos + 5 > buf_size) return WRITE_FAILED~;
            buf[pos++] = 4;
            uint sid = (uint)v.sym_val;
            buf[pos++] = (char)(sid & 0xFF);
            buf[pos++] = (char)((sid >> 8) & 0xFF);
            buf[pos++] = (char)((sid >> 16) & 0xFF);
            buf[pos++] = (char)((sid >> 24) & 0xFF);
        } else if (v.tag == TIME_POINT) {
            if (pos + 34 > buf_size) return WRITE_FAILED~;
            buf[pos++] = 5;
            buf[pos++] = (char)v.time_point_val.kind;
            pos = write_i32_le(buf, pos, v.time_point_val.year);
            pos = write_i32_le(buf, pos, v.time_point_val.month);
            pos = write_i32_le(buf, pos, v.time_point_val.day);
            pos = write_i32_le(buf, pos, v.time_point_val.hour);
            pos = write_i32_le(buf, pos, v.time_point_val.minute);
            pos = write_i32_le(buf, pos, v.time_point_val.second);
            pos = write_i32_le(buf, pos, v.time_point_val.microsecond);
            pos = write_i32_le(buf, pos, v.time_point_val.tz_offset);
        } else {
            if (pos >= buf_size) return WRITE_FAILED~;
            buf[pos++] = 0;  // unsupported → nil
        }
    }
    return pos;
}

// Decode a tuple from bytes back to Values
fn usz? decode_tuple(char* buf, usz buf_size, Value*[] out, Interp* interp) {
    if (interp == null) return READ_FAILED~;
    usz pos = 0;
    usz count = 0;
    while (pos < buf_size && count < out.len) {
        char tag = buf[pos++];
        if (tag == 0) {
            out[count++] = make_nil(interp);
        } else if (tag == 1) {
            if (pos + 8 > buf_size) return READ_FAILED~;
            long val = 0;
            for (usz j = 0; j < 8; j++) val |= ((long)(uint)(char)buf[pos + j]) << (j * 8);
            pos += 8;
            out[count++] = make_int(interp, val);
        } else if (tag == 2) {
            if (pos + 8 > buf_size) return READ_FAILED~;
            double dval;
            char* dp = (char*)&dval;
            for (usz j = 0; j < 8; j++) dp[j] = buf[pos + j];
            pos += 8;
            out[count++] = make_double(interp, dval);
        } else if (tag == 3) {
            if (pos + 2 > buf_size) return READ_FAILED~;
            usz slen = (usz)(uint)(char)buf[pos] | ((usz)(uint)(char)buf[pos + 1] << 8);
            pos += 2;
            if (pos + slen > buf_size) return READ_FAILED~;
            out[count++] = make_string(interp, buf[pos..pos + slen - 1]);
            pos += slen;
        } else if (tag == 4) {
            if (pos + 4 > buf_size) return READ_FAILED~;
            uint sid = ((uint)buf[pos] & 0xFF) |
                (((uint)buf[pos + 1] & 0xFF) << 8) |
                (((uint)buf[pos + 2] & 0xFF) << 16) |
                (((uint)buf[pos + 3] & 0xFF) << 24);
            pos += 4;
            if (sid == (uint)INVALID_SYMBOL_ID || (usz)sid >= interp.symbols.count) return READ_FAILED~;
            Value* v = interp.alloc_value();
            if (v == null) return READ_FAILED~;
            v.tag = SYMBOL;
            v.sym_val = (SymbolId)sid;
            out[count++] = v;
        } else if (tag == 5) {
            if (pos + 32 > buf_size) return READ_FAILED~;
            TimePointKind kind = (TimePointKind)buf[pos++];
            int year = read_i32_le(buf, pos); pos += 4;
            int month = read_i32_le(buf, pos); pos += 4;
            int day = read_i32_le(buf, pos); pos += 4;
            int hour = read_i32_le(buf, pos); pos += 4;
            int minute = read_i32_le(buf, pos); pos += 4;
            int second = read_i32_le(buf, pos); pos += 4;
            int microsecond = read_i32_le(buf, pos); pos += 4;
            int tz_offset = read_i32_le(buf, pos); pos += 4;
            out[count++] = make_time_point(interp, kind, year, month, day, hour, minute, second, microsecond, tz_offset);
        } else {
            return READ_FAILED~;  // unknown tag = malformed data
        }
    }
    return count;
}
