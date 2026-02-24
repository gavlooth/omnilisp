module lisp;

// =============================================================================
// UTF-8 Codec Utilities
// =============================================================================

/// Returns the byte length of a UTF-8 codepoint from its lead byte (1-4), or 1 for invalid.
fn usz utf8_codepoint_len(char c) {
    uint b = (uint)c;
    if (b < 0x80) return 1;       // ASCII
    if (b < 0xC0) return 1;       // continuation byte (invalid as lead)
    if (b < 0xE0) return 2;       // 110xxxxx
    if (b < 0xF0) return 3;       // 1110xxxx
    if (b < 0xF8) return 4;       // 11110xxx
    return 1;                      // invalid
}

/// Decode one UTF-8 codepoint from `bytes` starting at `*pos`.
/// Advances `*pos` past the decoded codepoint. Returns the codepoint value.
/// On invalid sequence, returns 0xFFFD (replacement character) and advances by 1.
fn uint utf8_decode(char[] bytes, usz* pos) {
    usz p = *pos;
    if (p >= bytes.len) { *pos = bytes.len; return 0xFFFD; }

    uint b0 = (uint)(char)bytes[p];
    if (b0 < 0x80) {
        *pos = p + 1;
        return b0;
    }
    if (b0 < 0xC0) {
        // Stray continuation byte
        *pos = p + 1;
        return 0xFFFD;
    }
    if (b0 < 0xE0) {
        if (p + 1 >= bytes.len) { *pos = p + 1; return 0xFFFD; }
        uint cp = ((b0 & 0x1F) << 6) | ((uint)(char)bytes[p+1] & 0x3F);
        *pos = p + 2;
        return cp;
    }
    if (b0 < 0xF0) {
        if (p + 2 >= bytes.len) { *pos = p + 1; return 0xFFFD; }
        uint cp = ((b0 & 0x0F) << 12) |
                  (((uint)(char)bytes[p+1] & 0x3F) << 6) |
                  ((uint)(char)bytes[p+2] & 0x3F);
        *pos = p + 3;
        return cp;
    }
    if (b0 < 0xF8) {
        if (p + 3 >= bytes.len) { *pos = p + 1; return 0xFFFD; }
        uint cp = ((b0 & 0x07) << 18) |
                  (((uint)(char)bytes[p+1] & 0x3F) << 12) |
                  (((uint)(char)bytes[p+2] & 0x3F) << 6) |
                  ((uint)(char)bytes[p+3] & 0x3F);
        *pos = p + 4;
        return cp;
    }
    *pos = p + 1;
    return 0xFFFD;
}

/// Encode a Unicode codepoint to UTF-8 bytes. Returns the number of bytes written (1-4).
/// `buf` must have at least 4 bytes capacity.
fn usz utf8_encode(uint cp, char[] buf) {
    if (cp < 0x80) {
        buf[0] = (char)cp;
        return 1;
    }
    if (cp < 0x800) {
        buf[0] = (char)(0xC0 | (cp >> 6));
        buf[1] = (char)(0x80 | (cp & 0x3F));
        return 2;
    }
    if (cp < 0x10000) {
        buf[0] = (char)(0xE0 | (cp >> 12));
        buf[1] = (char)(0x80 | ((cp >> 6) & 0x3F));
        buf[2] = (char)(0x80 | (cp & 0x3F));
        return 3;
    }
    if (cp < 0x110000) {
        buf[0] = (char)(0xF0 | (cp >> 18));
        buf[1] = (char)(0x80 | ((cp >> 12) & 0x3F));
        buf[2] = (char)(0x80 | ((cp >> 6) & 0x3F));
        buf[3] = (char)(0x80 | (cp & 0x3F));
        return 4;
    }
    // Invalid codepoint → replacement character
    buf[0] = (char)0xEF; buf[1] = (char)0xBF; buf[2] = (char)0xBD;
    return 3;
}

/// Count the number of Unicode codepoints in a UTF-8 byte string.
fn usz utf8_strlen(char[] bytes) {
    usz count = 0;
    usz pos = 0;
    while (pos < bytes.len) {
        pos += utf8_codepoint_len(bytes[pos]);
        count++;
    }
    return count;
}

/// Convert a codepoint index to a byte offset.
/// Returns the byte offset of the `char_idx`-th codepoint.
/// If `char_idx` is beyond the string, returns `bytes.len`.
fn usz utf8_byte_offset(char[] bytes, usz char_idx) {
    usz pos = 0;
    usz count = 0;
    while (pos < bytes.len && count < char_idx) {
        pos += utf8_codepoint_len(bytes[pos]);
        count++;
    }
    return pos;
}

/// Validate a UTF-8 byte sequence. Returns true if valid.
fn bool utf8_valid(char[] bytes) {
    usz pos = 0;
    while (pos < bytes.len) {
        uint b0 = (uint)(char)bytes[pos];
        usz expected_len;
        if (b0 < 0x80) { pos++; continue; }
        else if (b0 < 0xC0) { return false; }
        else if (b0 < 0xE0) { expected_len = 2; }
        else if (b0 < 0xF0) { expected_len = 3; }
        else if (b0 < 0xF8) { expected_len = 4; }
        else { return false; }

        if (pos + expected_len > bytes.len) return false;
        // Check continuation bytes
        for (usz i = 1; i < expected_len; i++) {
            uint b = (uint)(char)bytes[pos + i];
            if (b < 0x80 || b >= 0xC0) return false;
        }
        pos += expected_len;
    }
    return true;
}
