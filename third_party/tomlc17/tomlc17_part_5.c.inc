  /* 0x00000800 - 0x0000FFFF:
     1110xxxx 10xxxxxx 10xxxxxx
  */
  if (0xE == (i >> 4)) {
    if (len < 3)
      return -1;
    v = i & 0x0F;
    for (int j = 0; j < 2; j++) {
      i = *buf++;
      if (0x2 != (i >> 6))
        return -1;
      v = (v << 6) | (i & 0x3f);
    }
    return *ret = v, (const char *)buf - orig;
  }

  /* 0x00010000 - 0x001FFFFF:
     11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
  */
  if (0x1E == (i >> 3)) {
    if (len < 4)
      return -1;
    v = i & 0x07;
    for (int j = 0; j < 3; j++) {
      i = *buf++;
      if (0x2 != (i >> 6))
        return -1;
      v = (v << 6) | (i & 0x3f);
    }
    return *ret = v, (const char *)buf - orig;
  }

  if (0) {
    // NOTE: these code points taking more than 4 bytes are not supported

    /* 0x00200000 - 0x03FFFFFF:
       111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
    */
    if (0x3E == (i >> 2)) {
      if (len < 5)
        return -1;
      v = i & 0x03;
      for (int j = 0; j < 4; j++) {
        i = *buf++;
        if (0x2 != (i >> 6))
          return -1;
        v = (v << 6) | (i & 0x3f);
      }
      return *ret = v, (const char *)buf - orig;
    }

    /* 0x04000000 - 0x7FFFFFFF:
       1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
    */
    if (0x7e == (i >> 1)) {
      if (len < 6)
        return -1;
      v = i & 0x01;
      for (int j = 0; j < 5; j++) {
        i = *buf++;
        if (0x2 != (i >> 6))
          return -1;
        v = (v << 6) | (i & 0x3f);
      }
      return *ret = v, (const char *)buf - orig;
    }
  }

  return -1;
}

/**
 * Convert a UCS char to utf8 code, and return it in buf.
 * Return #bytes used in buf to encode the char, or
 * -1 on error.
 */
static int ucs_to_utf8(uint32_t code, char buf[4]) {
  /* http://stackoverflow.com/questions/6240055/manually-converting-unicode-codepoints-into-utf-8-and-utf-16
   */
  /* The UCS code values 0xd800–0xdfff (UTF-16 surrogates) as well
   * as 0xfffe and 0xffff (UCS noncharacters) should not appear in
   * conforming UTF-8 streams.
   */
  /*
   *  https://github.com/toml-lang/toml-test/issues/165
   *  [0xd800, 0xdfff] and [0xfffe, 0xffff] are implicitly allowed by TOML, so
   * we disable the check.
   */
  if (0) {
    if (0xd800 <= code && code <= 0xdfff)
      return -1;
    if (0xfffe <= code && code <= 0xffff)
      return -1;
  }

  /* 0x00000000 - 0x0000007F:
     0xxxxxxx
  */
  if (code <= 0x7F) {
    buf[0] = (unsigned char)code;
    return 1;
  }

  /* 0x00000080 - 0x000007FF:
     110xxxxx 10xxxxxx
  */
  if (code <= 0x000007FF) {
    buf[0] = (unsigned char)(0xc0 | (code >> 6));
    buf[1] = (unsigned char)(0x80 | (code & 0x3f));
    return 2;
  }

  /* 0x00000800 - 0x0000FFFF:
     1110xxxx 10xxxxxx 10xxxxxx
  */
  if (code <= 0x0000FFFF) {
    buf[0] = (unsigned char)(0xe0 | (code >> 12));
    buf[1] = (unsigned char)(0x80 | ((code >> 6) & 0x3f));
    buf[2] = (unsigned char)(0x80 | (code & 0x3f));
    return 3;
  }

  /* 0x00010000 - 0x001FFFFF:
     11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
  */
  if (code <= 0x001FFFFF) {
    buf[0] = (unsigned char)(0xf0 | (code >> 18));
    buf[1] = (unsigned char)(0x80 | ((code >> 12) & 0x3f));
    buf[2] = (unsigned char)(0x80 | ((code >> 6) & 0x3f));
    buf[3] = (unsigned char)(0x80 | (code & 0x3f));
    return 4;
  }

#ifdef UNDEF
  if (0) {
    // NOTE: these code points taking more than 4 bytes are not supported
    /* 0x00200000 - 0x03FFFFFF:
       111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
    */
    if (code <= 0x03FFFFFF) {
      buf[0] = (unsigned char)(0xf8 | (code >> 24));
      buf[1] = (unsigned char)(0x80 | ((code >> 18) & 0x3f));
      buf[2] = (unsigned char)(0x80 | ((code >> 12) & 0x3f));
      buf[3] = (unsigned char)(0x80 | ((code >> 6) & 0x3f));
      buf[4] = (unsigned char)(0x80 | (code & 0x3f));
      return 5;
    }

    /* 0x04000000 - 0x7FFFFFFF:
       1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
    */
    if (code <= 0x7FFFFFFF) {
      buf[0] = (unsigned char)(0xfc | (code >> 30));
      buf[1] = (unsigned char)(0x80 | ((code >> 24) & 0x3f));
      buf[2] = (unsigned char)(0x80 | ((code >> 18) & 0x3f));
      buf[3] = (unsigned char)(0x80 | ((code >> 12) & 0x3f));
      buf[4] = (unsigned char)(0x80 | ((code >> 6) & 0x3f));
      buf[5] = (unsigned char)(0x80 | (code & 0x3f));
      return 6;
    }
  }
#endif
