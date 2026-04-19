// Read a date as YYYY-MM-DD from p[]. Return #bytes consumed.
static int read_date(const char *p, int *year, int *month, int *day) {
  const char *pp = p;
  int n;
  n = read_int(p, year);
  if (n != 4 || p[4] != '-') {
    return 0;
  }
  n = read_int(p += n + 1, month);
  if (n != 2 || p[2] != '-') {
    return 0;
  }
  n = read_int(p += n + 1, day);
  if (n != 2) {
    return 0;
  }
  p += 2;
  assert(p - pp == 10);
  return p - pp;
}

// Read a time as HH:MM:SS.subsec from p[]. Return #bytes consumed.
static int read_time(const char *p, int *hour, int *minute, int *second,
                     int *usec) {
  const char *pp = p;
  int n;
  *hour = *minute = *second = *usec = 0;
  // scan hours
  n = read_int(p, hour);
  if (n != 2 || p[2] != ':') {
    return 0;
  }
  p += 3;

  // scan minutes
  n = read_int(p, minute);
  if (n != 2) {
    return 0;
  }
  if (p[2] != ':') {
    // seconds are optional in v1.1
    p += 2;
    return p - pp;
  }
  p += 3;

  // scan seconds
  n = read_int(p, second);
  if (n != 2) {
    return 0;
  }
  p += 2;

  if (*p != '.') {
    return p - pp;
  }
  p++; // skip the period
  if (!isdigit(*p)) {
    // trailing period
    return 0;
  }
  int micro_factor = 100000;
  while (isdigit(*p) && micro_factor) {
    *usec += (*p - '0') * micro_factor;
    micro_factor /= 10;
    p++;
  }
  return p - pp;
}

// Reads a timezone from p[]. Return #bytes consumed.
// tzhours and tzminutes restricted to 2-char integers only.
static int read_tzone(const char *p, char *tzsign, int *tzhour, int *tzminute) {
  const char *pp = p;

  // Default values
  *tzhour = *tzminute = 0;
  *tzsign = '+';

  // Look for Zulu
  if (*p == 'Z' || *p == 'z') {
    return 1; // done! tz is +00:00.
  }

  // Look for +/-
  *tzsign = *p++;
  if (!(*tzsign == '+' || *tzsign == '-')) {
    return 0;
  }

  // Look for HH:MM
  int n;
  n = read_int(p, tzhour);
  if (n != 2 || p[2] != ':') {
    return 0;
  }
  n = read_int(p += 3, tzminute);
  if (n != 2) {
    return 0;
  }
  p += 2;
  return p - pp;
}

static int scan_time(scanner_t *sp, token_t *tok) {
  int lineno = sp->lineno;
  char buffer[20];
  scan_copystr(sp, buffer, sizeof(buffer));

  char *p = buffer;
  int hour, minute, sec, usec;
  int len = read_time(p, &hour, &minute, &sec, &usec);
  if (len == 0) {
    return RETERROR(sp->ebuf, lineno, "invalid time");
  }
  if (!is_valid_time(hour, minute, sec, usec)) {
    return RETERROR(sp->ebuf, lineno, "invalid time");
  }

  *tok = mktoken(sp, TOK_TIME);
  tok->str.len = len;
  sp->cur += len;
  tok->u.tsval.year = -1;
  tok->u.tsval.month = -1;
  tok->u.tsval.day = -1;
  tok->u.tsval.hour = hour;
  tok->u.tsval.minute = minute;
  tok->u.tsval.sec = sec;
  tok->u.tsval.usec = usec;
  tok->u.tsval.tz = -1;
  return 0;
}

static int scan_timestamp(scanner_t *sp, token_t *tok) {
  int year, month, day, hour, minute, sec, usec, tz;
  int n;
  // make a copy of sp->cur into buffer to ensure NUL terminated string
  char buffer[80];
  scan_copystr(sp, buffer, sizeof(buffer));

  toktyp_t toktyp = TOK_FIN;
  int lineno = sp->lineno;
  const char *p = buffer;
  if (isdigit(p[0]) && isdigit(p[1]) && p[2] == ':') {
    year = month = day = hour = minute = sec = usec = tz = -1;
    n = read_time(buffer, &hour, &minute, &sec, &usec);
    if (!n) {
      return RETERROR(sp->ebuf, lineno, "invalid time");
    }
    toktyp = TOK_TIME;
    p += n;
    goto done;
  }

  year = month = day = hour = minute = sec = usec = tz = -1;
  n = read_date(p, &year, &month, &day);
  if (!n) {
    return RETERROR(sp->ebuf, lineno, "invalid date");
  }
  toktyp = TOK_DATE;
  p += n;

  // Check if there is a time component
  if (!((p[0] == 'T' || p[0] == ' ' || p[0] == 't') && isdigit(p[1]) &&
        isdigit(p[2]) && p[3] == ':')) {
    goto done; // date only
  }

  // Read the time
  n = read_time(p += 1, &hour, &minute, &sec, &usec);
  if (!n) {
    return RETERROR(sp->ebuf, lineno, "invalid timestamp");
  }
  toktyp = TOK_DATETIME;
  p += n;

  // Read the (optional) timezone
  char tzsign;
  int tzhour, tzminute;
  n = read_tzone(p, &tzsign, &tzhour, &tzminute);
  if (n == 0) {
    goto done; // datetime only
  }
  toktyp = TOK_DATETIMETZ;
  p += n;

  // Check tzminute range. This must be done here instead of is_valid_timezone()
  // because we combine tzhour and tzminute into tz (by minutes only).
  if (!(0 <= tzminute && tzminute < 60)) {
    return RETERROR(sp->ebuf, lineno, "invalid timezone");
  }
  tz = (tzhour * 60 + tzminute) * (tzsign == '-' ? -1 : 1);
  goto done; // datetimetz

done:
  *tok = mktoken(sp, toktyp);
  n = p - buffer;
  tok->str.len = n;
  sp->cur += n;

  tok->u.tsval.year = year;
  tok->u.tsval.month = month;
  tok->u.tsval.day = day;
  tok->u.tsval.hour = hour;
  tok->u.tsval.minute = minute;
  tok->u.tsval.sec = sec;
  tok->u.tsval.usec = usec;
  tok->u.tsval.tz = tz;

  // Do some error checks based on type
  switch (tok->toktyp) {
  case TOK_TIME:
    if (!is_valid_time(hour, minute, sec, usec)) {
      return RETERROR(sp->ebuf, lineno, "invalid time");
    }
    break;
  case TOK_DATE:
    if (!is_valid_date(year, month, day)) {
      return RETERROR(sp->ebuf, lineno, "invalid date");
    }
    break;
  case TOK_DATETIME:
  case TOK_DATETIMETZ:
    if (!is_valid_date(year, month, day)) {
      return RETERROR(sp->ebuf, lineno, "invalid date");
    }
    if (!is_valid_time(hour, minute, sec, usec)) {
      return RETERROR(sp->ebuf, lineno, "invalid time");
    }
    if (tok->toktyp == TOK_DATETIMETZ && !is_valid_timezone(tz)) {
      return RETERROR(sp->ebuf, lineno, "invalid timezone");
    }
    break;
  default:
    assert(0);
    return RETERROR(sp->ebuf, lineno, "internal error");
  }

  return 0;
}

// Given a toml number (int and float) in buffer[]:
//   1. squeeze out '_'
//   2. check for syntax restrictions
static int process_numstr(char *buffer, int base, const char **reason) {
  // squeeze out _
  char *q = strchr(buffer, '_');
  if (q) {
    for (int i = q - buffer; buffer[i]; i++) {
      if (buffer[i] != '_') {
        *q++ = buffer[i];
        continue;
      }
      int left = (i == 0) ? 0 : buffer[i - 1];
      int right = buffer[i + 1];
      if (!isdigit(left) && !(base == 16 && is_hex_char(left))) {
        *reason = "underscore only allowed between digits";
        return -1;
      }
      if (!isdigit(right) && !(base == 16 && is_hex_char(right))) {
        *reason = "underscore only allowed between digits";
        return -1;
      }
    }
    *q = 0;
  }

  // decimal points must be surrounded by digits. Also, convert to lowercase.
  for (int i = 0; buffer[i]; i++) {
    if (buffer[i] == '.') {
      if (i == 0 || !isdigit(buffer[i - 1]) || !isdigit(buffer[i + 1])) {
        *reason = "decimal point must be surrounded by digits";
        return -1;
      }
    } else if ('A' <= buffer[i] && buffer[i] <= 'Z') {
      buffer[i] = tolower(buffer[i]);
    }
  }

  if (base == 10) {
    // check for leading 0:  '+01' is an error!
    q = buffer;
    q += (*q == '+' || *q == '-') ? 1 : 0;
    if (q[0] == '0' && isdigit(q[1])) {
      *reason = "leading 0 in numbers";
      return -1;
    }

    // 1e+01 is also an error
    if (0 != (q = strchr(buffer, 'e'))) {
      q += (*q == '+' || *q == '-') ? 1 : 0;
      if (q[0] == '0' && isdigit(q[1])) {
        *reason = "leading 0 in numbers";
        return -1;
      }
    }
  }

  return 0;
}

static int scan_float(scanner_t *sp, token_t *tok) {
  char buffer[50]; // need to accomodate "9_007_199_254_740_991.0"
  scan_copystr(sp, buffer, sizeof(buffer));

  int lineno = sp->lineno;
  char *p = buffer;
  p += (*p == '+' || *p == '-') ? 1 : 0;
  if (0 == memcmp(p, "nan", 3) || (0 == memcmp(p, "inf", 3))) {
    p += 3;
  } else {
    p += strspn(p, "_0123456789eE.+-");
  }
  int len = p - buffer;
  buffer[len] = 0;

  const char *reason;
  if (process_numstr(buffer, 10, &reason)) {
    return RETERROR(sp->ebuf, lineno, reason);
  }

  errno = 0;
  char *q;
  double fp64 = strtod(buffer, &q);
  if (errno || *q || q == buffer) {
    return RETERROR(sp->ebuf, lineno, "error parsing float");
  }

  *tok = mktoken(sp, TOK_FLOAT);
  tok->u.fp64 = fp64;
  tok->str.len = len;
  sp->cur += len;
  return 0;
}

static int scan_number(scanner_t *sp, token_t *tok) {
  const char *reason;
  char buffer[50]; // need to accomodate "9_007_199_254_740_991.0"
  scan_copystr(sp, buffer, sizeof(buffer));

  char *p = buffer;
  int lineno = sp->lineno;
  // process %0x, %0o or %0b integers
  if (p[0] == '0') {
    const char *span = 0;
    int base = 0;
    switch (p[1]) {
    case 'x':
      base = 16;
      span = "_0123456789abcdefABCDEF";
      break;
    case 'o':
      base = 8;
      span = "_01234567";
      break;
    case 'b':
      base = 2;
      span = "_01";
      break;
    }
    if (base) {
      p += 2;
      p += strspn(p, span);
      int len = p - buffer;
      buffer[len] = 0;

      if (process_numstr(buffer + 2, base, &reason)) {
        return RETERROR(sp->ebuf, lineno, reason);
      }

      // use strtoll to obtain the value
      *tok = mktoken(sp, TOK_INTEGER);
      errno = 0;
      char *q;
      tok->u.int64 = strtoll(buffer + 2, &q, base);
      if (errno || *q || q == buffer + 2) {
        return RETERROR(sp->ebuf, lineno, "error parsing integer");
      }
      tok->str.len = len;
      sp->cur += len;
      return 0;
    }
  }

  // handle inf/nan
  if (*p == '+' || *p == '-') {
    p++;
  }
  if (*p == 'i' || *p == 'n') {
    return scan_float(sp, tok);
  }

  // regular int or float
  p = buffer;
  p += strspn(p, "0123456789_+-.eE");
  int len = p - buffer;
  buffer[len] = 0;

  if (process_numstr(buffer, 10, &reason)) {
    return RETERROR(sp->ebuf, lineno, reason);
  }

  *tok = mktoken(sp, TOK_INTEGER);
  errno = 0;
  char *q;
  tok->u.int64 = strtoll(buffer, &q, 10);
  if (errno || *q || q == buffer) {
    if (*q && strchr(".eE", *q)) {
      return scan_float(sp, tok); // try to fit a float
    }
    return RETERROR(sp->ebuf, lineno, "error parsing integer");
  }

  tok->str.len = len;
  sp->cur += len;
  return 0;
}

static int scan_bool(scanner_t *sp, token_t *tok) {
  char buffer[10];
  scan_copystr(sp, buffer, sizeof(buffer));

  int lineno = sp->lineno;
  bool val = false;
  const char *p = buffer;
  if (0 == strncmp(p, "true", 4)) {
    val = true;
    p += 4;
  } else if (0 == strncmp(p, "false", 5)) {
    val = false;
    p += 5;
  } else {
    return RETERROR(sp->ebuf, lineno, "invalid boolean value");
  }
  if (*p && !strchr("# \r\n\t,}]", *p)) {
    return RETERROR(sp->ebuf, lineno, "invalid boolean value");
  }

  int len = p - buffer;
  *tok = mktoken(sp, TOK_BOOL);
  tok->u.b1 = val;
  tok->str.len = len;
  sp->cur += len;
  return 0;
}

// Check if the next token may be TIME
static inline bool test_time(const char *p, const char *endp) {
  return &p[2] < endp && isdigit(p[0]) && isdigit(p[1]) && p[2] == ':';
}

// Check if the next token may be DATE
static inline bool test_date(const char *p, const char *endp) {
  return &p[4] < endp && isdigit(p[0]) && isdigit(p[1]) && isdigit(p[2]) &&
         isdigit(p[3]) && p[4] == '-';
}

// Check if the next token may be BOOL
static inline bool test_bool(const char *p, const char *endp) {
  return &p[0] < endp && (*p == 't' || *p == 'f');
}

// Check if the next token may be NUMBER
static bool test_number(const char *p, const char *endp) {
  if (&p[0] < endp && *p && strchr("0123456789+-._", *p)) {
    return true;
  }
  if (&p[3] < endp) {
    if (0 == memcmp(p, "nan", 3) || 0 == memcmp(p, "inf", 3)) {
      return true;
    }
  }
  return false;
}

// Scan a literal that is not a string
static int scan_nonstring_literal(scanner_t *sp, token_t *tok) {
  int lineno = sp->lineno;
  if (test_time(sp->cur, sp->endp)) {
    return scan_time(sp, tok);
  }

  if (test_date(sp->cur, sp->endp)) {
    return scan_timestamp(sp, tok);
  }

  if (test_bool(sp->cur, sp->endp)) {
    return scan_bool(sp, tok);
  }

  if (test_number(sp->cur, sp->endp)) {
    return scan_number(sp, tok);
  }
  return RETERROR(sp->ebuf, lineno, "invalid value");
}

// Scan a literal
static int scan_literal(scanner_t *sp, token_t *tok) {
  *tok = mktoken(sp, TOK_LIT);
  const char *p = sp->cur;
  while (p < sp->endp && (isalnum(*p) || *p == '_' || *p == '-')) {
    p++;
  }
  tok->str.len = p - tok->str.ptr;
  sp->cur = p;
  return 0;
}

// Save the current state of the scanner
static scanner_state_t scan_mark(scanner_t *sp) {
  scanner_state_t mark;
  mark.sp = sp;
  mark.cur = sp->cur;
  mark.lineno = sp->lineno;
  return mark;
}

// Restore the scanner state to a previously saved state
static void scan_restore(scanner_t *sp, scanner_state_t mark) {
  assert(mark.sp == sp);
  sp->cur = mark.cur;
  sp->lineno = mark.lineno;
}

// Return the next token
static int scan_next(scanner_t *sp, bool keymode, token_t *tok) {
again:
  *tok = mktoken(sp, TOK_FIN);

  int ch = S_GET();
  if (ch == TOK_FIN) {
    return 0;
  }

  tok->str.len = 1;
  switch (ch) {
  case '\n':
    tok->toktyp = TOK_ENDL;
    break;

  case ' ':
  case '\t':
    goto again; // skip whitespace

  case '#':
    // comment: skip until newline
    while (!S_MATCH('\n')) {
      ch = S_GET();
      if (ch == TOK_FIN)
        break;
      if ((0 <= ch && ch <= 0x8) || (0x0a <= ch && ch <= 0x1f) ||
          (ch == 0x7f)) {
        return RETERROR(sp->ebuf, sp->lineno, "bad control char in comment");
      }
    }
    goto again; // skip comment

  case '.':
    tok->toktyp = TOK_DOT;
    break;

  case '=':
    tok->toktyp = TOK_EQUAL;
    break;

  case ',':
    tok->toktyp = TOK_COMMA;
    break;

  case '[':
    tok->toktyp = TOK_LBRACK;
    if (keymode && S_MATCH('[')) {
      S_GET();
      tok->toktyp = TOK_LLBRACK;
      tok->str.len = 2;
    }
    break;

  case ']':
    tok->toktyp = TOK_RBRACK;
    if (keymode && S_MATCH(']')) {
      S_GET();
      tok->toktyp = TOK_RRBRACK;
      tok->str.len = 2;
    }
    break;

  case '{':
    tok->toktyp = TOK_LBRACE;
    break;

  case '}':
    tok->toktyp = TOK_RBRACE;
    break;

  case '"':
    sp->cur--;
    DO(scan_string(sp, tok));
    break;

  case '\'':
    sp->cur--;
    DO(scan_litstring(sp, tok));
    break;

  default:
    sp->cur--;
    DO(keymode ? scan_literal(sp, tok) : scan_nonstring_literal(sp, tok));
    break;
  }

  return 0;
}

// Check for stack overflow due to excessive number of brackets or braces
static int check_overflow(scanner_t *sp, token_t *tok) {
  switch (tok->toktyp) {
  case TOK_LBRACK:
    sp->bracket_level++;
    if (sp->bracket_level > BRACKET_LEVEL_MAX) {
      return RETERROR(sp->ebuf, sp->lineno, "stack overflow");
    }
    break;
  case TOK_RBRACK:
    sp->bracket_level--;
    break;
  case TOK_LBRACE:
    sp->brace_level++;
    if (sp->brace_level > BRACE_LEVEL_MAX) {
      return RETERROR(sp->ebuf, sp->lineno, "stack overflow");
    }
    break;
  case TOK_RBRACE:
    sp->brace_level--;
    break;
  default:
    break;
  }
  return 0;
}

static int scan_key(scanner_t *sp, token_t *tok) {
  if (sp->errmsg) {
    return -1;
  }
  if (scan_next(sp, true, tok) || check_overflow(sp, tok)) {
    sp->errmsg = sp->ebuf.ptr;
    return -1;
  }
  return 0;
}

static int scan_value(scanner_t *sp, token_t *tok) {
  if (sp->errmsg) {
    return -1;
  }
  if (scan_next(sp, false, tok) || check_overflow(sp, tok)) {
    sp->errmsg = sp->ebuf.ptr;
    return -1;
  }
  return 0;
}

/**
 * Convert a char in utf8 into UCS, and store it in *ret.
 * Return #bytes consumed or -1 on failure.
 */
static int utf8_to_ucs(const char *orig, int len, uint32_t *ret) {
  const unsigned char *buf = (const unsigned char *)orig;
  unsigned i = *buf++;
  uint32_t v;

  /* 0x00000000 - 0x0000007F:
     0xxxxxxx
  */
  if (0 == (i >> 7)) {
    if (len < 1)
      return -1;
    v = i;
    return *ret = v, 1;
  }
  /* 0x00000080 - 0x000007FF:
     110xxxxx 10xxxxxx
  */
  if (0x6 == (i >> 5)) {
    if (len < 2)
      return -1;
    v = i & 0x1f;
    for (int j = 0; j < 1; j++) {
      i = *buf++;
      if (0x2 != (i >> 6))
        return -1;
      v = (v << 6) | (i & 0x3f);
    }
    return *ret = v, (const char *)buf - orig;
  }
