  // descend the key from the toptab
  toml_datum_t *tab = &pp->toptab;
  for (int i = 0; i < keypart.nspan; i++) {
    span_t curkey = keypart.span[i];
    int j = tab_find(tab, curkey);
    if (j < 0) {
      // If not found: add a new (key,tab) pair
      const char *reason;
      toml_datum_t newtab = mkdatum(TOML_TABLE);
      newtab.flag |= FLAG_STDEXPR;
      if (tab_add(tab, curkey, newtab, &reason)) {
        return RETERROR(pp->ebuf, keylineno, "%s", reason);
      }
      tab = &tab->u.tab.value[tab->u.tab.size - 1];
      continue;
    }

    // Found: get the value
    toml_datum_t *value = &tab->u.tab.value[j];

    // If value is table, then point to that table and continue descent.
    if (value->type == TOML_TABLE) {
      tab = value;
      continue;
    }

    // If value is an array of table, point to the last element of the array and
    // continue descent.
    if (value->type == TOML_ARRAY) {
      if (value->flag & FLAG_INLINED) {
        return RETERROR(pp->ebuf, keylineno, "cannot expand array %s",
                        curkey.ptr);
      }
      if (value->u.arr.size <= 0) {
        return RETERROR(pp->ebuf, keylineno, "array %s has no elements",
                        curkey.ptr);
      }
      value = &value->u.arr.elem[value->u.arr.size - 1];
      if (value->type != TOML_TABLE) {
        return RETERROR(pp->ebuf, keylineno, "array %s must be array of tables",
                        curkey.ptr);
      }
      tab = value;
      continue;
    }

    // keypart not found
    return RETERROR(pp->ebuf, keylineno, "cannot locate table at key %s",
                    curkey.ptr);
  }

  // For the final keypart, make sure entry at key is an array of tables
  const char *reason;
  int idx = tab_find(tab, lastkeypart);
  if (idx == -1) {
    // If not found, add an array of table.
    if (tab_add(tab, lastkeypart, mkdatum(TOML_ARRAY), &reason)) {
      return RETERROR(pp->ebuf, keylineno, "%s", reason);
    }
    idx = tab_find(tab, lastkeypart);
    assert(idx >= 0);
  }
  // Check that this is an array.
  if (tab->u.tab.value[idx].type != TOML_ARRAY) {
    return RETERROR(pp->ebuf, keylineno, "entry must be an array");
  }
  // Add an empty table to the array
  toml_datum_t *arr = &tab->u.tab.value[idx];
  if (arr->flag & FLAG_INLINED) {
    return RETERROR(pp->ebuf, keylineno, "cannot extend a static array");
  }
  toml_datum_t *pelem = arr_emplace(arr, &reason);
  if (!pelem) {
    return RETERROR(pp->ebuf, keylineno, "%s", reason);
  }
  *pelem = mkdatum(TOML_TABLE);

  // Set the last element of this array as curtab of the parser
  pp->curtab = &arr->u.arr.elem[arr->u.arr.size - 1];
  assert(pp->curtab->type == TOML_TABLE);

  return 0;
}

// Parse an expression. A toml doc is just a list of expressions.
static int parse_keyvalue_expr(parser_t *pp, token_t tok) {
  // Obtain the key
  int keylineno = tok.lineno;
  keypart_t keypart;
  DO(parse_key(pp, tok, &keypart));

  // match the '='
  DO(scan_key(&pp->scanner, &tok));
  if (tok.toktyp != TOK_EQUAL) {
    return RETERROR(pp->ebuf, tok.lineno, "expect '='");
  }

  // Obtain the value
  toml_datum_t val;
  DO(scan_value(&pp->scanner, &tok));
  DO(parse_val(pp, tok, &val));

  // Locate the last table using keypart[]
  const char *reason;
  toml_datum_t *tab = pp->curtab;
  for (int i = 0; i < keypart.nspan - 1; i++) {
    int j = tab_find(tab, keypart.span[i]);
    if (j < 0) {
      if (i > 0 && (tab->flag & FLAG_EXPLICIT)) {
        return RETERROR(
            pp->ebuf, keylineno,
            "cannot extend a previously defined table using dotted expression");
      }
      toml_datum_t newtab = mkdatum(TOML_TABLE);
      if (tab_add(tab, keypart.span[i], newtab, &reason)) {
        return RETERROR(pp->ebuf, keylineno, "%s", reason);
      }
      tab = &tab->u.tab.value[tab->u.tab.size - 1];
      continue;
    }
    toml_datum_t *value = &tab->u.tab.value[j];
    if (value->type == TOML_TABLE) {
      tab = value;
      continue;
    }
    if (value->type == TOML_ARRAY) {
      return RETERROR(pp->ebuf, keylineno,
                      "encountered previously declared array '%s'",
                      keypart.span[i].ptr);
    }
    return RETERROR(pp->ebuf, keylineno, "cannot locate table at '%s'",
                    keypart.span[i].ptr);
  }

  // Check for disallowed situations.
  if (tab->flag & FLAG_INLINED) {
    return RETERROR(pp->ebuf, keylineno, "inline table cannot be extended");
  }
  if (keypart.nspan > 1 && (tab->flag & FLAG_EXPLICIT)) {
    return RETERROR(
        pp->ebuf, keylineno,
        "cannot extend a previously defined table using dotted expression");
  }

  // Add a new key/value for tab.
  if (tab_add(tab, keypart.span[keypart.nspan - 1], val, &reason)) {
    return RETERROR(pp->ebuf, keylineno, "%s", reason);
  }

  return 0;
}

// Normalize a LIT/STRING/MLSTRING/LITSTRING/MLLITSTRING
// -> unescape all escaped chars
// The returned string is allocated out of pp->sbuf[]
static int parse_norm(parser_t *pp, token_t tok, span_t *ret_span) {
  // Allocate a buffer to store the normalized string. Add one
  // extra-byte for terminating NUL.
  char *p = pool_alloc(pp->pool, tok.str.len + 1);
  if (!p) {
    return RETERROR(pp->ebuf, tok.lineno, "out of memory");
  }

  // Copy from token string into buffer
  memcpy(p, tok.str.ptr, tok.str.len);
  p[tok.str.len] = 0; // additional NUL term for safety

  ret_span->ptr = p;
  ret_span->len = tok.str.len;

  switch (tok.toktyp) {
  case TOK_LIT:
  case TOK_LITSTRING:
  case TOK_MLLITSTRING:
    // no need to handle escape chars
    return 0;

  case TOK_STRING:
  case TOK_MLSTRING:
    // need to handle escape chars
    break;

  default:
    return RETERROR(pp->ebuf, 0, "internal: arg must be a string");
  }

  // if there is no escape char, then done!
  if (!tok.u.escp) {
    return 0; // success
  }

  // p points to the backslash
  p += (tok.u.escp - tok.str.ptr);
  assert(p - ret_span->ptr == tok.u.escp - tok.str.ptr);
  assert(*p == '\\');

  // Normalize the escaped chars
  char *dst = p;
  while (*p) {
    if (*p != '\\') {
      *dst++ = *p++;
      continue;
    }
    switch (p[1]) {
    case '"':
    case '\\':
      *dst++ = p[1];
      p += 2;
      continue;
    case 'b':
      *dst++ = '\b';
      p += 2;
      continue;
    case 't':
      *dst++ = '\t';
      p += 2;
      continue;
    case 'n':
      *dst++ = '\n';
      p += 2;
      continue;
    case 'f':
      *dst++ = '\f';
      p += 2;
      continue;
    case 'r':
      *dst++ = '\r';
      p += 2;
      continue;
    case 'e':
      *dst++ = '\e';
      p += 2;
      continue;
    case 'x': {
      char buf[3];
      memcpy(buf, p + 2, 2);
      buf[2] = 0;
      int32_t ucs = strtol(buf, 0, 16);
      int n = ucs_to_utf8(ucs, dst);
      if (n < 0) {
        return RETERROR(pp->ebuf, tok.lineno, "error converting UCS %s to UTF8",
                        buf);
      }
      dst += n;
      p += 2 + 2; // \xNN
      continue;
    }
    case 'u':
    case 'U': {
      char buf[9];
      int sz = (p[1] == 'u' ? 4 : 8);
      memcpy(buf, p + 2, sz);
      buf[sz] = 0;
      int32_t ucs = strtol(buf, 0, 16);
      if (0xD800 <= ucs && ucs <= 0xDFFF) {
        // explicitly prohibit surrogates (non-scalar unicode code point)
        return RETERROR(pp->ebuf, tok.lineno, "invalid UTF8 char \\u%04x", ucs);
      }
      int n = ucs_to_utf8(ucs, dst);
      if (n < 0) {
        return RETERROR(pp->ebuf, tok.lineno, "error converting UCS %s to UTF8",
                        buf);
      }
      dst += n;
      p += 2 + sz; // \uNNNN or \UNNNNNNNN
      continue;
    }

    case ' ':
    case '\t':
    case '\r':
      // line-ending backslash
      // --- allow for extra whitespace chars after backslash
      // --- skip until newline
      p++;                     // skip the escape char
      p += strspn(p, " \t\r"); // skip whitespaces
      if (*p != '\n') {
        return RETERROR(pp->ebuf, tok.lineno,
                        "unexpected char after line-ending backslash");
      }
      // fallthru
    case '\n':
      // skip all whitespaces including newline
      p++;
      p += strspn(p, " \t\r\n");
      continue;
    default:
      *dst++ = *p++;
      continue;
    }
  }
  *dst = 0;
  ret_span->len = dst - ret_span->ptr;
  return 0;
}

// -------------- scanner functions

// Get the next char
static int scan_get(scanner_t *sp) {
  int ret = TOK_FIN;
  const char *p = sp->cur;
  if (p < sp->endp) {
    ret = *p++;
    if (ret == '\r' && p < sp->endp && *p == '\n') {
      ret = *p++;
    }
  }
  sp->cur = p;
  sp->lineno += (ret == '\n' ? 1 : 0);
  return ret;
}

// Check if the next char matches ch.
static inline bool scan_match(scanner_t *sp, int ch) {
  const char *p = sp->cur;
  // exact match? done.
  if (p < sp->endp && *p == ch) {
    return true;
  }
  // \n also matches \r\n
  if (ch == '\n' && p + 1 < sp->endp) {
    return p[0] == '\r' && p[1] == '\n';
  }
  // not a match
  return false;
}

// Check if the next char is in accept[].
static bool scan_matchany(scanner_t *sp, const char *accept) {
  for (; *accept; accept++) {
    if (scan_match(sp, *accept)) {
      return true;
    }
  }
  return false;
}

// Check if the next n chars match ch.
static inline bool scan_nmatch(scanner_t *sp, int ch, int n) {
  assert(ch != '\n'); // not handled
  if (sp->cur + n > sp->endp) {
    return false;
  }
  const char *p = sp->cur;
  int i;
  for (i = 0; i < n && p[i] == ch; i++)
    ;
  return i == n;
}

// Initialize a token.
static inline token_t mktoken(scanner_t *sp, toktyp_t typ) {
  token_t tok = {0};
  tok.toktyp = typ;
  tok.str.ptr = sp->cur;
  tok.lineno = sp->lineno;
  return tok;
}

#define S_GET() scan_get(sp)
#define S_MATCH(ch) scan_match(sp, (ch))
#define S_MATCH3(ch) scan_nmatch(sp, (ch), 3)
#define S_MATCH4(ch) scan_nmatch(sp, (ch), 4)
#define S_MATCH6(ch) scan_nmatch(sp, (ch), 6)

static inline bool is_valid_char(int ch) {
  // i.e. (0x20 <= ch && ch <= 0x7e) || (ch & 0x80);
  return isprint(ch) || (ch & 0x80);
}

static inline bool is_hex_char(int ch) {
  ch = toupper(ch);
  return ('0' <= ch && ch <= '9') || ('A' <= ch && ch <= 'F');
}

// Initialize a scanner
static void scan_init(scanner_t *sp, const char *src, int len, char *errbuf,
                      int errbufsz) {
  memset(sp, 0, sizeof(*sp));
  sp->src = src;
  sp->endp = src + len;
  assert(*sp->endp == '\0');
  sp->cur = src;
  sp->lineno = 1;
  sp->ebuf.ptr = errbuf;
  sp->ebuf.len = errbufsz;
}

static int scan_multiline_string(scanner_t *sp, token_t *tok) {
  assert(S_MATCH3('"'));
  S_GET(), S_GET(), S_GET(); // skip opening """

  // According to spec: trim first newline after """
  if (S_MATCH('\n')) {
    S_GET();
  }

  *tok = mktoken(sp, TOK_MLSTRING);
  // scan until terminating """
  const char *escp = NULL;
  while (1) {
    if (S_MATCH3('"')) {
      if (S_MATCH4('"')) {
        // special case... """abcd """" -> (abcd ")
        // but sequences of 3 or more double quotes are not allowed
        if (S_MATCH6('"')) {
          return RETERROR(sp->ebuf, sp->lineno,
                          "detected sequences of 3 or more double quotes");
        } else {
          ; // no problem
        }
      } else {
        break; // found terminating """
      }
    }
    int ch = S_GET();
    if (ch == TOK_FIN) {
      return RETERROR(sp->ebuf, sp->lineno, "unterminated \"\"\"");
    }
    // If non-escaped char ...
    if (ch != '\\') {
      if (!(is_valid_char(ch) || (ch && strchr(" \t\n", ch)))) {
        return RETERROR(sp->ebuf, sp->lineno, "invalid char in string");
      }
      continue;
    }
    // ch is backslash
    if (!escp) {
      escp = sp->cur - 1;
      assert(*escp == '\\');
    }

    // handle escape char
    ch = S_GET();
    if (ch && strchr("btnfre\"\\", ch)) {
      // skip \", \\, \b, \f, \n, \r, \t
      continue;
    }
    int top = 0;
    switch (ch) {
    case 'x':
      top = 2;
      break;
    case 'u':
      top = 4;
      break;
    case 'U':
      top = 8;
      break;
    default:
      break;
    }
    if (top) {
      for (int i = 0; i < top; i++) {
        if (!is_hex_char(S_GET())) {
          return RETERROR(sp->ebuf, sp->lineno,
                          "expect %d hex digits after \\%c", top, ch);
        }
      }
      continue;
    }
    // handle line-ending backslash
    if (ch == ' ' || ch == '\t') {
      // Although the spec does not allow for whitespace following a
      // line-ending backslash, some standard tests expect it.
      // Skip whitespace till EOL.
      while (ch != TOK_FIN && ch && strchr(" \t", ch)) {
        ch = S_GET();
      }
      if (ch != '\n') {
        // Got a backslash followed by whitespace, followed by some char
        // before newline
        return RETERROR(sp->ebuf, sp->lineno, "bad escape char in string");
      }
      // fallthru
    }
    if (ch == '\n') {
      // got a line-ending backslash
      // - skip all whitespaces
      while (scan_matchany(sp, " \t\n")) {
        S_GET();
      }
      continue;
    }
    return RETERROR(sp->ebuf, sp->lineno, "bad escape char in string");
  }
  tok->str.len = sp->cur - tok->str.ptr;
  tok->u.escp = escp;

  assert(S_MATCH3('"'));
  S_GET(), S_GET(), S_GET();
  return 0;
}

static int scan_string(scanner_t *sp, token_t *tok) {
  assert(S_MATCH('"'));
  if (S_MATCH3('"')) {
    return scan_multiline_string(sp, tok);
  }
  S_GET(); // skip opening "

  // scan until closing "
  *tok = mktoken(sp, TOK_STRING);
  const char *escp = NULL;
  while (!S_MATCH('"')) {
    int ch = S_GET();
    if (ch == TOK_FIN) {
      return RETERROR(sp->ebuf, sp->lineno, "unterminated string");
    }
    // If non-escaped char ...
    if (ch != '\\') {
      if (!(is_valid_char(ch) || ch == ' ' || ch == '\t')) {
        return RETERROR(sp->ebuf, sp->lineno, "invalid char in string");
      }
      continue;
    }
    // ch is backslash
    if (!escp) {
      escp = sp->cur - 1;
      assert(*escp == '\\');
    }

    // handle escape char
    ch = S_GET();
    if (ch && strchr("btnfre\"\\", ch)) {
      // skip \b, \t, \n, \f, \r, \e, \", \\  .
      continue;
    }
    int top = 0;
    switch (ch) {
    case 'x':
      top = 2;
      break;
    case 'u':
      top = 4;
      break;
    case 'U':
      top = 8;
      break;
    default:
      return RETERROR(sp->ebuf, sp->lineno, "bad escape char in string");
    }
    for (int i = 0; i < top; i++) {
      if (!is_hex_char(S_GET())) {
        return RETERROR(sp->ebuf, sp->lineno, "expect %d hex digits after \\%c",
                        top, ch);
      }
    }
  }
  tok->str.len = sp->cur - tok->str.ptr;
  tok->u.escp = escp;

  assert(S_MATCH('"'));
  S_GET(); // skip the terminating "
  return 0;
}

static int scan_multiline_litstring(scanner_t *sp, token_t *tok) {
  assert(S_MATCH3('\''));
  S_GET(), S_GET(), S_GET(); // skip opening '''

  // According to spec: trim first newline after '''
  if (S_MATCH('\n')) {
    S_GET();
  }

  // scan until terminating '''
  *tok = mktoken(sp, TOK_MLLITSTRING);
  while (1) {
    if (S_MATCH3('\'')) {
      if (S_MATCH4('\'')) {
        // special case... '''abcd '''' -> (abcd ')
        // but sequences of 3 or more single quotes are not allowed
        if (S_MATCH6('\'')) {
          return RETERROR(sp->ebuf, sp->lineno,
                          "sequences of 3 or more single quotes");
        } else {
          ; // no problem
        }
      } else {
        break; // found terminating '''
      }
    }
    int ch = S_GET();
    if (ch == TOK_FIN) {
      return RETERROR(sp->ebuf, sp->lineno,
                      "unterminated multiline lit string");
    }
    if (!(is_valid_char(ch) || (ch && strchr(" \t\n", ch)))) {
      return RETERROR(sp->ebuf, sp->lineno, "invalid char in string");
    }
  }
  tok->str.len = sp->cur - tok->str.ptr;

  assert(S_MATCH3('\''));
  S_GET(), S_GET(), S_GET();
  return 0;
}

static int scan_litstring(scanner_t *sp, token_t *tok) {
  assert(S_MATCH('\''));
  if (S_MATCH3('\'')) {
    return scan_multiline_litstring(sp, tok);
  }
  S_GET(); // skip opening '

  // scan until closing '
  *tok = mktoken(sp, TOK_LITSTRING);
  while (!S_MATCH('\'')) {
    int ch = S_GET();
    if (ch == TOK_FIN) {
      return RETERROR(sp->ebuf, sp->lineno, "unterminated string");
    }
    if (!(is_valid_char(ch) || ch == '\t')) {
      return RETERROR(sp->ebuf, sp->lineno, "invalid char in string");
    }
  }
  tok->str.len = sp->cur - tok->str.ptr;
  assert(S_MATCH('\''));
  S_GET();
  return 0;
}

static bool is_valid_date(int year, int month, int day) {
  if (!(1 <= year)) {
    return false;
  }
  if (!(1 <= month && month <= 12)) {
    return false;
  }
  int is_leap_year = (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
  int days_in_month[] = {
      31, 28 + is_leap_year, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  return (1 <= day && day <= days_in_month[month - 1]);
}

static bool is_valid_time(int hour, int minute, int sec, int usec) {
  if (!(0 <= hour && hour <= 23)) {
    return false;
  }
  if (!(0 <= minute && minute <= 59)) {
    return false;
  }
  if (!(0 <= sec && sec <= 59)) {
    return false;
  }
  if (!(0 <= usec)) {
    return false;
  }
  return true;
}

static bool is_valid_timezone(int minute) {
  minute = (minute < 0 ? -minute : minute);
  int hour = minute / 60;
  minute = minute % 60;
  if (!(0 <= hour && hour <= 23)) {
    return false;
  }
  if (!(0 <= minute && minute < 60)) {
    return false;
  }
  return true;
}

// Read an int (without signs) from the string p.
static int read_int(const char *p, int *ret) {
  const char *pp = p;
  int val = 0;
  for (; isdigit(*p); p++) {
    val = val * 10u + (*p - '0');
    if (val < 0) {
      return 0; // overflowed
    }
  }
  *ret = val;
  return p - pp;
}
