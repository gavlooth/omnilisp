/**
 *  Parse a toml document.
 */
toml_result_t toml_parse_file_ex(const char *fname) {
  toml_result_t result = {0};
  FILE *fp = fopen(fname, "r");
  if (!fp) {
    snprintf(result.errmsg, sizeof(result.errmsg), "fopen: %s", fname);
    return result;
  }
  result = toml_parse_file(fp);
  fclose(fp);
  return result;
}

/**
 *  Parse a toml document.
 */
toml_result_t toml_parse_file(FILE *fp) {
  toml_result_t result = {0};
  char *buf = 0;
  int top, max; // index into buf[]
  top = max = 0;

  // Read file into memory
  while (!feof(fp)) {
    assert(top <= max);
    if (top == max) {
      // need to extend buf[]
      int tmpmax = (max * 1.5) + 1000;
      if (tmpmax < 0) {
        // the real max is INT_MAX - 1 to account for terminating NUL.
        if (max < INT_MAX - 1) {
          tmpmax = INT_MAX - 1;
        } else {
          snprintf(result.errmsg, sizeof(result.errmsg),
                   "file is bigger than %d bytes", INT_MAX - 1);
          FREE(buf);
          return result;
        }
      }
      // add an extra byte for terminating NUL
      char *tmp = REALLOC(buf, tmpmax + 1);
      if (!tmp) {
        snprintf(result.errmsg, sizeof(result.errmsg), "out of memory");
        FREE(buf);
        return result;
      }
      buf = tmp;
      max = tmpmax;
    }

    errno = 0;
    top += fread(buf + top, 1, max - top, fp);
    if (ferror(fp)) {
      snprintf(result.errmsg, sizeof(result.errmsg), "%s",
               errno ? strerror(errno) : "Error reading file");
      FREE(buf);
      return result;
    }
  }
  buf[top] = 0; // NUL terminator

  result = toml_parse(buf, top);
  FREE(buf);
  return result;
}

/**
 *  Parse a toml document.
 */
toml_result_t toml_parse(const char *src, int len) {
  toml_result_t result = {0};
  parser_t parser = {0};
  parser_t *pp = &parser;

  // Check that src is NUL terminated.
  if (src[len]) {
    snprintf(result.errmsg, sizeof(result.errmsg),
             "src[] must be NUL terminated");
    goto bail;
  }

  // If user insists, check that src[] is a valid utf8 string.
  if (toml_option.check_utf8) {
    int line = 1; // keeps track of line number
    for (int i = 0; i < len;) {
      uint32_t ch;
      int n = utf8_to_ucs(src + i, len - i, &ch);
      if (n < 0) {
        snprintf(result.errmsg, sizeof(result.errmsg),
                 "invalid UTF8 char on line %d", line);
        goto bail;
      }
      if (0xD800 <= ch && ch <= 0xDFFF) {
        // explicitly prohibit surrogates (non-scalar unicode code point)
        snprintf(result.errmsg, sizeof(result.errmsg),
                 "invalid UTF8 char \\u%04x on line %d", ch, line);
        goto bail;
      }
      line += (ch == '\n' ? 1 : 0);
      i += n;
    }
  }

  // Initialize parser
  pp->toptab = mkdatum(TOML_TABLE);
  pp->curtab = &pp->toptab;
  pp->ebuf.ptr = result.errmsg; // parse error will be printed into pp->ebuf
  pp->ebuf.len = sizeof(result.errmsg);

  // Alloc memory pool
  pp->pool =
      pool_create(len + 10); // add some extra bytes for NUL term and safety
  if (!pp->pool) {
    snprintf(result.errmsg, sizeof(result.errmsg), "out of memory");
    goto bail;
  }

  // Initialize scanner. Scan error will be printed into pp->ebuf.
  scan_init(&pp->scanner, src, len, pp->ebuf.ptr, pp->ebuf.len);

  // Keep parsing until FIN
  for (;;) {
    token_t tok;
    if (scan_key(&pp->scanner, &tok)) {
      goto bail;
    }
    // break on FIN
    if (tok.toktyp == TOK_FIN) {
      break;
    }
    switch (tok.toktyp) {
    case TOK_ENDL: // skip blank lines
      continue;
    case TOK_LBRACK:
      if (parse_std_table_expr(pp, tok)) {
        goto bail;
      }
      break;
    case TOK_LLBRACK:
      if (parse_array_table_expr(pp, tok)) {
        goto bail;
      }
      break;
    default:
      // non-blank line: parse an expression
      if (parse_keyvalue_expr(pp, tok)) {
        goto bail;
      }
      break;
    }
    // each expression must be followed by newline
    if (scan_key(&pp->scanner, &tok)) {
      goto bail;
    }
    if (tok.toktyp == TOK_FIN || tok.toktyp == TOK_ENDL) {
      continue;
    }
    RETERROR(pp->ebuf, tok.lineno, "ENDL expected");
    goto bail;
  }

  // return result
  result.ok = true;
  result.toptab = pp->toptab;
  result.__internal = (void *)pp->pool;
  return result;

bail:
  // return error
  datum_free(&pp->toptab);
  pool_destroy(pp->pool);
  result.ok = false;
  assert(result.errmsg[0]); // make sure there is an errmsg
  return result;
}

// Convert a (LITSTRING, LIT, MLLITSTRING, MLSTRING, or STRING) token to a
// datum.
static int token_to_string(parser_t *pp, token_t tok, toml_datum_t *ret) {
  *ret = mkdatum(TOML_STRING);
  span_t span;
  DO(parse_norm(pp, tok, &span));
  ret->u.str.ptr = (char *)span.ptr;
  ret->u.str.len = span.len;
  return 0;
}

// Convert TIME token to a datum.
static int token_to_time(parser_t *pp, token_t tok, toml_datum_t *ret) {
  (void)pp;
  *ret = mkdatum(TOML_TIME);
  ret->u.ts.hour = tok.u.tsval.hour;
  ret->u.ts.minute = tok.u.tsval.minute;
  ret->u.ts.second = tok.u.tsval.sec;
  ret->u.ts.usec = tok.u.tsval.usec;
  return 0;
}

// Convert a DATE token to a datum.
static int token_to_date(parser_t *pp, token_t tok, toml_datum_t *ret) {
  (void)pp;
  *ret = mkdatum(TOML_DATE);
  ret->u.ts.year = tok.u.tsval.year;
  ret->u.ts.month = tok.u.tsval.month;
  ret->u.ts.day = tok.u.tsval.day;
  return 0;
}

// Convert a DATETIME token to a datum.
static int token_to_datetime(parser_t *pp, token_t tok, toml_datum_t *ret) {
  (void)pp;
  *ret = mkdatum(TOML_DATETIME);
  ret->u.ts.year = tok.u.tsval.year;
  ret->u.ts.month = tok.u.tsval.month;
  ret->u.ts.day = tok.u.tsval.day;
  ret->u.ts.hour = tok.u.tsval.hour;
  ret->u.ts.minute = tok.u.tsval.minute;
  ret->u.ts.second = tok.u.tsval.sec;
  ret->u.ts.usec = tok.u.tsval.usec;
  return 0;
}

// Convert a DATETIMETZ token to a datum.
static int token_to_datetimetz(parser_t *pp, token_t tok, toml_datum_t *ret) {
  (void)pp;
  *ret = mkdatum(TOML_DATETIMETZ);
  ret->u.ts.year = tok.u.tsval.year;
  ret->u.ts.month = tok.u.tsval.month;
  ret->u.ts.day = tok.u.tsval.day;
  ret->u.ts.hour = tok.u.tsval.hour;
  ret->u.ts.minute = tok.u.tsval.minute;
  ret->u.ts.second = tok.u.tsval.sec;
  ret->u.ts.usec = tok.u.tsval.usec;
  ret->u.ts.tz = tok.u.tsval.tz;
  return 0;
}

// Convert an int64 token to a datum.
static int token_to_int64(parser_t *pp, token_t tok, toml_datum_t *ret) {
  (void)pp;
  assert(tok.toktyp == TOK_INTEGER);
  *ret = mkdatum(TOML_INT64);
  ret->u.int64 = tok.u.int64;
  return 0;
}

// Convert a fp64 token to a datum.
static int token_to_fp64(parser_t *pp, token_t tok, toml_datum_t *ret) {
  (void)pp;
  assert(tok.toktyp == TOK_FLOAT);
  *ret = mkdatum(TOML_FP64);
  ret->u.fp64 = tok.u.fp64;
  return 0;
}

// Convert a boolean token to a datum.
static int token_to_boolean(parser_t *pp, token_t tok, toml_datum_t *ret) {
  (void)pp;
  assert(tok.toktyp == TOK_BOOL);
  *ret = mkdatum(TOML_BOOLEAN);
  ret->u.boolean = tok.u.b1;
  return 0;
}

// Parse a multipart key. Return 0 on success, -1 otherwise.
static int parse_key(parser_t *pp, token_t tok, keypart_t *ret_keypart) {
  ret_keypart->nspan = 0;
  // key = simple-key | dotted_key
  // simple-key = STRING | LITSTRING | LIT
  // dotted-key = simple-key (DOT simple-key)+
  if (tok.toktyp != TOK_STRING && tok.toktyp != TOK_LITSTRING &&
      tok.toktyp != TOK_LIT) {
    return RETERROR(pp->ebuf, tok.lineno, "missing key");
  }

  int n = 0;
  span_t *kpspan = ret_keypart->span;

  // Normalize the first keypart
  if (parse_norm(pp, tok, &kpspan[n])) {
    return RETERROR(pp->ebuf, tok.lineno,
                    "unable to normalize string; probably a unicode issue");
  }
  n++;

  // Scan and normalize the second to last keypart
  while (1) {
    scanner_state_t mark = scan_mark(&pp->scanner);

    // Eat the dot if it is there
    DO(scan_key(&pp->scanner, &tok));

    // If not a dot, we are done with keyparts.
    if (tok.toktyp != TOK_DOT) {
      scan_restore(&pp->scanner, mark);
      break;
    }

    // Scan the n-th key
    DO(scan_key(&pp->scanner, &tok));

    if (tok.toktyp != TOK_STRING && tok.toktyp != TOK_LITSTRING &&
        tok.toktyp != TOK_LIT) {
      return RETERROR(pp->ebuf, tok.lineno, "expects a string in dotted-key");
    }

    if (n >= KEYPARTMAX) {
      return RETERROR(pp->ebuf, tok.lineno, "too many key parts");
    }

    // Normalize the n-th key.
    DO(parse_norm(pp, tok, &kpspan[n]));
    n++;
  }

  // This key has n parts.
  ret_keypart->nspan = n;
  return 0;
}

// Starting at toptab, descend following keypart[]. If a key does not
// exist in the current table, create a new table entry for the
// key. Returns the final table represented by the key.
static toml_datum_t *descend_keypart(parser_t *pp, int lineno,
                                     toml_datum_t *toptab, keypart_t *keypart,
                                     bool stdtabexpr) {
  toml_datum_t *tab = toptab; // current tab

  for (int i = 0; i < keypart->nspan; i++) {
    const char *reason;
    // Find the i-th keypart
    int j = tab_find(tab, keypart->span[i]);
    // Not found: add a new (key, tab) pair.
    if (j < 0) {
      toml_datum_t newtab = mkdatum(TOML_TABLE);
      newtab.flag |= stdtabexpr ? FLAG_STDEXPR : 0;
      if (tab_add(tab, keypart->span[i], newtab, &reason)) {
        RETERROR(pp->ebuf, lineno, "%s", reason);
        return NULL;
      }
      tab = &tab->u.tab.value[tab->u.tab.size - 1]; // descend
      continue;
    }

    // Found: extract the value of the key.
    toml_datum_t *value = &tab->u.tab.value[j];

    // If the value is a table, descend.
    if (value->type == TOML_TABLE) {
      tab = value; // descend
      continue;
    }

    // If the value is an array: locate the last entry and descend.
    if (value->type == TOML_ARRAY) {
      // If empty: error.
      if (value->u.arr.size <= 0) {
        RETERROR(pp->ebuf, lineno, "array %s has no elements",
                 keypart->span[i].ptr);
        return NULL;
      }

      // Extract the last element of the array.
      value = &value->u.arr.elem[value->u.arr.size - 1];

      // It must be a table!
      if (value->type != TOML_TABLE) {
        RETERROR(pp->ebuf, lineno, "array %s must be array of tables",
                 keypart->span[i].ptr);
        return NULL;
      }
      tab = value; // descend
      continue;
    }

    // key not found
    RETERROR(pp->ebuf, lineno, "cannot locate table at key %s",
             keypart->span[i].ptr);
    return NULL;
  }

  // Return the table corresponding to the keypart[].
  return tab;
}

// Recursively set flags on datum
static void set_flag_recursive(toml_datum_t *datum, uint32_t flag) {
  datum->flag |= flag;
  switch (datum->type) {
  case TOML_ARRAY:
    for (int i = 0, top = datum->u.arr.size; i < top; i++) {
      set_flag_recursive(&datum->u.arr.elem[i], flag);
    }
    break;
  case TOML_TABLE:
    for (int i = 0, top = datum->u.tab.size; i < top; i++) {
      set_flag_recursive(&datum->u.tab.value[i], flag);
    }
    break;
  default:
    break;
  }
}

// Parse an inline array.
static int parse_inline_array(parser_t *pp, token_t tok,
                              toml_datum_t *ret_datum) {
  assert(tok.toktyp == TOK_LBRACK);
  *ret_datum = mkdatum(TOML_ARRAY);
  int need_comma = 0;

  // loop until RBRACK
  for (;;) {
    // skip ENDL
    do {
      DO(scan_value(&pp->scanner, &tok));
    } while (tok.toktyp == TOK_ENDL);

    // If got an RBRACK: done!
    if (tok.toktyp == TOK_RBRACK) {
      break;
    }

    // If got a COMMA: check if it is expected.
    if (tok.toktyp == TOK_COMMA) {
      if (need_comma) {
        need_comma = 0;
        continue;
      }
      return RETERROR(pp->ebuf, tok.lineno,
                      "syntax error while parsing array: unexpected comma");
    }

    // Not a comma, but need a comma: error!
    if (need_comma) {
      return RETERROR(pp->ebuf, tok.lineno,
                      "syntax error while parsing array: missing comma");
    }

    // This is a valid value!

    // Add the value to the array.
    const char *reason;
    toml_datum_t *pelem = arr_emplace(ret_datum, &reason);
    if (!pelem) {
      return RETERROR(pp->ebuf, tok.lineno, "while parsing array: %s", reason);
    }

    // Parse the value and save into array.
    DO(parse_val(pp, tok, pelem));

    // Need comma before the next value.
    need_comma = 1;
  }

  // Set the INLINE flag for all things in this array.
  set_flag_recursive(ret_datum, FLAG_INLINED);
  return 0;
}

// Parse an inline table.
static int parse_inline_table(parser_t *pp, token_t tok,
                              toml_datum_t *ret_datum) {
  assert(tok.toktyp == TOK_LBRACE);
  *ret_datum = mkdatum(TOML_TABLE);
  bool need_comma = 0;
  bool was_comma = 0;

  // loop until RBRACE
  for (;;) {
    DO(scan_key(&pp->scanner, &tok));

    // Got an RBRACE: done!
    if (tok.toktyp == TOK_RBRACE) {
      if (was_comma) {
        /*
        return RETERROR(pp->ebuf, tok.lineno,
                        "extra comma before closing brace");
        */
        // extra comma before RBRACE is allowed for v1.1
        (void)0;
      }
      break;
    }

    // Got a comma: check if it is expected.
    if (tok.toktyp == TOK_COMMA) {
      if (need_comma) {
        need_comma = 0, was_comma = 1;
        continue;
      }
      return RETERROR(pp->ebuf, tok.lineno, "unexpected comma");
    }

    // Newline not allowed in inline table.
    // newline is allowed in v1.1
    if (tok.toktyp == TOK_ENDL) {
      // return RETERROR(pp->ebuf, tok.lineno, "unexpected newline");
      continue;
    }

    // Not a comma, but need a comma: error!
    if (need_comma) {
      return RETERROR(pp->ebuf, tok.lineno, "missing comma");
    }

    // Get the keyparts
    keypart_t keypart = {0};
    int keylineno = tok.lineno;
    DO(parse_key(pp, tok, &keypart));

    // Descend to one keypart before last
    span_t lastkeypart = keypart.span[--keypart.nspan];
    toml_datum_t *tab =
        descend_keypart(pp, keylineno, ret_datum, &keypart, false);
    if (!tab) {
      return -1;
    }

    // If tab is a previously declared inline table: error.
    if (tab->flag & FLAG_INLINED) {
      return RETERROR(pp->ebuf, tok.lineno, "inline table cannot be extended");
    }

    // We are explicitly defining it now.
    tab->flag |= FLAG_EXPLICIT;

    // match EQUAL
    DO(scan_value(&pp->scanner, &tok));

    if (tok.toktyp != TOK_EQUAL) {
      if (tok.toktyp == TOK_ENDL) {
        return RETERROR(pp->ebuf, tok.lineno, "unexpected newline");
      } else {
        return RETERROR(pp->ebuf, tok.lineno, "missing '='");
      }
    }

    // obtain the value
    toml_datum_t value;
    DO(scan_value(&pp->scanner, &tok));
    DO(parse_val(pp, tok, &value));

    // Add the value to tab.
    const char *reason;
    if (tab_add(tab, lastkeypart, value, &reason)) {
      return RETERROR(pp->ebuf, tok.lineno, "%s", reason);
    }
    need_comma = 1, was_comma = 0;
  }

  set_flag_recursive(ret_datum, FLAG_INLINED);
  return 0;
}

// Parse a value.
static int parse_val(parser_t *pp, token_t tok, toml_datum_t *ret) {
  // val = string / boolean / array / inline-table / date-time / float / integer
  switch (tok.toktyp) {
  case TOK_STRING:
  case TOK_MLSTRING:
  case TOK_LITSTRING:
  case TOK_MLLITSTRING:
    return token_to_string(pp, tok, ret);
  case TOK_TIME:
    return token_to_time(pp, tok, ret);
  case TOK_DATE:
    return token_to_date(pp, tok, ret);
  case TOK_DATETIME:
    return token_to_datetime(pp, tok, ret);
  case TOK_DATETIMETZ:
    return token_to_datetimetz(pp, tok, ret);
  case TOK_INTEGER:
    return token_to_int64(pp, tok, ret);
  case TOK_FLOAT:
    return token_to_fp64(pp, tok, ret);
  case TOK_BOOL:
    return token_to_boolean(pp, tok, ret);
  case TOK_LBRACK: // inline-array
    return parse_inline_array(pp, tok, ret);
  case TOK_LBRACE: // inline-table
    return parse_inline_table(pp, tok, ret);
  default:
    break;
  }
  return RETERROR(pp->ebuf, tok.lineno, "missing value");
}

// Parse a standard table expression, and set the curtab of the parser
// to the table referenced.  A standard table expression is a line
// like [a.b.c.d].
static int parse_std_table_expr(parser_t *pp, token_t tok) {
  // std-table = [ key ]
  // Eat the [
  assert(tok.toktyp == TOK_LBRACK); // [ ate by caller

  // Read the first keypart
  DO(scan_key(&pp->scanner, &tok));

  // Extract the keypart[]
  int keylineno = tok.lineno;
  keypart_t keypart;
  DO(parse_key(pp, tok, &keypart));

  // Eat the ]
  DO(scan_key(&pp->scanner, &tok));
  if (tok.toktyp != TOK_RBRACK) {
    return RETERROR(pp->ebuf, tok.lineno, "missing right-bracket");
  }

  // Descend to one keypart before last.
  span_t lastkeypart = keypart.span[--keypart.nspan];

  // Descend keypart from the toptab.
  toml_datum_t *tab =
      descend_keypart(pp, keylineno, &pp->toptab, &keypart, true);
  if (!tab) {
    return -1;
  }

  // Look for the last keypart in the final tab
  int j = tab_find(tab, lastkeypart);
  if (j < 0) {
    // If not found: add it.
    if (tab->flag & FLAG_INLINED) {
      return RETERROR(pp->ebuf, keylineno, "inline table cannot be extended");
    }
    const char *reason;
    toml_datum_t newtab = mkdatum(TOML_TABLE);
    newtab.flag |= FLAG_STDEXPR;
    if (tab_add(tab, lastkeypart, newtab, &reason)) {
      return RETERROR(pp->ebuf, keylineno, "%s", reason);
    }
    // this is the new tab
    tab = &tab->u.tab.value[tab->u.tab.size - 1];
  } else {
    // Found: check for errors
    tab = &tab->u.tab.value[j];
    if (tab->flag & FLAG_EXPLICIT) {
      /*
        This is not OK:
        [x.y.z]
        [x.y.z]

        but this is OK:
        [x.y.z]
        [x]
      */
      return RETERROR(pp->ebuf, keylineno, "table defined more than once");
    }
    if (!(tab->flag & FLAG_STDEXPR)) {
      /*
      [t1]			# OK
      t2.t3.v = 0		# OK
      [t1.t2]   		# should FAIL  - t2 was non-explicit but was not
      created by std-table-expr
      */
      return RETERROR(pp->ebuf, keylineno, "table defined before");
    }
  }

  // Set explicit flag on tab
  tab->flag |= FLAG_EXPLICIT;

  // Set tab as curtab of the parser
  pp->curtab = tab;
  return 0;
}

// Parse an array table expression, and set the curtab of the parser
// to the table referenced. A standard array table expresison is a line
// like [[a.b.c.d]].
static int parse_array_table_expr(parser_t *pp, token_t tok) {
  // array-table = [[ key ]]
  assert(tok.toktyp == TOK_LLBRACK); // [[ ate by caller

  // Read the first keypart
  DO(scan_key(&pp->scanner, &tok));

  int keylineno = tok.lineno;
  keypart_t keypart;
  DO(parse_key(pp, tok, &keypart));

  // eat the ]]
  token_t rrb;
  DO(scan_key(&pp->scanner, &rrb));
  if (rrb.toktyp != TOK_RRBRACK) {
    return RETERROR(pp->ebuf, rrb.lineno, "missing ']]'");
  }

  // remove the last keypart from keypart[]
  span_t lastkeypart = keypart.span[--keypart.nspan];
