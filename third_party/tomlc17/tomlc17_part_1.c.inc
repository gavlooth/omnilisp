/* Copyright (c) 2024-2026, CK Tan.
 * https://github.com/cktan/tomlc17/blob/main/LICENSE
 */
#include "tomlc17.h"
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const toml_datum_t DATUM_ZERO = {0};

static toml_option_t toml_option = {0, realloc, free};

#define MALLOC(n) toml_option.mem_realloc(0, n)
#define REALLOC(p, n) toml_option.mem_realloc(p, n)
#define FREE(p) toml_option.mem_free(p)

#define DO(x)                                                                  \
  if (x)                                                                       \
    return -1;                                                                 \
  else                                                                         \
    (void)0

// Copy string src to dst where dst is limited to dstsz that includes
// NUL. Return 0 on success, -1 otherwise (because src[] is longer than dst[]).
static inline int copystring(char *dst, int dstsz, const char *src) {
  int srcsz = strlen(src) + 1;
  if (srcsz > dstsz) {
    return -1;
  }
  memcpy(dst, src, srcsz);
  return 0;
}

/*
 *  Error buffer
 */
typedef struct ebuf_t ebuf_t;
struct ebuf_t {
  char *ptr;
  int len;
};

/*
 *  Format an error into ebuf[]. Always return -1.
 */
static int RETERROR(ebuf_t ebuf, int lineno, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  char *p = ebuf.ptr;
  char *q = p + ebuf.len;
  if (lineno) {
    snprintf(p, q - p, "(line %d) ", lineno);
    p += strlen(p);
  }
  vsnprintf(p, q - p, fmt, args);
  return -1;
}

/*
 *  Memory pool. Allocated a big block once and hand out piecemeal.
 */
typedef struct pool_t pool_t;
struct pool_t {
  int top, max;
  char buf[1]; // first byte starts here
};

/**
 *  Create a memory pool of N bytes. Return the memory pool on
 *  success, or NULL if out of memory.
 */
static pool_t *pool_create(int N) {
  if (N <= 0) {
    N = 100; // minimum
  }
  int totalsz = sizeof(pool_t) + N;
  pool_t *pool = MALLOC(totalsz);
  if (!pool) {
    return NULL;
  }
  memset(pool, 0, totalsz);
  pool->max = N;
  return pool;
}

/**
 *  Destroy a memory pool.
 */
static void pool_destroy(pool_t *pool) { FREE(pool); }

/**
 *  Allocate n bytes from pool. Return the memory allocated on
 *  success, or NULL if out of memory.
 */
static char *pool_alloc(pool_t *pool, int n) {
  if (pool->top + n > pool->max) {
    return NULL;
  }
  char *ret = pool->buf + pool->top;
  pool->top += n;
  return ret;
}

/* This is a string view. */
typedef struct span_t span_t;
struct span_t {
  const char *ptr;
  int len;
};

/* Represents a multi-part key */
#define KEYPARTMAX 10
typedef struct keypart_t keypart_t;
struct keypart_t {
  int nspan;
  span_t span[KEYPARTMAX];
};

static int utf8_to_ucs(const char *s, int len, uint32_t *ret);
static int ucs_to_utf8(uint32_t code, char buf[4]);

// flags for toml_datum_t::flag.
#define FLAG_INLINED 1
#define FLAG_STDEXPR 2
#define FLAG_EXPLICIT 4

// Maximum levels of brackets and braces to prevent
// stack overflow during recursive descent of the parser.
#define BRACKET_LEVEL_MAX 30
#define BRACE_LEVEL_MAX 30

static inline size_t align8(size_t x) { return (((x) + 7) & ~7); }

enum toktyp_t {
  TOK_DOT = 1,
  TOK_EQUAL,
  TOK_COMMA,
  TOK_LBRACK,
  TOK_LLBRACK,
  TOK_RBRACK,
  TOK_RRBRACK,
  TOK_LBRACE,
  TOK_RBRACE,
  TOK_LIT,
  TOK_STRING,
  TOK_MLSTRING,
  TOK_LITSTRING,
  TOK_MLLITSTRING,
  TOK_TIME,
  TOK_DATE,
  TOK_DATETIME,
  TOK_DATETIMETZ,
  TOK_INTEGER,
  TOK_FLOAT,
  TOK_BOOL,
  TOK_ENDL,
  TOK_FIN = -5000, // EOF
};
typedef enum toktyp_t toktyp_t;
typedef struct scanner_t scanner_t;

/* Remember the current state of a scanner */
typedef struct scanner_state_t scanner_state_t;
struct scanner_state_t {
  scanner_t *sp;
  const char *cur; // points into scanner_t::src[]
  int lineno;      // current line number
};

// A scan token
typedef struct token_t token_t;
struct token_t {
  toktyp_t toktyp;
  int lineno;
  span_t str;

  // values represented by str
  union {
    const char *escp; // point to an esc char in str
    int64_t int64;
    double fp64;
    bool b1;
    struct {
      // validity depends on toktyp for TIME, DATE, DATETIME, DATETIMETZ
      int year, month, day, hour, minute, sec, usec;
      int tz; // +- minutes
    } tsval;
  } u;
};

// Scanner object
struct scanner_t {
  const char *src;  // src[] is a NUL-terminated string
  const char *endp; // end of src[]. always pointing at a NUL char.
  const char *cur;  // current char in src[]
  int lineno;       // line number of current char
  char *errmsg;     // point to errbuf if there was an error
  ebuf_t ebuf;

  int bracket_level; // count depth of [ ]
  int brace_level;   // count depth of { }
};
static void scan_init(scanner_t *sp, const char *src, int len, char *errbuf,
                      int errbufsz);
static int scan_key(scanner_t *sp, token_t *tok);
static int scan_value(scanner_t *sp, token_t *tok);
// restore scanner to state before tok was returned
static scanner_state_t scan_mark(scanner_t *sp);
static void scan_restore(scanner_t *sp, scanner_state_t state);

#ifndef min
static inline int min(int a, int b) { return a < b ? a : b; }
#endif

// Copy up to dstsz - 1 chars from the current position of the scanner
// to dst.
static void scan_copystr(scanner_t *sp, char *dst, int dstsz) {
  assert(dstsz > 0);
  int len = min(sp->endp - sp->cur, dstsz - 1); // account for NUL
  if (len > 0) {
    memcpy(dst, sp->cur, len);
    dst[len] = 0;
  }
}

// Parser object
typedef struct parser_t parser_t;
struct parser_t {
  scanner_t scanner;
  toml_datum_t toptab;  // top table
  toml_datum_t *curtab; // current table
  pool_t *pool;         // memory pool for strings
  ebuf_t ebuf;
};

// Put key into tab dictionary. Return a place to
// the datum for the key on success, or NULL otherwise.
static toml_datum_t *tab_emplace(toml_datum_t *tab, span_t key,
                                 const char **reason) {
  assert(tab->type == TOML_TABLE);
  int N = tab->u.tab.size;
  for (int i = 0; i < N; i++) {
    if (tab->u.tab.len[i] == key.len &&
        0 == memcmp(tab->u.tab.key[i], key.ptr, key.len)) {
      return &tab->u.tab.value[i];
    }
  }
  // Expand pkey[], plen[] and value[]
  {
    char **pkey = REALLOC(tab->u.tab.key, sizeof(*pkey) * align8(N + 1));
    if (!pkey) {
      *reason = "out of memory";
      return NULL;
    }
    tab->u.tab.key = (const char **)pkey;
  }

  {
    int *plen = REALLOC(tab->u.tab.len, sizeof(*plen) * align8(N + 1));
    if (!plen) {
      *reason = "out of memory";
      return NULL;
    }
    tab->u.tab.len = plen;
  }

  {
    toml_datum_t *value =
        REALLOC(tab->u.tab.value, sizeof(*value) * align8(N + 1));
    if (!value) {
      *reason = "out of memory";
      return NULL;
    }
    tab->u.tab.value = value;
  }

  // Append the new key/value
  tab->u.tab.size = N + 1;
  tab->u.tab.key[N] = (char *)key.ptr;
  tab->u.tab.len[N] = key.len;
  tab->u.tab.value[N] = DATUM_ZERO;
  return &tab->u.tab.value[N];
}

// Find key in tab and return its index. If not found, return -1.
static int tab_find(toml_datum_t *tab, span_t key) {
  assert(tab->type == TOML_TABLE);
  for (int i = 0, top = tab->u.tab.size; i < top; i++) {
    if (tab->u.tab.len[i] == key.len &&
        0 == memcmp(tab->u.tab.key[i], key.ptr, key.len)) {
      return i;
    }
  }
  return -1;
}

// Add a new key in tab. Return 0 on success, -1 otherwise.
// On error, *reason will point to an error message.
static int tab_add(toml_datum_t *tab, span_t newkey, toml_datum_t newvalue,
                   const char **reason) {
  assert(tab->type == TOML_TABLE);
  if (-1 != tab_find(tab, newkey)) {
    *reason = "duplicate key";
    return -1;
  }
  toml_datum_t *pvalue = tab_emplace(tab, newkey, reason);
  if (!pvalue) {
    return -1;
  }
  *pvalue = newvalue;
  return 0;
}

// Add a new element into an array. Return 0 on success, -1 otherwise.
// On error, *reason will point to an error message.
static toml_datum_t *arr_emplace(toml_datum_t *arr, const char **reason) {
  assert(arr->type == TOML_ARRAY);
  int n = arr->u.arr.size;
  toml_datum_t *elem = REALLOC(arr->u.arr.elem, sizeof(*elem) * align8(n + 1));
  if (!elem) {
    *reason = "out of memory";
    return NULL;
  }
  arr->u.arr.elem = elem;
  arr->u.arr.size = n + 1;
  elem[n] = DATUM_ZERO;
  return &elem[n];
}

// ------------------- parser section
static int parse_norm(parser_t *pp, token_t tok, span_t *ret_span);
static int parse_val(parser_t *pp, token_t tok, toml_datum_t *ret);
static int parse_keyvalue_expr(parser_t *pp, token_t tok);
static int parse_std_table_expr(parser_t *pp, token_t tok);
static int parse_array_table_expr(parser_t *pp, token_t tok);

static toml_datum_t mkdatum(toml_type_t ty) {
  toml_datum_t ret = {0};
  ret.type = ty;
  if (ty == TOML_DATE || ty == TOML_TIME || ty == TOML_DATETIME ||
      ty == TOML_DATETIMETZ) {
    ret.u.ts.year = -1;
    ret.u.ts.month = -1;
    ret.u.ts.day = -1;
    ret.u.ts.hour = -1;
    ret.u.ts.minute = -1;
    ret.u.ts.second = -1;
    ret.u.ts.usec = -1;
    ret.u.ts.tz = -1;
  }
  return ret;
}

// Recursively free any dynamically allocated memory in the datum tree
static void datum_free(toml_datum_t *datum) {
  if (datum->type == TOML_TABLE) {
    for (int i = 0, top = datum->u.tab.size; i < top; i++) {
      datum_free(&datum->u.tab.value[i]);
    }
    FREE(datum->u.tab.key);
    FREE(datum->u.tab.len);
    FREE(datum->u.tab.value);
  } else if (datum->type == TOML_ARRAY) {
    for (int i = 0, top = datum->u.arr.size; i < top; i++) {
      datum_free(&datum->u.arr.elem[i]);
    }
    FREE(datum->u.arr.elem);
  }
  // other types do not allocate memory
  *datum = DATUM_ZERO;
}

// Make a deep copy of src to dst.
// Return 0 on success, -1 otherwise.
static int datum_copy(toml_datum_t *dst, toml_datum_t src, pool_t *pool,
                      const char **reason) {
  *dst = mkdatum(src.type);
  switch (src.type) {
  case TOML_STRING:
    dst->u.str.ptr = pool_alloc(pool, src.u.str.len + 1);
    if (!dst->u.str.ptr) {
      *reason = "out of memory";
      goto bail;
    }
    dst->u.str.len = src.u.str.len;
    memcpy((char *)dst->u.str.ptr, src.u.str.ptr, src.u.str.len + 1);
    break;
  case TOML_TABLE:
    for (int i = 0; i < src.u.tab.size; i++) {
      span_t newkey = {src.u.tab.key[i], src.u.tab.len[i]};
      toml_datum_t *pvalue = tab_emplace(dst, newkey, reason);
      if (!pvalue) {
        goto bail;
      }
      if (datum_copy(pvalue, src.u.tab.value[i], pool, reason)) {
        goto bail;
      }
    }
    break;
  case TOML_ARRAY:
    for (int i = 0; i < src.u.arr.size; i++) {
      toml_datum_t *pelem = arr_emplace(dst, reason);
      if (!pelem) {
        goto bail;
      }
      if (datum_copy(pelem, src.u.arr.elem[i], pool, reason)) {
        goto bail;
      }
    }
    break;
  default:
    *dst = src;
    break;
  }

  return 0;

bail:
  datum_free(dst);
  return -1;
}

// Check if datum is an array of tables.
static inline bool is_array_of_tables(toml_datum_t datum) {
  bool ret = (datum.type == TOML_ARRAY);
  for (int i = 0; ret && i < datum.u.arr.size; i++) {
    ret = (datum.u.arr.elem[i].type == TOML_TABLE);
  }
  return ret;
}

// Merge src into dst. Return 0 on success, -1 otherwise.
static int datum_merge(toml_datum_t *dst, toml_datum_t src, pool_t *pool,
                       const char **reason) {
  if (dst->type != src.type) {
    datum_free(dst);
    return datum_copy(dst, src, pool, reason);
  }
  switch (src.type) {
  case TOML_TABLE:
    // for key-value in src:
    //    override key-value in dst.
    for (int i = 0; i < src.u.tab.size; i++) {
      span_t key;
      key.ptr = src.u.tab.key[i];
      key.len = src.u.tab.len[i];
      toml_datum_t *pvalue = tab_emplace(dst, key, reason);
      if (!pvalue) {
        return -1;
      }
      if (pvalue->type) {
        DO(datum_merge(pvalue, src.u.tab.value[i], pool, reason));
      } else {
        datum_free(pvalue);
        DO(datum_copy(pvalue, src.u.tab.value[i], pool, reason));
      }
    }
    return 0;
  case TOML_ARRAY:
    if (is_array_of_tables(src)) {
      // append src array to dst
      for (int i = 0; i < src.u.arr.size; i++) {
        toml_datum_t *pelem = arr_emplace(dst, reason);
        if (!pelem) {
          return -1;
        }
        DO(datum_copy(pelem, src.u.arr.elem[i], pool, reason));
      }
      return 0;
    }
    // fallthru
  default:
    break;
  }
  datum_free(dst);
  return datum_copy(dst, src, pool, reason);
}

// Compare the content of a and b.
static bool datum_equiv(toml_datum_t a, toml_datum_t b) {
  if (a.type != b.type) {
    return false;
  }
  int N;
  switch (a.type) {
  case TOML_STRING:
    return a.u.str.len == b.u.str.len &&
           0 == memcmp(a.u.str.ptr, b.u.str.ptr, a.u.str.len);
  case TOML_INT64:
    return a.u.int64 == b.u.int64;
  case TOML_FP64:
    return a.u.fp64 == b.u.fp64;
  case TOML_BOOLEAN:
    return !!a.u.boolean == !!b.u.boolean;
  case TOML_DATE:
    return a.u.ts.year == b.u.ts.year && a.u.ts.month == b.u.ts.month &&
           a.u.ts.day == b.u.ts.day;
  case TOML_TIME:
    return a.u.ts.hour == b.u.ts.hour && a.u.ts.minute == b.u.ts.minute &&
           a.u.ts.second == b.u.ts.second && a.u.ts.usec == b.u.ts.usec;
  case TOML_DATETIME:
    return a.u.ts.year == b.u.ts.year && a.u.ts.month == b.u.ts.month &&
           a.u.ts.day == b.u.ts.day && a.u.ts.hour == b.u.ts.hour &&
           a.u.ts.minute == b.u.ts.minute && a.u.ts.second == b.u.ts.second &&
           a.u.ts.usec == b.u.ts.usec;
  case TOML_DATETIMETZ:
    return a.u.ts.year == b.u.ts.year && a.u.ts.month == b.u.ts.month &&
           a.u.ts.day == b.u.ts.day && a.u.ts.hour == b.u.ts.hour &&
           a.u.ts.minute == b.u.ts.minute && a.u.ts.second == b.u.ts.second &&
           a.u.ts.usec == b.u.ts.usec && a.u.ts.tz == b.u.ts.tz;
  case TOML_ARRAY:
    N = a.u.arr.size;
    if (N != b.u.arr.size) {
      return false;
    }
    for (int i = 0; i < N; i++) {
      if (!datum_equiv(a.u.arr.elem[i], b.u.arr.elem[i])) {
        return false;
      }
    }
    return true;
  case TOML_TABLE:
    N = a.u.tab.size;
    if (N != b.u.tab.size) {
      return false;
    }
    for (int i = 0; i < N; i++) {
      int len = a.u.tab.len[i];
      if (len != b.u.tab.len[i]) {
        return false;
      }
      if (0 != memcmp(a.u.tab.key[i], b.u.tab.key[i], len)) {
        return false;
      }
      if (!datum_equiv(a.u.tab.value[i], b.u.tab.value[i])) {
        return false;
      }
    }
    return true;
  default:
    break;
  }
  return false;
}

/**
 *  Override values in r1 using r2. Return a new result. All results
 *  (i.e., r1, r2 and the returned result) must be freed using toml_free()
 *  after use.
 *
 *  LOGIC:
 *   ret = copy of r1
 *   for each item x in r2:
 *     if x is not in ret:
 *          override
 *     elif x in ret is NOT of the same type:
 *         override
 *     elif x is an array of tables:
 *         append r2.x to ret.x
 *     elif x is a table:
 *         merge r2.x to ret.x
 *     else:
 *         override
 */
toml_result_t toml_merge(const toml_result_t *r1, const toml_result_t *r2) {
  const char *reason = "";
  toml_result_t ret = {0};
  pool_t *pool = 0;
  if (!r1->ok) {
    reason = "param error: r1 not ok";
    goto bail;
  }
  if (!r2->ok) {
    reason = "param error: r2 not ok";
    goto bail;
  }
  {
    pool_t *r1pool = (pool_t *)r1->__internal;
    pool_t *r2pool = (pool_t *)r2->__internal;
    pool = pool_create(r1pool->top + r2pool->top);
    if (!pool) {
      reason = "out of memory";
      goto bail;
    }
  }

  // Make a copy of r1
  if (datum_copy(&ret.toptab, r1->toptab, pool, &reason)) {
    goto bail;
  }

  // Merge r2 into the result
  if (datum_merge(&ret.toptab, r2->toptab, pool, &reason)) {
    goto bail;
  }

  ret.ok = 1;
  ret.__internal = pool;
  return ret;

bail:
  pool_destroy(pool);
  snprintf(ret.errmsg, sizeof(ret.errmsg), "%s", reason);
  return ret;
}

bool toml_equiv(const toml_result_t *r1, const toml_result_t *r2) {
  if (!(r1->ok && r2->ok)) {
    return false;
  }
  return datum_equiv(r1->toptab, r2->toptab);
}

/**
 * Find a key in a toml_table. Return the value of the key if found,
 * or a TOML_UNKNOWN otherwise.
 */
toml_datum_t toml_get(toml_datum_t datum, const char *key) {
  toml_datum_t ret = {0};
  if (datum.type == TOML_TABLE) {
    int n = datum.u.tab.size;
    const char **pkey = datum.u.tab.key;
    toml_datum_t *pvalue = datum.u.tab.value;
    for (int i = 0; i < n; i++) {
      if (0 == strcmp(pkey[i], key)) {
        return pvalue[i];
      }
    }
  }
  return ret;
}

/**
 * Locate a value starting from a toml_table. Return the value of the key if
 * found, or a TOML_UNKNOWN otherwise.
 *
 * Note: the multipart-key is separated by DOT, and must not have any escape
 * chars.
 */
toml_datum_t toml_seek(toml_datum_t table, const char *multipart_key) {
  if (table.type != TOML_TABLE) {
    return DATUM_ZERO;
  }

  // Make a mutable copy of the multipart_key for splitting
  char buf[256];
  if (copystring(buf, sizeof(buf), multipart_key)) {
    return DATUM_ZERO;
  }

  // Go through the multipart name part by part.
  char *p = buf;            // start of current key
  char *q = strchr(p, '.'); // end of current key
  toml_datum_t datum = table;
  while (q && datum.type == TOML_TABLE) {
    *q = 0;
    datum = toml_get(datum, p);
    if (datum.type == TOML_TABLE) {
      p = q + 1;
      q = strchr(p, '.');
    }
  }

  if (!q && datum.type == TOML_TABLE) {
    return toml_get(datum, p);
  }

  return DATUM_ZERO;
}

/**
 *  Return the default options.
 */
toml_option_t toml_default_option(void) {
  toml_option_t opt = {0, realloc, free};
  return opt;
}

/**
 *  Override the current options.
 */
void toml_set_option(toml_option_t opt) { toml_option = opt; }

/**
 *  Free the result returned by toml_parse().
 */
void toml_free(toml_result_t result) {
  datum_free(&result.toptab);
  pool_destroy((pool_t *)result.__internal);
}
