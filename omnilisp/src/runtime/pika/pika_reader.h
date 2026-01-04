/*
 * Pika Reader - Drop-in replacement for omni_reader
 *
 * Provides the same API as omni_reader.h but uses Pika parser internally.
 * Supports dual-mode output for tower collapse.
 */

#ifndef PIKA_READER_H
#define PIKA_READER_H

#include "../types.h"
#include "omni_grammar.h"
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Reader state (compatible with omni_reader) */
typedef struct PikaReader {
    const char* input;
    size_t pos;
    size_t len;
    int line;
    int col;
    OmniOutputMode mode;  /* Output mode for dual output */
} PikaReader;

/* Initialize reader with input string */
PikaReader* pika_reader_new(const char* input);
PikaReader* pika_reader_new_with_mode(const char* input, OmniOutputMode mode);
void pika_reader_free(PikaReader* r);

/* Set output mode */
void pika_reader_set_mode(PikaReader* r, OmniOutputMode mode);

/* Main read function - returns AST Value */
Value* pika_read_expr(PikaReader* r);

/* Read multiple expressions (for files) */
Value* pika_read_all(PikaReader* r);

/* Convenience: read from string */
Value* pika_parse(const char* input);
Value* pika_parse_all(const char* input);

/* Dual-mode parsing: returns both AST and string */
typedef struct {
    Value* ast;
    char* str;
} PikaDualResult;

PikaDualResult pika_parse_dual(const char* input);
PikaDualResult pika_parse_all_dual(const char* input);
void pika_dual_result_free(PikaDualResult* result);

/* ============== Compatibility Aliases ============== */

/* These match the omni_reader.h interface exactly */
#define Reader PikaReader
#define reader_new pika_reader_new
#define reader_free pika_reader_free
#define omni_read_expr pika_read_expr
#define omni_read_all pika_read_all
#define omni_parse pika_parse
#define omni_parse_all pika_parse_all

#ifdef __cplusplus
}
#endif

#endif /* PIKA_READER_H */
