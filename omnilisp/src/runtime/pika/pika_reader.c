/*
 * Pika Reader Implementation
 *
 * Drop-in replacement for omni_reader using Pika parser.
 */

#define _POSIX_C_SOURCE 200809L
#include "pika_reader.h"
#include "omni_grammar.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* ============== Reader State ============== */

PikaReader* pika_reader_new(const char* input) {
    return pika_reader_new_with_mode(input, OMNI_OUTPUT_AST);
}

PikaReader* pika_reader_new_with_mode(const char* input, OmniOutputMode mode) {
    PikaReader* r = malloc(sizeof(PikaReader));
    if (!r) return NULL;
    r->input = input;
    r->pos = 0;
    r->len = strlen(input);
    r->line = 1;
    r->col = 1;
    r->mode = mode;
    return r;
}

void pika_reader_free(PikaReader* r) {
    if (r) free(r);
}

void pika_reader_set_mode(PikaReader* r, OmniOutputMode mode) {
    if (r) r->mode = mode;
}

/* ============== Core Read Functions ============== */

Value* pika_read_expr(PikaReader* r) {
    if (!r || r->pos >= r->len) return mk_error("Unexpected end of input");

    /* Parse remaining input starting at current position */
    const char* remaining = r->input + r->pos;
    OmniParseResult result = omni_pika_parse(remaining, r->mode);

    if (!result.success) {
        Value* err = mk_error(result.error ? result.error : "Parse error");
        omni_parse_result_free(&result);
        return err;
    }

    /* Update position based on what was consumed */
    /* For now, we assume the whole remaining input was an expression */
    /* A more sophisticated implementation would track exact positions */

    Value* ast = result.ast;
    omni_parse_result_free(&result);
    return ast ? ast : mk_nil();
}

Value* pika_read_all(PikaReader* r) {
    if (!r) return mk_error("NULL reader");

    OmniParseResult result = omni_pika_parse_all(r->input, r->mode);

    if (!result.success) {
        Value* err = mk_error(result.error ? result.error : "Parse error");
        omni_parse_result_free(&result);
        return err;
    }

    Value* ast = result.ast;
    omni_parse_result_free(&result);
    return ast ? ast : mk_nil();
}

/* ============== Convenience Functions ============== */

Value* pika_parse(const char* input) {
    PikaReader* r = pika_reader_new(input);
    if (!r) return mk_error("OOM");
    Value* result = pika_read_expr(r);
    pika_reader_free(r);
    return result;
}

Value* pika_parse_all(const char* input) {
    PikaReader* r = pika_reader_new(input);
    if (!r) return mk_error("OOM");
    Value* result = pika_read_all(r);
    pika_reader_free(r);
    return result;
}

/* ============== Dual-Mode Parsing ============== */

PikaDualResult pika_parse_dual(const char* input) {
    PikaDualResult dual = {NULL, NULL};

    OmniParseResult result = omni_pika_parse(input, OMNI_OUTPUT_BOTH);

    if (result.success) {
        dual.ast = result.ast;
        dual.str = result.str;
        result.str = NULL; /* Transfer ownership */
    }

    omni_parse_result_free(&result);
    return dual;
}

PikaDualResult pika_parse_all_dual(const char* input) {
    PikaDualResult dual = {NULL, NULL};

    OmniParseResult result = omni_pika_parse_all(input, OMNI_OUTPUT_BOTH);

    if (result.success) {
        dual.ast = result.ast;
        dual.str = result.str;
        result.str = NULL; /* Transfer ownership */
    }

    omni_parse_result_free(&result);
    return dual;
}

void pika_dual_result_free(PikaDualResult* result) {
    if (!result) return;
    if (result->str) free(result->str);
    /* ast is managed by ASAP/GC */
}
