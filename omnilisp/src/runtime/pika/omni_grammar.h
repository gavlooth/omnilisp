/*
 * OmniLisp Grammar for Pika Parser
 *
 * Defines the OmniLisp grammar using pika_c clause builders.
 * Supports dual-mode output: AST nodes (Value*) or strings.
 */

#ifndef OMNI_GRAMMAR_H
#define OMNI_GRAMMAR_H

#include "../pika_c/pika.h"
#include "../types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Output mode for parsing */
typedef enum {
    OMNI_OUTPUT_AST,     /* Emit Value* AST nodes */
    OMNI_OUTPUT_STRING,  /* Emit string representation */
    OMNI_OUTPUT_BOTH     /* Emit both (for staging/tower collapse) */
} OmniOutputMode;

/* Parse result with dual output */
typedef struct {
    Value* ast;          /* AST result (if OUTPUT_AST or OUTPUT_BOTH) */
    char* str;           /* String result (if OUTPUT_STRING or OUTPUT_BOTH) */
    int success;
    char* error;
    int error_pos;
    int error_line;
    int error_col;
} OmniParseResult;

/* Get the OmniLisp grammar (singleton, lazy-initialized) */
PikaGrammar* omni_get_grammar(void);

/* Free the grammar (call at program exit) */
void omni_grammar_cleanup(void);

/* Parse a single expression */
OmniParseResult omni_pika_parse(const char* input, OmniOutputMode mode);

/* Parse all expressions in input */
OmniParseResult omni_pika_parse_all(const char* input, OmniOutputMode mode);

/* Parse with custom grammar (for user-defined DSLs) */
OmniParseResult omni_pika_parse_with_grammar(
    PikaGrammar* grammar,
    const char* start_rule,
    const char* input,
    OmniOutputMode mode
);

/* Free parse result */
void omni_parse_result_free(OmniParseResult* result);

/* ============== String Operations ============== */

/* Match pattern against string, return list of captures or nil */
Value* omni_pika_match(const char* pattern, const char* input);

/* Match with compiled grammar rule */
Value* omni_pika_match_rule(PikaGrammar* grammar, const char* rule, const char* input);

/* Split string by pattern, return list of strings */
Value* omni_pika_split(const char* pattern, const char* input);

/* Replace pattern in string */
Value* omni_pika_replace(const char* pattern, const char* replacement,
                         const char* input, int global);

/* Find all matches of pattern */
Value* omni_pika_find_all(const char* pattern, const char* input);

/* ============== PEG DSL Compilation ============== */

/* Compile PEG grammar from string */
PikaGrammar* omni_compile_peg(const char* peg_source, char** error_out);

/* Compile single regex-like pattern */
PikaGrammar* omni_compile_pattern(const char* pattern, char** error_out);

#ifdef __cplusplus
}
#endif

#endif /* OMNI_GRAMMAR_H */
