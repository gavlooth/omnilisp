/*
 * OmniLisp Grammar for Pika Parser
 *
 * Defines the OmniLisp grammar using pika_c clause builders.
 * Supports dual-mode output: AST nodes (Value*) or strings.
 *
 * PIKA PARSER POWER HIERARCHY
 * ===========================
 * Pika is MORE POWERFUL than both standard PEG and traditional regex:
 *
 *   Formal Regex (DFA)
 *       │  • No nesting, no recursion
 *       ▼
 *   Standard PEG (Packrat)
 *       │  • NO left recursion (infinite loop)
 *       │  • Anchored to start only
 *       │  • First match only
 *       ▼
 *   PIKA PARSER  ◄── We use this
 *       │  • LEFT RECURSION support (expr <- expr '+' term)
 *       │  • SUBSTRING matching (find anywhere in input)
 *       │  • ALL non-overlapping matches
 *       │  • Grammar composition with named rules
 *       ▼
 *   PCRE (practical regex)
 *       • Has backreferences (\1) - Pika lacks this only
 *
 * See docs/PATTERN_SYNTAX.md for full documentation.
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

/* ============== PIKA-SPECIFIC ADVANCED API ============== */
/*
 * These functions leverage Pika's unique capabilities that exceed
 * standard PEG and regex: left recursion, substring matching,
 * all-matches mode, and grammar composition.
 */

/* Match info with position - for find_all_with_positions */
typedef struct {
    char* text;          /* Matched text (owned, must free) */
    int start;           /* Start position in input */
    int end;             /* End position in input */
    int line;            /* Line number (1-based) */
    int column;          /* Column number (1-based) */
} OmniMatchInfo;

/* Result of find_all_with_positions */
typedef struct {
    OmniMatchInfo* matches;  /* Array of matches (owned, must free) */
    size_t count;            /* Number of matches */
    char* error;             /* Error message if failed (owned) */
} OmniMatchResult;

/*
 * Grammar-based matching - LEVERAGES LEFT RECURSION
 *
 * Unlike standard PEG, Pika handles left-recursive grammars:
 *   expr <- expr '+' term / term   // Works in Pika, infinite loop in packrat
 *
 * Example:
 *   omni_pika_grammar_match(
 *       "expr <- expr '+' term / term\n"
 *       "term <- [0-9]+\n",
 *       "expr", "1+2+3"
 *   );
 */
Value* omni_pika_grammar_match(const char* grammar_spec, const char* start_rule,
                                const char* input);

/*
 * Find all matches with position information - LEVERAGES SUBSTRING MATCHING
 *
 * Unlike standard PEG (anchored to start), Pika finds matches anywhere.
 * Returns all non-overlapping matches with their positions.
 */
OmniMatchResult omni_pika_find_all_positions(const char* pattern, const char* input);

/* Free match result */
void omni_match_result_free(OmniMatchResult* result);

/*
 * Grammar-based find all - COMBINES LEFT RECURSION + ALL MATCHES
 *
 * Find all matches of a grammar rule anywhere in input.
 * This is the most powerful matching mode.
 */
OmniMatchResult omni_pika_grammar_find_all(const char* grammar_spec,
                                            const char* rule,
                                            const char* input);

/*
 * Cached grammar compilation - for repeated use
 *
 * Compiles grammar once, returns handle for reuse.
 * Much faster than recompiling for each match.
 */
typedef struct OmniCompiledGrammar OmniCompiledGrammar;

OmniCompiledGrammar* omni_grammar_compile(const char* grammar_spec, char** error_out);
void omni_grammar_compiled_free(OmniCompiledGrammar* compiled);

/* Match using pre-compiled grammar */
Value* omni_pika_compiled_match(OmniCompiledGrammar* compiled,
                                 const char* start_rule,
                                 const char* input);

/* Find all using pre-compiled grammar */
OmniMatchResult omni_pika_compiled_find_all(OmniCompiledGrammar* compiled,
                                             const char* rule,
                                             const char* input);

#ifdef __cplusplus
}
#endif

#endif /* OMNI_GRAMMAR_H */
