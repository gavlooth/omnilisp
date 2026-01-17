/*
 * regex_compile.h - Pika-style Regex Engine for OmniLisp
 *
 * Implements a regex engine using Pika-style PEG matching.
 * This is a self-contained implementation that doesn't depend on
 * the compiler's Pika parser.
 *
 * Supported Regex Features:
 *   - Literals: abc
 *   - Character classes: [abc], [a-z], [^abc]
 *   - Any character: .
 *   - Quantifiers: *, +, ?
 *   - Alternation: a|b
 *   - Grouping: (ab)+
 *   - Escapes: \d, \w, \s, \n, \t, \r, \\, \., etc.
 *
 * Not Supported (PEG limitation):
 *   - Backreferences: \1, \2
 */

#ifndef REGEX_COMPILE_H
#define REGEX_COMPILE_H

#include "../include/omni.h"
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Opaque compiled regex structure */
typedef struct PikaRegexCompiled PikaRegexCompiled;

/*
 * Compile a regex pattern
 *
 * Parameters:
 *   - pattern: Regex pattern string
 *
 * Returns:
 *   - Compiled regex structure (caller must free with pika_regex_free)
 *   - On error, the error field is set (check with pika_regex_error)
 */
PikaRegexCompiled* pika_regex_compile(const char* pattern);

/*
 * Get error message from compilation
 * Returns NULL if no error
 */
const char* pika_regex_error(PikaRegexCompiled* compiled);

/*
 * Free a compiled regex
 */
void pika_regex_free(PikaRegexCompiled* compiled);

/*
 * Find first match anywhere in input (not anchored to start)
 *
 * Returns:
 *   - Matched substring as Obj*, or NULL if no match
 */
Obj* pika_regex_search(PikaRegexCompiled* compiled, const char* input);

/*
 * Find all non-overlapping matches
 *
 * Returns:
 *   - List of matched strings as Obj* (pair list), or NULL
 */
Obj* pika_regex_find_all(PikaRegexCompiled* compiled, const char* input);

/*
 * Split string by pattern
 *
 * Returns:
 *   - List of strings as Obj* (pair list)
 */
Obj* pika_regex_split(PikaRegexCompiled* compiled, const char* input);

/*
 * Replace matches with replacement string
 *
 * Parameters:
 *   - compiled: Compiled regex
 *   - replacement: Replacement string
 *   - input: Input string
 *   - global: If true, replace all; if false, replace first only
 *
 * Returns:
 *   - New string with replacements
 */
Obj* pika_regex_replace(PikaRegexCompiled* compiled, const char* replacement,
                        const char* input, bool global);

/*
 * Check if pattern matches entire string
 *
 * Returns:
 *   - Boolean Obj* (true/false)
 */
Obj* pika_regex_fullmatch(PikaRegexCompiled* compiled, const char* input);

#ifdef __cplusplus
}
#endif

#endif /* REGEX_COMPILE_H */
