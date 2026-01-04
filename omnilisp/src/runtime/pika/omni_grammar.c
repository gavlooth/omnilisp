/*
 * OmniLisp Grammar Implementation
 *
 * Uses pika_c to define the OmniLisp grammar with dual-mode output.
 */

#define _POSIX_C_SOURCE 200809L
#include "omni_grammar.h"
#include "../pika_c/pika.h"
#include "../types.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

/* ============== Grammar Singleton ============== */

static PikaGrammar* g_omni_grammar = NULL;

/* ============== AST Construction from Matches ============== */

/* Forward declarations */
static Value* match_to_ast(const PikaMatch* match, const char* input);
static char* match_to_string(const PikaMatch* match, const char* input);

/*
 * Helper: Collect all labeled submatches with a given label recursively
 */
static void collect_labeled_matches(
    const PikaMatch* match,
    const char* label,
    const PikaMatch*** out_matches,
    size_t* out_count,
    size_t* out_capacity
) {
    if (!match) return;

    size_t sub_count = 0;
    PikaLabeledMatch* subs = pika_match_get_submatches(match, &sub_count);

    for (size_t i = 0; i < sub_count; i++) {
        if (subs[i].label && strcmp(subs[i].label, label) == 0) {
            /* Found a match with the target label */
            if (*out_count >= *out_capacity) {
                *out_capacity = (*out_capacity == 0) ? 8 : (*out_capacity * 2);
                *out_matches = realloc(*out_matches, *out_capacity * sizeof(PikaMatch*));
            }
            (*out_matches)[(*out_count)++] = subs[i].match;
            /* DON'T recurse into this match - it will be processed separately */
            /* This prevents nested lists from having their elements flattened */
        } else {
            /* Only recurse into non-matching submatches */
            collect_labeled_matches(subs[i].match, label, out_matches, out_count, out_capacity);
        }
    }

    free(subs);
}

/*
 * Convert pika match to AST Value*
 *
 * The match tree structure:
 * - The match may have labeled submatches (e.g., "list", "int", "elem")
 * - When we find a labeled submatch, we process it according to its label
 * - For compound forms (list, array), we recursively search for "elem" labels
 */
static Value* match_to_ast(const PikaMatch* match, const char* input) {
    if (!match) return mk_nil();

    int start = pika_match_start(match);
    int len = pika_match_len(match);

    /* Get submatches with labels */
    size_t sub_count = 0;
    PikaLabeledMatch* subs = pika_match_get_submatches(match, &sub_count);

    /* Check label to determine AST type */
    const char* label = NULL;
    const PikaMatch* labeled_match = NULL;
    if (subs && sub_count > 0 && subs[0].label) {
        label = subs[0].label;
        labeled_match = subs[0].match;
    }

    /* Integer */
    if (label && strcmp(label, "int") == 0) {
        /* Use the labeled match's position */
        int lstart = pika_match_start(labeled_match);
        int llen = pika_match_len(labeled_match);
        char* text = malloc(llen + 1);
        memcpy(text, input + lstart, llen);
        text[llen] = '\0';
        long val = strtol(text, NULL, 10);
        free(text);
        free(subs);
        return mk_int(val);
    }

    /* Hex integer */
    if (label && strcmp(label, "hex") == 0) {
        int lstart = pika_match_start(labeled_match);
        int llen = pika_match_len(labeled_match);
        char* text = malloc(llen + 1);
        memcpy(text, input + lstart, llen);
        text[llen] = '\0';
        long val = strtol(text + 2, NULL, 16); /* Skip 0x */
        free(text);
        free(subs);
        return mk_int(val);
    }

    /* Boolean */
    if (label && strcmp(label, "bool") == 0) {
        int lstart = pika_match_start(labeled_match);
        int llen = pika_match_len(labeled_match);
        char* text = malloc(llen + 1);
        memcpy(text, input + lstart, llen);
        text[llen] = '\0';
        Value* result = mk_sym(text);  /* #t or #f as symbol */
        free(text);
        free(subs);
        return result;
    }

    /* Symbol */
    if (label && strcmp(label, "sym") == 0) {
        int lstart = pika_match_start(labeled_match);
        int llen = pika_match_len(labeled_match);
        char* text = malloc(llen + 1);
        memcpy(text, input + lstart, llen);
        text[llen] = '\0';

        /* Special symbols */
        Value* result;
        if (strcmp(text, "true") == 0 || strcmp(text, "false") == 0) {
            result = mk_sym(text);
        } else if (strcmp(text, "nothing") == 0) {
            result = mk_nothing();
        } else if (strcmp(text, "nil") == 0) {
            result = mk_nil();
        } else {
            result = mk_sym(text);
        }
        free(text);
        free(subs);
        return result;
    }

    /* String */
    if (label && strcmp(label, "string") == 0) {
        int lstart = pika_match_start(labeled_match);
        int llen = pika_match_len(labeled_match);
        /* Extract string content (skip quotes, handle escapes) */
        char* text = malloc(llen + 1);
        int j = 0;
        for (int i = 1; i < llen - 1; i++) { /* Skip quotes */
            char c = input[lstart + i];
            if (c == '\\' && i + 1 < llen - 1) {
                i++;
                c = input[lstart + i];
                switch (c) {
                    case 'n': text[j++] = '\n'; break;
                    case 't': text[j++] = '\t'; break;
                    case 'r': text[j++] = '\r'; break;
                    case '\\': text[j++] = '\\'; break;
                    case '"': text[j++] = '"'; break;
                    case '0': text[j++] = '\0'; break;
                    default: text[j++] = c; break;
                }
            } else {
                text[j++] = c;
            }
        }
        text[j] = '\0';
        Value* result = mk_code(text);
        free(text);
        free(subs);
        return result;
    }

    /* List - recursively find all "elem" labeled matches */
    if (label && strcmp(label, "list") == 0) {
        const PikaMatch** elem_matches = NULL;
        size_t elem_count = 0;
        size_t elem_capacity = 0;

        collect_labeled_matches(labeled_match, "elem", &elem_matches, &elem_count, &elem_capacity);

        Value* items = mk_nil();
        Value** tail = &items;

        for (size_t i = 0; i < elem_count; i++) {
            Value* elem = match_to_ast(elem_matches[i], input);
            Value* cell = mk_cell(elem, mk_nil());
            *tail = cell;
            tail = &cell->cell.cdr;
        }

        free(elem_matches);
        free(subs);
        return items;
    }

    /* Array - similar to list, but wrapped with 'array' symbol */
    if (label && strcmp(label, "array") == 0) {
        const PikaMatch** elem_matches = NULL;
        size_t elem_count = 0;
        size_t elem_capacity = 0;

        collect_labeled_matches(labeled_match, "elem", &elem_matches, &elem_count, &elem_capacity);

        Value* items = mk_nil();
        Value** tail = &items;

        for (size_t i = 0; i < elem_count; i++) {
            Value* elem = match_to_ast(elem_matches[i], input);
            Value* cell = mk_cell(elem, mk_nil());
            *tail = cell;
            tail = &cell->cell.cdr;
        }

        free(elem_matches);
        free(subs);
        return mk_cell(mk_sym("array"), items);
    }

    /* Quote - recurse into the quoted expression */
    if (label && strcmp(label, "quote") == 0) {
        /* Get submatches of quote to find the quoted expr */
        size_t quote_sub_count = 0;
        PikaLabeledMatch* quote_subs = pika_match_get_submatches(labeled_match, &quote_sub_count);

        Value* result = mk_nil();
        if (quote_sub_count > 0) {
            Value* quoted = match_to_ast(quote_subs[0].match, input);
            result = mk_cell(mk_sym("quote"), mk_cell(quoted, mk_nil()));
        }
        free(quote_subs);
        free(subs);
        return result;
    }

    /* Quasiquote */
    if (label && strcmp(label, "quasiquote") == 0) {
        size_t qq_sub_count = 0;
        PikaLabeledMatch* qq_subs = pika_match_get_submatches(labeled_match, &qq_sub_count);

        Value* result = mk_nil();
        if (qq_sub_count > 0) {
            Value* quoted = match_to_ast(qq_subs[0].match, input);
            result = mk_cell(mk_sym("quasiquote"), mk_cell(quoted, mk_nil()));
        }
        free(qq_subs);
        free(subs);
        return result;
    }

    /* Unquote */
    if (label && strcmp(label, "unquote") == 0) {
        size_t uq_sub_count = 0;
        PikaLabeledMatch* uq_subs = pika_match_get_submatches(labeled_match, &uq_sub_count);

        Value* result = mk_nil();
        if (uq_sub_count > 0) {
            Value* unquoted = match_to_ast(uq_subs[0].match, input);
            result = mk_cell(mk_sym("unquote"), mk_cell(unquoted, mk_nil()));
        }
        free(uq_subs);
        free(subs);
        return result;
    }

    /* Unquote-splicing */
    if (label && strcmp(label, "unquote-splicing") == 0) {
        size_t us_sub_count = 0;
        PikaLabeledMatch* us_subs = pika_match_get_submatches(labeled_match, &us_sub_count);

        Value* result = mk_nil();
        if (us_sub_count > 0) {
            Value* spliced = match_to_ast(us_subs[0].match, input);
            result = mk_cell(mk_sym("unquote-splicing"), mk_cell(spliced, mk_nil()));
        }
        free(us_subs);
        free(subs);
        return result;
    }

    /* Default: if has submatches, recurse into first one */
    if (sub_count > 0) {
        Value* first = match_to_ast(subs[0].match, input);
        free(subs);
        return first;
    }

    /* Fallback: return matched text as symbol */
    char* text = malloc(len + 1);
    memcpy(text, input + start, len);
    text[len] = '\0';
    Value* result = mk_sym(text);
    free(text);
    free(subs);
    return result;
}

/* Convert match to string representation */
static char* match_to_string(const PikaMatch* match, const char* input) {
    if (!match) return strdup("");

    int start = pika_match_start(match);
    int len = pika_match_len(match);

    char* text = malloc(len + 1);
    memcpy(text, input + start, len);
    text[len] = '\0';
    return text;
}

/* ============== Grammar Definition ============== */

/*
 * IMPORTANT: Grammar ownership rules
 * -----------------------------------
 * pika_c clause builders TAKE OWNERSHIP of their children. This means:
 * - Each clause can only be used ONCE in the grammar tree
 * - For shared constructs (like 'skip'), define as a named RULE and use
 *   pika_clause_rule_ref("rule_name") to reference it
 * - Never reuse the same PikaClause* pointer in multiple places
 */

PikaGrammar* omni_get_grammar(void) {
    if (g_omni_grammar) return g_omni_grammar;

    /* Build OmniLisp grammar using pika_c API */

    /* ========== SHARED RULES (referenced via rule_ref) ========== */

    /* Whitespace character: space, tab, newline, return, comma */
    PikaClause* ws_char_clause = pika_clause_charset_from_chars(" \t\n\r,", 5);

    /* Comment: ; to end of line */
    PikaClause* comment_arr[] = {
        pika_clause_char(';'),
        pika_clause_zero_or_more(
            pika_clause_charset_invert(pika_clause_charset_from_chars("\n", 1))
        )
    };
    PikaClause* comment_clause = pika_clause_seq(comment_arr, 2);

    /* Skip: zero or more (whitespace | comment) */
    PikaClause* skip_item_arr[] = {
        pika_clause_charset_from_chars(" \t\n\r,", 5),  /* Fresh ws_char for skip */
        pika_clause_seq((PikaClause*[]){
            pika_clause_char(';'),
            pika_clause_zero_or_more(
                pika_clause_charset_invert(pika_clause_charset_from_chars("\n", 1))
            )
        }, 2)  /* Fresh comment for skip */
    };
    PikaClause* skip_item = pika_clause_first(skip_item_arr, 2);
    PikaClause* skip_clause = pika_clause_zero_or_more(skip_item);

    /* ========== ATOM RULES ========== */

    /* Boolean literals: #t and #f */
    PikaClause* bool_true = pika_clause_ast_label("bool",
        pika_clause_str("#t")
    );
    PikaClause* bool_false = pika_clause_ast_label("bool",
        pika_clause_str("#f")
    );
    PikaClause* bool_arr[] = {bool_true, bool_false};
    PikaClause* bool_clause = pika_clause_first(bool_arr, 2);

    /* Integer: one or more digits */
    PikaClause* int_clause = pika_clause_ast_label("int",
        pika_clause_one_or_more(pika_clause_charset_from_range('0', '9'))
    );

    /* Hex integer: 0x followed by hex digits */
    PikaClause* hex_arr[] = {
        pika_clause_str("0x"),
        pika_clause_one_or_more(
            pika_clause_charset_from_pattern("0-9a-fA-F")
        )
    };
    PikaClause* hex_clause = pika_clause_ast_label("hex",
        pika_clause_seq(hex_arr, 2)
    );

    /* Symbol first character: alpha or special */
    PikaClause* sym_first_arr[] = {
        pika_clause_charset_from_range('a', 'z'),
        pika_clause_charset_from_range('A', 'Z'),
        pika_clause_charset_from_chars("_+-*/<>=!?&%|", 13)
    };
    PikaClause* sym_first = pika_clause_charset_union_take(sym_first_arr, 3);

    /* Symbol rest characters: alpha, digit, or special */
    PikaClause* sym_rest_arr[] = {
        pika_clause_charset_from_range('a', 'z'),
        pika_clause_charset_from_range('A', 'Z'),
        pika_clause_charset_from_range('0', '9'),
        pika_clause_charset_from_chars("_+-*/<>=!?&%|.:#'", 17)
    };
    PikaClause* sym_rest = pika_clause_charset_union_take(sym_rest_arr, 4);

    /* Symbol: first char followed by zero or more rest chars */
    PikaClause* sym_arr[] = {sym_first, pika_clause_zero_or_more(sym_rest)};
    PikaClause* sym_clause = pika_clause_ast_label("sym",
        pika_clause_seq(sym_arr, 2)
    );

    /* String character: escape sequence or non-quote/backslash */
    PikaClause* string_char_arr[] = {
        pika_clause_seq((PikaClause*[]){
            pika_clause_char('\\'),
            pika_clause_charset_from_chars("ntr\\\"0", 6)
        }, 2),
        pika_clause_charset_invert(pika_clause_charset_from_chars("\"\\", 2))
    };
    PikaClause* string_char = pika_clause_first(string_char_arr, 2);

    /* String: quote, chars, quote */
    PikaClause* string_arr[] = {
        pika_clause_char('"'),
        pika_clause_zero_or_more(string_char),
        pika_clause_char('"')
    };
    PikaClause* string_clause = pika_clause_ast_label("string",
        pika_clause_seq(string_arr, 3)
    );

    /* Atom: bool / hex / int / string / symbol (order matters for PEG) */
    /* Note: bool must come before symbol to match #t/#f before treating as symbol */
    PikaClause* atom_arr[] = {bool_clause, hex_clause, int_clause, string_clause, sym_clause};
    PikaClause* atom_clause = pika_clause_first(atom_arr, 5);

    /* ========== QUOTE FORMS (use rule_ref for expr) ========== */

    /* Quote: 'expr */
    PikaClause* quote_arr[] = {pika_clause_char('\''), pika_clause_rule_ref("expr")};
    PikaClause* quote = pika_clause_ast_label("quote",
        pika_clause_seq(quote_arr, 2)
    );

    /* Quasiquote: `expr */
    PikaClause* quasiquote_arr[] = {pika_clause_char('`'), pika_clause_rule_ref("expr")};
    PikaClause* quasiquote = pika_clause_ast_label("quasiquote",
        pika_clause_seq(quasiquote_arr, 2)
    );

    /* Unquote-splicing: ,@expr (must come before unquote) */
    PikaClause* unquote_splice_arr[] = {pika_clause_str(",@"), pika_clause_rule_ref("expr")};
    PikaClause* unquote_splice = pika_clause_ast_label("unquote-splicing",
        pika_clause_seq(unquote_splice_arr, 2)
    );

    /* Unquote: ,expr */
    PikaClause* unquote_arr[] = {pika_clause_char(','), pika_clause_rule_ref("expr")};
    PikaClause* unquote = pika_clause_ast_label("unquote",
        pika_clause_seq(unquote_arr, 2)
    );

    /* ========== COMPOUND FORMS (use rule_ref for skip and expr) ========== */

    /* List element: skip followed by expr */
    PikaClause* list_elem_arr[] = {
        pika_clause_rule_ref("skip"),
        pika_clause_ast_label("elem", pika_clause_rule_ref("expr"))
    };
    PikaClause* list_elem = pika_clause_seq(list_elem_arr, 2);

    /* List: ( skip (skip expr)* skip ) */
    PikaClause* list_arr[] = {
        pika_clause_char('('),
        pika_clause_rule_ref("skip"),
        pika_clause_zero_or_more(list_elem),
        pika_clause_rule_ref("skip"),
        pika_clause_char(')')
    };
    PikaClause* list = pika_clause_ast_label("list",
        pika_clause_seq(list_arr, 5)
    );

    /* Array element: skip followed by expr */
    PikaClause* array_elem_arr[] = {
        pika_clause_rule_ref("skip"),
        pika_clause_ast_label("elem", pika_clause_rule_ref("expr"))
    };
    PikaClause* array_elem = pika_clause_seq(array_elem_arr, 2);

    /* Array: [ skip (skip expr)* skip ] */
    PikaClause* array_arr[] = {
        pika_clause_char('['),
        pika_clause_rule_ref("skip"),
        pika_clause_zero_or_more(array_elem),
        pika_clause_rule_ref("skip"),
        pika_clause_char(']')
    };
    PikaClause* array = pika_clause_ast_label("array",
        pika_clause_seq(array_arr, 5)
    );

    /* ========== EXPRESSION (combines all forms) ========== */

    PikaClause* expr_choices[] = {
        list, array,
        unquote_splice, unquote, quasiquote, quote,
        pika_clause_rule_ref("atom")
    };
    PikaClause* expr_clause = pika_clause_first(expr_choices, 7);

    /* ========== PROGRAM (top-level) ========== */

    /* Program element: skip followed by expr */
    PikaClause* prog_elem_arr[] = {
        pika_clause_rule_ref("skip"),
        pika_clause_ast_label("elem", pika_clause_rule_ref("expr"))
    };
    PikaClause* prog_elem = pika_clause_seq(prog_elem_arr, 2);

    /* Program: skip (skip expr)* skip */
    PikaClause* program_arr[] = {
        pika_clause_rule_ref("skip"),
        pika_clause_zero_or_more(prog_elem),
        pika_clause_rule_ref("skip")
    };
    PikaClause* program_clause = pika_clause_ast_label("program",
        pika_clause_seq(program_arr, 3)
    );

    /* ========== BUILD GRAMMAR ========== */

    /* Note: Rules are defined in dependency order for clarity,
     * but pika resolves rule_refs by name so order doesn't matter */
    PikaRule* rules[] = {
        pika_rule("ws_char", ws_char_clause),
        pika_rule("comment", comment_clause),
        pika_rule("skip", skip_clause),
        pika_rule("atom", atom_clause),
        pika_rule("expr", expr_clause),
        pika_rule("program", program_clause)
    };

    g_omni_grammar = pika_grammar_new(rules, 6);
    return g_omni_grammar;
}

void omni_grammar_cleanup(void) {
    if (g_omni_grammar) {
        pika_grammar_free(g_omni_grammar);
        g_omni_grammar = NULL;
    }
}

/* ============== Parsing API ============== */

OmniParseResult omni_pika_parse(const char* input, OmniOutputMode mode) {
    OmniParseResult result = {0};

    PikaGrammar* grammar = omni_get_grammar();
    if (!grammar) {
        result.error = strdup("Failed to initialize grammar");
        return result;
    }

    PikaMemoTable* memo = pika_grammar_parse(grammar, input);
    if (!memo) {
        result.error = strdup("Parse failed: out of memory");
        return result;
    }

    /* Get matches for expr rule */
    size_t match_count = 0;
    PikaMatch** matches = pika_memo_get_non_overlapping_matches_for_rule(
        memo, "expr", &match_count
    );

    if (match_count == 0 || !matches) {
        result.error = strdup("No expression found");
        pika_memo_free(memo);
        return result;
    }

    /* Use first match */
    PikaMatch* match = matches[0];

    if (mode == OMNI_OUTPUT_AST || mode == OMNI_OUTPUT_BOTH) {
        result.ast = match_to_ast(match, input);
    }

    if (mode == OMNI_OUTPUT_STRING || mode == OMNI_OUTPUT_BOTH) {
        result.str = match_to_string(match, input);
    }

    result.success = 1;

    free(matches);
    pika_memo_free(memo);
    return result;
}

OmniParseResult omni_pika_parse_all(const char* input, OmniOutputMode mode) {
    OmniParseResult result = {0};

    PikaGrammar* grammar = omni_get_grammar();
    if (!grammar) {
        result.error = strdup("Failed to initialize grammar");
        return result;
    }

    PikaMemoTable* memo = pika_grammar_parse(grammar, input);
    if (!memo) {
        result.error = strdup("Parse failed: out of memory");
        return result;
    }

    /* Get matches for program rule */
    size_t match_count = 0;
    PikaMatch** matches = pika_memo_get_non_overlapping_matches_for_rule(
        memo, "program", &match_count
    );

    if (match_count == 0 || !matches) {
        result.error = strdup("Parse failed");
        pika_memo_free(memo);
        return result;
    }

    PikaMatch* match = matches[0];

    if (mode == OMNI_OUTPUT_AST || mode == OMNI_OUTPUT_BOTH) {
        /* Extract all expr elements from program */
        size_t sub_count = 0;
        PikaLabeledMatch* subs = pika_match_get_submatches(match, &sub_count);

        Value* exprs = mk_nil();
        Value** tail = &exprs;

        for (size_t i = 0; i < sub_count; i++) {
            if (subs[i].label && strcmp(subs[i].label, "elem") == 0) {
                Value* expr = match_to_ast(subs[i].match, input);
                Value* cell = mk_cell(expr, mk_nil());
                *tail = cell;
                tail = &cell->cell.cdr;
            }
        }

        result.ast = exprs;
        free(subs);
    }

    if (mode == OMNI_OUTPUT_STRING || mode == OMNI_OUTPUT_BOTH) {
        result.str = match_to_string(match, input);
    }

    result.success = 1;

    free(matches);
    pika_memo_free(memo);
    return result;
}

OmniParseResult omni_pika_parse_with_grammar(
    PikaGrammar* grammar,
    const char* start_rule,
    const char* input,
    OmniOutputMode mode
) {
    OmniParseResult result = {0};

    if (!grammar) {
        result.error = strdup("NULL grammar");
        return result;
    }

    PikaMemoTable* memo = pika_grammar_parse(grammar, input);
    if (!memo) {
        result.error = strdup("Parse failed: out of memory");
        return result;
    }

    size_t match_count = 0;
    PikaMatch** matches = pika_memo_get_non_overlapping_matches_for_rule(
        memo, start_rule, &match_count
    );

    if (match_count == 0 || !matches) {
        result.error = strdup("No match found");
        pika_memo_free(memo);
        return result;
    }

    PikaMatch* match = matches[0];

    if (mode == OMNI_OUTPUT_AST || mode == OMNI_OUTPUT_BOTH) {
        result.ast = match_to_ast(match, input);
    }

    if (mode == OMNI_OUTPUT_STRING || mode == OMNI_OUTPUT_BOTH) {
        result.str = match_to_string(match, input);
    }

    result.success = 1;

    free(matches);
    pika_memo_free(memo);
    return result;
}

void omni_parse_result_free(OmniParseResult* result) {
    if (!result) return;
    if (result->str) free(result->str);
    if (result->error) free(result->error);
    /* Note: ast is managed by GC/ASAP, not freed here */
}

/* ============== String Operations ============== */

Value* omni_pika_match(const char* pattern, const char* input) {
    char* error = NULL;
    PikaGrammar* grammar = omni_compile_pattern(pattern, &error);
    if (!grammar) {
        if (error) free(error);
        return mk_nil();
    }

    PikaMemoTable* memo = pika_grammar_parse(grammar, input);
    if (!memo) {
        pika_grammar_free(grammar);
        return mk_nil();
    }

    size_t match_count = 0;
    PikaMatch** matches = pika_memo_get_non_overlapping_matches_for_rule(
        memo, "pattern", &match_count
    );

    if (match_count == 0 || !matches) {
        pika_memo_free(memo);
        pika_grammar_free(grammar);
        return mk_nil();
    }

    /* Return matched text */
    PikaMatch* match = matches[0];
    int start = pika_match_start(match);
    int len = pika_match_len(match);

    char* text = malloc(len + 1);
    memcpy(text, input + start, len);
    text[len] = '\0';

    Value* result = mk_code(text);
    free(text);
    free(matches);
    pika_memo_free(memo);
    pika_grammar_free(grammar);

    return result;
}

Value* omni_pika_split(const char* pattern, const char* input) {
    char* error = NULL;
    PikaGrammar* grammar = omni_compile_pattern(pattern, &error);
    if (!grammar) {
        if (error) free(error);
        /* Return input as single element if pattern fails */
        return mk_cell(mk_code(input), mk_nil());
    }

    PikaMemoTable* memo = pika_grammar_parse(grammar, input);
    if (!memo) {
        pika_grammar_free(grammar);
        return mk_cell(mk_code(input), mk_nil());
    }

    size_t match_count = 0;
    PikaMatch** matches = pika_memo_get_non_overlapping_matches_for_rule(
        memo, "pattern", &match_count
    );

    if (match_count == 0 || !matches) {
        pika_memo_free(memo);
        pika_grammar_free(grammar);
        return mk_cell(mk_code(input), mk_nil());
    }

    /* Split by matches */
    Value* result = mk_nil();
    Value** tail = &result;
    size_t input_len = strlen(input);
    size_t pos = 0;

    for (size_t i = 0; i < match_count; i++) {
        int start = pika_match_start(matches[i]);
        int len = pika_match_len(matches[i]);

        /* Add text before match */
        if ((size_t)start > pos) {
            char* seg = malloc(start - pos + 1);
            memcpy(seg, input + pos, start - pos);
            seg[start - pos] = '\0';
            Value* cell = mk_cell(mk_code(seg), mk_nil());
            free(seg);
            *tail = cell;
            tail = &cell->cell.cdr;
        }

        pos = start + len;
    }

    /* Add remaining text */
    if (pos < input_len) {
        char* seg = malloc(input_len - pos + 1);
        memcpy(seg, input + pos, input_len - pos);
        seg[input_len - pos] = '\0';
        Value* cell = mk_cell(mk_code(seg), mk_nil());
        free(seg);
        *tail = cell;
    }

    free(matches);
    pika_memo_free(memo);
    pika_grammar_free(grammar);

    return result;
}

Value* omni_pika_replace(const char* pattern, const char* replacement,
                         const char* input, int global) {
    char* error = NULL;
    PikaGrammar* grammar = omni_compile_pattern(pattern, &error);
    if (!grammar) {
        if (error) free(error);
        return mk_code(input);
    }

    PikaMemoTable* memo = pika_grammar_parse(grammar, input);
    if (!memo) {
        pika_grammar_free(grammar);
        return mk_code(input);
    }

    size_t match_count = 0;
    PikaMatch** matches = pika_memo_get_non_overlapping_matches_for_rule(
        memo, "pattern", &match_count
    );

    if (match_count == 0 || !matches) {
        pika_memo_free(memo);
        pika_grammar_free(grammar);
        return mk_code(input);
    }

    /* Build result with replacements */
    size_t input_len = strlen(input);
    size_t repl_len = strlen(replacement);
    size_t result_size = input_len + match_count * repl_len + 1;
    char* result = malloc(result_size);
    size_t result_pos = 0;
    size_t pos = 0;

    size_t limit = global ? match_count : 1;
    for (size_t i = 0; i < limit; i++) {
        int start = pika_match_start(matches[i]);
        int len = pika_match_len(matches[i]);

        /* Copy text before match */
        if ((size_t)start > pos) {
            memcpy(result + result_pos, input + pos, start - pos);
            result_pos += start - pos;
        }

        /* Copy replacement */
        memcpy(result + result_pos, replacement, repl_len);
        result_pos += repl_len;

        pos = start + len;
    }

    /* Copy remaining */
    if (pos < input_len) {
        memcpy(result + result_pos, input + pos, input_len - pos);
        result_pos += input_len - pos;
    }
    result[result_pos] = '\0';

    Value* val = mk_code(result);
    free(result);
    free(matches);
    pika_memo_free(memo);
    pika_grammar_free(grammar);

    return val;
}

Value* omni_pika_find_all(const char* pattern, const char* input) {
    char* error = NULL;
    PikaGrammar* grammar = omni_compile_pattern(pattern, &error);
    if (!grammar) {
        if (error) free(error);
        return mk_nil();
    }

    PikaMemoTable* memo = pika_grammar_parse(grammar, input);
    if (!memo) {
        pika_grammar_free(grammar);
        return mk_nil();
    }

    size_t match_count = 0;
    PikaMatch** matches = pika_memo_get_non_overlapping_matches_for_rule(
        memo, "pattern", &match_count
    );

    if (match_count == 0 || !matches) {
        pika_memo_free(memo);
        pika_grammar_free(grammar);
        return mk_nil();
    }

    /* Build list of matches */
    Value* result = mk_nil();
    Value** tail = &result;

    for (size_t i = 0; i < match_count; i++) {
        int start = pika_match_start(matches[i]);
        int len = pika_match_len(matches[i]);

        char* text = malloc(len + 1);
        memcpy(text, input + start, len);
        text[len] = '\0';

        Value* cell = mk_cell(mk_code(text), mk_nil());
        free(text);
        *tail = cell;
        tail = &cell->cell.cdr;
    }

    free(matches);
    pika_memo_free(memo);
    pika_grammar_free(grammar);

    return result;
}

/* ============== PEG DSL Compilation ============== */

PikaGrammar* omni_compile_peg(const char* peg_source, char** error_out) {
    /* Use pika_meta_parse from pika_c */
    return pika_meta_parse(peg_source, error_out);
}

/* Simple pattern compiler for regex-like patterns */
PikaGrammar* omni_compile_pattern(const char* pattern, char** error_out) {
    /* Convert simple pattern to PEG and compile */
    /* For now, just wrap in a simple grammar */

    /* Build pattern clause by parsing the pattern string */
    /* This is a simplified implementation - real one would parse regex syntax */

    size_t len = strlen(pattern);
    PikaClause* clause;

    /* Check for character class [abc] or [a-z] */
    if (len >= 3 && pattern[0] == '[' && pattern[len-1] == ']') {
        char* inner = malloc(len - 1);
        memcpy(inner, pattern + 1, len - 2);
        inner[len - 2] = '\0';
        clause = pika_clause_charset_from_pattern(inner);
        free(inner);
    }
    /* Check for literal with + */
    else if (len >= 2 && pattern[len-1] == '+') {
        char* base = malloc(len);
        memcpy(base, pattern, len - 1);
        base[len - 1] = '\0';

        if (base[0] == '[' && base[strlen(base)-1] == ']') {
            char* inner = malloc(strlen(base) - 1);
            memcpy(inner, base + 1, strlen(base) - 2);
            inner[strlen(base) - 2] = '\0';
            clause = pika_clause_one_or_more(pika_clause_charset_from_pattern(inner));
            free(inner);
        } else {
            clause = pika_clause_one_or_more(pika_clause_str(base));
        }
        free(base);
    }
    /* Check for literal with * */
    else if (len >= 2 && pattern[len-1] == '*') {
        char* base = malloc(len);
        memcpy(base, pattern, len - 1);
        base[len - 1] = '\0';

        if (base[0] == '[' && base[strlen(base)-1] == ']') {
            char* inner = malloc(strlen(base) - 1);
            memcpy(inner, base + 1, strlen(base) - 2);
            inner[strlen(base) - 2] = '\0';
            clause = pika_clause_zero_or_more(pika_clause_charset_from_pattern(inner));
            free(inner);
        } else {
            clause = pika_clause_zero_or_more(pika_clause_str(base));
        }
        free(base);
    }
    /* Literal string */
    else {
        clause = pika_clause_str(pattern);
    }

    PikaRule* rules[] = {
        pika_rule("pattern", clause)
    };

    return pika_grammar_new(rules, 1);
}
