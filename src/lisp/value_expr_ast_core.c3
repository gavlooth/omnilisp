module lisp;

import std::collections::list;
import std::io;
import main;

// =============================================================================
// SECTION 6.5: PATTERN MATCHING
// =============================================================================

/**
 * PatternTag — Type of pattern.
 */
enum PatternTag : char {
    PAT_WILDCARD,   // _ - matches anything, binds nothing
    PAT_VAR,        // x - matches anything, binds to x
    PAT_LIT,        // 42, "hello", 'sym - matches literal value
    PAT_CONS,       // (head . tail) - matches cons cell
    PAT_SEQ,        // [a b c], [head .. tail], [x y ..] - sequence pattern
    PAT_AS,         // (pattern 'as name) - bind whole matched value too
    PAT_QUOTE,      // 'symbol or 'literal - matches quoted datum
    PAT_CONSTRUCTOR, // (TypeName sub-patterns...) - matches type instance
    PAT_GUARD,      // (? pred) or (? pred sub-pattern) - guarded pattern
    PAT_DICT,       // {name age} - dict destructuring, binds 'name and 'age keys
}

/**
 * RestPosition — Where the rest pattern (..) appears in a sequence.
 */
enum RestPosition : char {
    REST_NONE,      // No rest: [a b c] - exact match
    REST_START,     // [.. last] - skip all but last
    REST_MIDDLE,    // [head .. tail] - first + rest
    REST_END,       // [x y ..] - first N, ignore rest
}

/**
 * Pattern — Pattern AST node.
 * Note: PatternSeq and PatternCons are embedded inside to avoid forward declaration issues.
 */
struct Pattern {
    PatternTag tag;

    union {
        SymbolId var_name;        // PAT_VAR
        Value* lit_value;         // PAT_LIT
        Value* quote_datum;       // PAT_QUOTE

        // PAT_CONS: cons pattern (head . tail)
        struct {
            Pattern* car_pat;
            Pattern* cdr_pat;
        }

        // PAT_SEQ: sequence pattern [a b c], [head .. tail], etc.
        struct {
            Pattern** elements;
            usz elem_count;
            RestPosition rest_pos;
            SymbolId rest_binding;  // Name to bind rest to (if REST_MIDDLE)
        }

        // PAT_AS: (pattern 'as name)
        struct {
            Pattern* as_sub;
            SymbolId as_name;
        }

        // PAT_CONSTRUCTOR: (TypeName sub-patterns...)
        struct {
            SymbolId constructor_name;
            Pattern** ctor_sub_patterns;
            usz ctor_sub_count;
        }

        // PAT_GUARD: (? pred) or (? pred sub-pattern)
        struct {
            Expr* guard_pred;       // Predicate expression
            Pattern* guard_sub;     // Optional sub-pattern (null = match anything)
        }

        // PAT_DICT: {name age} - extract keys from dict
        struct {
            SymbolId* dict_keys;    // Symbol IDs to look up (and bind to)
            usz dict_key_count;
        }
    }
}

/**
 * MatchClause — Single clause in a match expression.
 */
struct MatchClause {
    Pattern* pattern;
    Expr* result;
}

/**
 * ExprMatch — Pattern matching expression.
 * (match expr (pattern1 result1) (pattern2 result2) ...)
 */
struct ExprMatch {
    Expr* scrutinee;
    MatchClause* clauses;
    usz clause_count;
}

/**
 * ExprAnd — Short-circuit boolean and.
 * (and left right) - returns left if falsy, otherwise right
 */
struct ExprAnd {
    Expr* left;
    Expr* right;
}

/**
 * ExprOr — Short-circuit boolean or.
 * (or left right) - returns left if truthy, otherwise right
