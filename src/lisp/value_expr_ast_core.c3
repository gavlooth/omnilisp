module lisp;

import std::collections::list;
import std::io;
import main;

module lisp;

import std::collections::list;
import std::io;
import main;

// =============================================================================
// SECTION 6/7: AST nodes + effect handler structs
// =============================================================================
// =============================================================================
// SECTION 6: EXPRESSION AST
// =============================================================================

/**
 * ExprTag — Type of expression.
 */
enum ExprTag : char {
    E_LIT,        // Literal value
    E_VAR,        // Variable reference
    E_LAMBDA,     // (lambda (x) body)
    E_APP,        // (f arg)
    E_IF,         // (if test then else)
    E_LET,        // (let (x val) body)
    E_DEFINE,     // (define name val)
    E_QUOTE,      // (quote datum)
    E_RESET,      // (checkpoint body)
    E_SHIFT,      // (capture k body)
    E_PERFORM,    // (perform tag arg) or (signal tag arg)
    E_HANDLE,     // (handle body (tag arg body) ...)
    E_RESOLVE,    // (resolve value) — resume continuation in handler clause
    E_INDEX,      // arr.[0] - path index access
    E_PATH,       // point.x, person.address.city - field access
    E_MATCH,      // (match expr (pattern result) ...)
    E_AND,        // (and left right) - short-circuit boolean and
    E_OR,         // (or left right) - short-circuit boolean or
    E_CALL,       // (f a b c) - multi-arg call (not curried)
    E_BLOCK,      // (block e1 e2 ... en) - sequence, returns last
    E_SET,        // (set! name value) - variable mutation
    E_QUASIQUOTE, // `template - quasiquote
    E_UNQUOTE,    // ,expr - unquote (inside quasiquote)
    E_UNQUOTE_SPLICING, // ,@expr - unquote-splicing (inside quasiquote)
    E_DEFMACRO,   // (defmacro name transformer) - macro definition
    E_MODULE,     // (module name (export ...) body...) - module definition
    E_IMPORT,     // (import name) - module import
    E_DEFTYPE,    // (define [type] Name fields...)
    E_DEFABSTRACT, // (define [abstract] Name [Parent])
    E_DEFUNION,   // (define [union] Name variants...)
    E_DEFALIAS,   // (define [alias] Name Target)
    E_DEFEFFECT,  // (define [effect] (tag (^Type arg)))
    E_EXPORT_FROM, // (export-from mod (sym1 sym2)) or (export-from mod 'all)
    E_FFI_LIB,    // (define [ffi lib] name "path.so")
    E_FFI_FN,     // (define [ffi λ libname] (fname (^T arg)...) ^RetT)
}

struct ExprLit {
    Value* value;
}

struct ExprVar {
    SymbolId name;
}

struct ExprLambda {
    SymbolId param;           // First/only param (or unused if param_count==0)
    SymbolId* params;         // All fixed params (malloc'd)
    usz      param_count;     // Number of fixed params
    bool     has_rest;        // True if variadic (.. rest)
    SymbolId rest_param;      // Rest parameter name
    Expr*    body;
    TypeAnnotation* param_annotations;  // Type annotations for params (malloc'd, null if none)
    bool has_typed_params;    // True if any param has a type annotation
    bool is_accessor_shorthand; // True only for leading-dot lookup lambdas like .name or .3
}

struct ExprApp {
    Expr* func;
    Expr* arg;
}

struct ExprIf {
    Expr* test;
    Expr* then_branch;
    Expr* else_branch;
}

struct ExprLet {
    SymbolId name;
    Expr*    init;
    Expr*    body;
    bool     is_recursive;  // true for (let ^rec ...) recursive bindings
    Pattern* pattern;       // non-null for destructuring: (let ([x y] init) body)
}

struct ExprDefine {
    SymbolId name;
    Expr*    value;
}

struct ExprQuote {
    Value* datum;
}

struct ExprReset {
    Expr* body;
}

struct ExprShift {
    SymbolId k_name;   // Name to bind continuation
    Expr*    body;
}

struct ExprPerform {
    SymbolId tag;
    Expr*    arg;
}

struct ExprResolve {
    Expr* value;   // the value to resolve with (resumes the continuation)
}

struct EffectClause {
    SymbolId effect_tag;    // Effect name
    SymbolId k_name;        // Continuation param
    SymbolId arg_name;      // Argument param
    Expr*    handler_body;
}

struct ExprHandle {
    Expr*          body;
    EffectClause*  clauses;
    usz            clause_count;
    bool           strict_mode;   // ^strict annotation
}

/**
 * ExprIndex — Index access expression: arr.[0], dict.['key]
 * OmniLisp uses dot-bracket notation for indexing.
 */
struct ExprIndex {
    Expr* collection;  // The thing being indexed
    Expr* index;       // The index expression
}

/**
 * ExprPath — Field access path: point.x, person.address.city
 */
struct ExprPath {
    SymbolId* segments;
    usz segment_count;
}

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
