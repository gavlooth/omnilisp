module lisp;

import std::collections::list;
import std::io;
import main;

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
    Expr*    tag_expr;
    bool     has_tag_expr;
    Expr*    arg;
}

struct ExprResolve {
    Expr* value;   // the value to resolve with (resumes the continuation)
}

struct EffectClause {
    SymbolId effect_tag;        // Effect name when parsed as bare symbol
    Expr*    effect_tag_expr;   // Resolved expression for qualified module tags
    bool     has_effect_tag_expr;
    SymbolId k_name;            // Continuation param
    SymbolId arg_name;          // Argument param
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
