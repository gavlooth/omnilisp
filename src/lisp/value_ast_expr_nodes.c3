module lisp;

struct ExprOr {
    Expr* left;
    Expr* right;
}

/**
 * ExprCall — Multi-argument function call (not curried).
 * (f a b c) - calls f with all arguments at once.
 */
struct ExprCall {
    Expr* func;
    Expr** args;
    usz arg_count;
}

/**
 * ExprBlock — Sequence expression (block e1 e2 ... en).
 * Evaluates all expressions, returns the last.
 */
struct ExprBlock {
    Expr** exprs;
    usz expr_count;
}

/**
 * ExprSet — Variable mutation: (set! name value)
 */
struct ExprSet {
    SymbolId name;
    Expr* value;
    bool is_path;
    SymbolId* path_segments;
    usz path_segment_count;
}

/**
 * ExprQuasiquote — Quasiquote template: `expr
 */
struct ExprQuasiquote {
    Expr* body;
}

/**
 * ExprUnquote — Unquote inside quasiquote: ,expr
 */
struct ExprUnquote {
    Expr* body;
}

/**
 * ExprUnquoteSplicing — Splice inside quasiquote: ,@expr
 */
struct ExprUnquoteSplicing {
    Expr* body;
}

/**
 * ExprDefineMacro — Pattern-based macro definition:
 * (define [macro] name (syntax-match (pattern (template ...)) ...))
 */
struct ExprDefineMacro {
    SymbolId name;
    MacroClause* clauses;
    usz clause_count;
}

/**
 * ExprModule — Module definition: (module name (export sym...) body...)
 */
struct ExprModule {
    SymbolId name;
    SymbolId* exports;        // heap-allocated
    usz export_count;
    usz export_capacity;
    Expr** body;           // heap-allocated
    usz body_count;
    usz body_capacity;
}

/**
 * ExprImport — Module import: (import name) or (import "path")
 */
struct ExprImport {
    SymbolId name;        // module name (symbol)
    bool has_path;    // true if importing by file path
    char* path;        // heap-allocated file path (if has_path), NUL-terminated
    usz path_len;
    // Selective imports: (import mod (sym1 (sym2 'as alias2)))
    SymbolId* imports;       // Names to import into scope (heap-allocated)
    SymbolId* aliases;       // Rename targets (0 = no rename) (heap-allocated)
    usz import_count;  // 0 = qualified-only, >0 = selective
    usz import_capacity;
    bool import_all;    // true for quoted marker 'all
}

/**
 * ExprWithModule — Scoped module open: (with mod body...)
 */
struct ExprWithModule {
    ExprImport import_expr;
    Expr** body;
    usz body_count;
    usz body_capacity;
}

/**
 * ExprExportFrom — Re-export symbols from another module: (export-from mod (sym1 sym2)) or (export-from mod 'all)
 */
struct ExprExportFrom {
    SymbolId source_module;  // Module to re-export from
    SymbolId* names;          // Specific names (heap-allocated, empty if 'all)
    usz name_count;
    usz name_capacity;
    bool all;            // true for quoted marker 'all
}
