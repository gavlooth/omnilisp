module lisp;

import std::collections::list;
import std::io;
import main;

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
 * ExprBegin — Sequence expression (begin e1 e2 ... en).
 * Evaluates all expressions, returns the last.
 */
struct ExprBegin {
    Expr** exprs;
    usz expr_count;
}

/**
 * ExprSet — Variable mutation: (set! name value)
 */
struct ExprSet {
    SymbolId name;
    Expr*    value;
    bool     is_path;
    SymbolId* path_segments;
    usz      path_segment_count;
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
 * (define [macro] name (pattern template) ...)
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
    SymbolId     name;
    SymbolId*    exports;        // heap-allocated
    usz          export_count;
    usz          export_capacity;
    Expr**       body;           // heap-allocated
    usz          body_count;
    usz          body_capacity;
}

/**
 * ExprImport — Module import: (import name) or (import "path")
 */
struct ExprImport {
    SymbolId name;        // module name (symbol)
    bool     has_path;    // true if importing by file path
    char*    path;        // heap-allocated file path (if has_path), NUL-terminated
    usz      path_len;
    // Selective imports: (import mod (sym1 (sym2 'as alias2)))
    SymbolId*    imports;       // Names to import into scope (heap-allocated)
    SymbolId*    aliases;       // Rename targets (0 = no rename) (heap-allocated)
    usz          import_count;  // 0 = qualified-only, >0 = selective
    usz          import_capacity;
    bool         import_all;    // true for quoted marker 'all
}

/**
 * ExprExportFrom — Re-export symbols from another module: (export-from mod (sym1 sym2)) or (export-from mod 'all)
 */
struct ExprExportFrom {
    SymbolId     source_module;  // Module to re-export from
    SymbolId*    names;          // Specific names (heap-allocated, empty if 'all)
    usz          name_count;
    usz          name_capacity;
    bool         all;            // true for quoted marker 'all
}

// =============================================================================
// SECTION 6.6: FFI AST NODES
// =============================================================================

// FFI type tag — must stay in sync with csrc/ffi_helpers.c OMNI_FFI_* enum
// Values: VOID=0, INT=1, DOUBLE=2, PTR=3, BOOL=4
enum FfiTypeTag : char {
    FFI_TYPE_VOID,
    FFI_TYPE_INT,
    FFI_TYPE_DOUBLE,
    FFI_TYPE_PTR,
    FFI_TYPE_BOOL,
}

struct ExprFfiLib {
    SymbolId name;           // e.g. "libc"
    Expr* path_expr;         // string literal "libc.so.6"
}

struct ExprFfiFn {
    SymbolId lib_name;       // which library (e.g. "libc")
    SymbolId fn_name;        // Omni name (auto kebab-cased)
    char* c_name;            // heap-allocated C symbol name for dlsym, NUL-terminated
    usz c_name_len;
    usz param_count;
    usz param_capacity;
    SymbolId* param_names;
    TypeAnnotation* param_types;
    TypeAnnotation return_type;
    bool has_return_type;
    bool is_variadic;
}

// FfiBoundFn — runtime state for a bound FFI function (used as Primitive.user_data)
struct FfiBoundFn {
    void* fn_ptr;           // null until first call (lazy dlsym)
    void* lib_handle;       // dlopen handle for lazy resolution
    char* c_name;           // heap-allocated C symbol name for lazy dlsym, NUL-terminated
    usz c_name_len;
    usz param_count;
    FfiTypeTag* param_types;
    FfiTypeTag return_type;
    bool has_return;
    bool is_variadic;
}

// =============================================================================
// SECTION 6.7: TYPE DEFINITION AST NODES
// =============================================================================

/**
 * TypeField — Field in a type definition with optional type annotation.
 */
struct TypeField {
    SymbolId name;
    TypeAnnotation type_ann;
}

/**
 * ExprDefType — (define [type] Name fields...) or (define [type] (Name Parent) fields...)
 */
struct ExprDefType {
    SymbolId name;
    SymbolId parent;
    bool has_parent;
    TypeField* fields;
    usz field_count;
    SymbolId* type_params;
    usz type_param_count;
}

/**
 * ExprDefAbstract — (define [abstract] Name) or (define [abstract] (Name Parent))
 */
struct ExprDefAbstract {
    SymbolId name;
    SymbolId parent;
    bool has_parent;
}

/**
 * ExprDefUnion — (define [union] (Name T...) Variant1 (Variant2 T) ...)
 */
struct ExprDefUnion {
    SymbolId name;
    SymbolId* type_params;
    usz type_param_count;
    UnionVariant* variants;
    usz variant_count;
}

/**
 * ExprDefAlias — (define [alias] Name Target)
 */
struct ExprDefAlias {
    SymbolId name;
    TypeAnnotation target;
}

/**
 * ExprDefEffect — (define [effect] (tag (^Type arg)))
 */
struct ExprDefEffect {
    SymbolId name;           // Effect tag name (e.g., io/print)
    TypeAnnotation arg_type; // Declared argument type (^Any, ^String, etc.)
    bool has_arg_type;       // true if argument type annotation present
}

/**
 * Module — Loaded module record.
 */
struct Module {
    SymbolId     name;
    Env*         env;              // Module's internal environment
    SymbolId*    exports;          // Exported symbol names (heap-allocated)
    usz          export_count;
    usz          export_capacity;
    bool         loaded;           // false during loading (circular import detection)
    char*        path;             // Source file path (heap-allocated)
    usz          path_len;
}

/**
 * Expr — Expression AST node.
 */
struct Expr {
    ExprTag tag;
    usz     loc_line;     // Source line (1-indexed, 0 if unknown)
    usz     loc_column;   // Source column (1-indexed, 0 if unknown)

    union {
        ExprLit     lit;
        ExprVar     var_expr;
        ExprLambda* lambda;       // Pointer-indirect (large struct)
        ExprApp     app;
        ExprIf      if_expr;
        ExprLet     let_expr;
        ExprDefine  define;
        ExprQuote   quote;
        ExprReset   reset;
        ExprShift   shift;
        ExprPerform perform;
        ExprResolve resolve;
        ExprHandle* handle;       // Pointer-indirect (large struct)
        ExprIndex   index;
        ExprPath    path;
        ExprMatch*  match;        // Pointer-indirect (large struct)
        ExprAnd     and_expr;
        ExprOr      or_expr;
        ExprCall*   call;         // Pointer-indirect (large struct)
        ExprBegin*  begin;        // Pointer-indirect (large struct)
        ExprSet     set_expr;
        ExprQuasiquote   quasiquote;
        ExprUnquote      unquote;
        ExprUnquoteSplicing unquote_splicing;
        ExprDefineMacro  define_macro;
        ExprModule*      module_expr;  // Pointer-indirect (large struct)
        ExprImport       import_expr;
        ExprDefType*     deftype;      // Pointer-indirect (large struct)
        ExprDefAbstract  defabstract;
        ExprDefUnion*    defunion;     // Pointer-indirect (large struct)
        ExprDefAlias     defalias;
        ExprDefEffect    defeffect;
        ExprExportFrom   export_from;
        ExprFfiLib       ffi_lib;
        ExprFfiFn*       ffi_fn;       // Pointer-indirect (large struct)
    }
}

// =============================================================================
// SECTION 7: EFFECT HANDLERS
// =============================================================================

/**
 * EffectHandler - Active effect handler in the handler stack.
 */
struct EffectHandler {
    SymbolId* tags;
    EffectClause* clauses;
    Value** closures;
    usz clause_count;
    Env* handler_env;
    Expr* body_expr;    // The handle body for continuation capture
    Env*  body_env;     // Environment at handle time
    bool  strict_mode;  // ^strict: reject unhandled effects in body
    void* effect_state;  // HandleEffectState* (set by stack-context-based handle, null otherwise)
}

