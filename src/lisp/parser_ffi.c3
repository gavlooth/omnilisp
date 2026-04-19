module lisp;

import std::collections::list;

bool g_parser_force_ffi_fn_alloc_fail = false;

fn void parser_free_ffi_fn_partial(ExprFfiFn* ffi) {
    if (ffi == null) return;
    // FFI AST payloads are arena-backed and are reclaimed with whole-AST teardown.
    ffi.c_name = null;
    ffi.param_names = null;
    ffi.param_types = null;
}

fn ExprFfiFn* Parser.alloc_ffi_fn(Parser* self, SymbolId lib_name) {
    ExprFfiFn* ffi = g_parser_force_ffi_fn_alloc_fail
        ? null
        : (ExprFfiFn*)self.alloc_ast_bytes(ExprFfiFn.sizeof, "out of memory while creating FFI function expression");
    if (ffi == null) {
        self.set_error("out of memory while creating FFI function expression");
        return null;
    }
    ffi.lib_name = lib_name;
    ffi.is_variadic = false;
    ffi.has_return_type = false;
    ffi.c_name = null;
    ffi.c_name_len = 0;
    ffi.param_count = 0;
    ffi.param_capacity = 0;
    ffi.param_names = null;
    ffi.param_types = null;
    return ffi;
}

fn bool Parser.parse_ffi_fn_signature(Parser* self, ExprFfiFn* ffi) {
    if (self.lexer.current.type != T_LPAREN) {
        self.set_error("expected ( for FFI function signature");
        return false;
    }
    self.lexer.advance();  // consume '('

    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected function name in FFI signature");
        return false;
    }
    ffi.fn_name = self.get_current_symbol();

    if (!self.set_ffi_c_name(ffi)) return false;
    self.lexer.advance();

    if (!self.parse_ffi_typed_params(ffi)) return false;

    self.expect(T_RPAREN, ")");
    return !self.has_error;
}

fn bool Parser.parse_ffi_optional_return_annotation(Parser* self, ExprFfiFn* ffi) {
    if (self.lexer.current.type == T_SYMBOL && self.lexer.current.text[0] == '^') {
        ffi.return_type = self.parse_type_annotation();
        ffi.has_return_type = ffi.return_type.has_annotation;
    }
    return !self.has_error;
}

// (define [ffi lib] name "path.so")
fn Expr* Parser.parse_ffi_lib(Parser* self, Expr* e) {
    e.tag = E_FFI_LIB;

    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected library name after [ffi lib]");
        return null;
    }
    e.ffi_lib.name = self.get_current_symbol();
    self.lexer.advance();

    e.ffi_lib.path_expr = self.parse_expr();
    if (e.ffi_lib.path_expr == null) {
        self.set_error("expected library path string");
        return null;
    }

    self.expect(T_RPAREN, ")");  // close define
    return e;
}

fn Expr* Parser.parse_ffi_fn(Parser* self, Expr* e, List{SymbolId}* attrs) {
    // attrs[0] = ffi, attrs[1] = λ/lambda, attrs[2] = library name
    if (attrs.len() < 3) {
        self.set_error("expected library name: [ffi λ libname]");
        return null;
    }

    ExprFfiFn* ffi = self.alloc_ffi_fn((*attrs)[2]);
    if (ffi == null) return null;
    self.parse_ffi_fn_modifiers(ffi, attrs);

    if (!self.parse_ffi_fn_signature(ffi)) {
        parser_free_ffi_fn_partial(ffi);
        return null;
    }

    if (!self.parse_ffi_optional_return_annotation(ffi)) {
        parser_free_ffi_fn_partial(ffi);
        return null;
    }

    self.expect(T_RPAREN, ")");
    if (self.has_error) {
        parser_free_ffi_fn_partial(ffi);
        return null;
    }

    e.tag = E_FFI_FN;
    e.ffi_fn = ffi;
    return e;
}

// (define [ffi module] libname "path.so" (fname (^Type arg)...) ^RetType ...)
fn Expr* Parser.parse_ffi_module(Parser* self, Expr* e, List{SymbolId}* attrs) {
    if (attrs.len() != 2) {
        self.set_error("[ffi module] does not accept extra attributes");
        return null;
    }
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected library name after [ffi module]");
        return null;
    }
    SymbolId lib_name = self.get_current_symbol();
    self.lexer.advance();

    Expr* lib_expr = self.alloc_expr_here();
    if (lib_expr == null) return null;
    lib_expr.tag = E_FFI_LIB;
    lib_expr.ffi_lib.name = lib_name;
    lib_expr.ffi_lib.path_expr = self.parse_expr();
    if (lib_expr.ffi_lib.path_expr == null) {
        self.set_error("expected library path string");
        return null;
    }

    List{Expr*} items;
    items.push(lib_expr);

    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        Expr* fn_expr = self.alloc_expr_here();
        if (fn_expr == null) { items.free(); return null; }

        ExprFfiFn* ffi = self.alloc_ffi_fn(lib_name);
        if (ffi == null) { items.free(); return null; }

        if (!self.parse_ffi_fn_signature(ffi)) {
            parser_free_ffi_fn_partial(ffi);
            items.free();
            return null;
        }

        if (self.lexer.current.type != T_SYMBOL || self.lexer.current.text[0] != '^') {
            self.set_error("expected return type annotation after [ffi module] function signature");
            parser_free_ffi_fn_partial(ffi);
            items.free();
            return null;
        }
        if (!self.parse_ffi_optional_return_annotation(ffi) || !ffi.has_return_type) {
            parser_free_ffi_fn_partial(ffi);
            items.free();
            return null;
        }

        fn_expr.tag = E_FFI_FN;
        fn_expr.ffi_fn = ffi;
        items.push(fn_expr);
    }

    if (self.has_error) { items.free(); return null; }
    self.expect(T_RPAREN, ")");
    if (self.has_error) { items.free(); return null; }

    e.tag = E_BLOCK;
    e.block = (ExprBlock*)self.alloc_ast_bytes(ExprBlock.sizeof, "out of memory while allocating FFI module expansion");
    if (e.block == null) { items.free(); return null; }
    e.block.expr_count = items.len();
    e.block.exprs = (Expr**)self.alloc_ast_array_bytes(Expr*.sizeof, items.len(), "out of memory while allocating FFI module expansion entries");
    if (e.block.exprs == null && items.len() > 0) { items.free(); return null; }
    for (usz i = 0; i < items.len(); i++) e.block.exprs[i] = items[i];
    items.free();
    return e;
}
