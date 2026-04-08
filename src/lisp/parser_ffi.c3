module lisp;

import std::collections::list;
import std::core::mem;

bool g_parser_force_ffi_fn_alloc_fail = false;

fn void parser_free_ffi_fn_partial(ExprFfiFn* ffi) {
    if (ffi == null) return;
    if (ffi.c_name != null) {
        mem::free(ffi.c_name);
        ffi.c_name = null;
    }
    if (ffi.param_names != null) {
        mem::free(ffi.param_names);
        ffi.param_names = null;
    }
    if (ffi.param_types != null) {
        mem::free(ffi.param_types);
        ffi.param_types = null;
    }
    mem::free(ffi);
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

    ExprFfiFn* ffi = g_parser_force_ffi_fn_alloc_fail
        ? null
        : (ExprFfiFn*)mem::malloc(ExprFfiFn.sizeof);
    if (ffi == null) {
        self.set_error("out of memory while creating FFI function expression");
        return null;
    }
    ffi.lib_name = (*attrs)[2];
    ffi.is_variadic = false;
    ffi.has_return_type = false;
    ffi.c_name = null;
    ffi.c_name_len = 0;
    ffi.param_count = 0;
    ffi.param_capacity = 0;
    ffi.param_names = null;
    ffi.param_types = null;

    self.parse_ffi_fn_modifiers(ffi, attrs);

    if (self.lexer.current.type != T_LPAREN) {
        self.set_error("expected ( for FFI function signature");
        parser_free_ffi_fn_partial(ffi);
        return null;
    }
    self.lexer.advance();  // consume '('

    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected function name in FFI signature");
        parser_free_ffi_fn_partial(ffi);
        return null;
    }
    ffi.fn_name = self.get_current_symbol();

    if (!self.set_ffi_c_name(ffi)) {
        parser_free_ffi_fn_partial(ffi);
        return null;
    }
    self.lexer.advance();

    if (!self.parse_ffi_typed_params(ffi)) {
        parser_free_ffi_fn_partial(ffi);
        return null;
    }

    self.expect(T_RPAREN, ")");
    if (self.has_error) {
        parser_free_ffi_fn_partial(ffi);
        return null;
    }

    if (self.lexer.current.type == T_SYMBOL && self.lexer.current.text[0] == '^') {
        ffi.return_type = self.parse_type_annotation();
        ffi.has_return_type = ffi.return_type.has_annotation;
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
