module lisp;

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

// (define [ffi λ libname] (fname (^Type param)...) ^RetType)
fn void Parser.parse_ffi_fn_modifiers(Parser* self, ExprFfiFn* ffi, SymbolId[8]* attrs, usz attr_count) {
    for (usz i = 3; i < attr_count; i++) {
        char[] attr_name = self.interp.symbols.get_name((*attrs)[i]);
        if (attr_name.len == 8) {
            bool is_var = true;
            char[] vstr = "variadic";
            for (usz j = 0; j < 8; j++) {
                if (attr_name[j] != vstr[j]) { is_var = false; break; }
            }
            if (is_var) ffi.is_variadic = true;
        }
    }
}

fn void Parser.set_ffi_c_name(Parser* self, ExprFfiFn* ffi) {
    char[] sym_text = self.interp.symbols.get_name(ffi.fn_name);
    usz clen = sym_text.len;
    if (clen > 127) clen = 127;
    for (usz i = 0; i < clen; i++) {
        if (sym_text[i] == '-') {
            ffi.c_name[i] = '_';
        } else {
            ffi.c_name[i] = sym_text[i];
        }
    }
    ffi.c_name[clen] = 0;
    ffi.c_name_len = clen;
}

fn bool Parser.parse_ffi_typed_params(Parser* self, ExprFfiFn* ffi) {
    while (self.lexer.current.type == T_LPAREN && !self.has_error) {
        if (ffi.param_count >= 16) {
            self.set_error("too many FFI parameters (max 16)");
            return false;
        }
        self.lexer.advance();  // consume '('

        TypeAnnotation ann = self.parse_type_annotation();
        if (!ann.has_annotation) {
            self.set_error("expected type annotation in FFI parameter");
            return false;
        }
        ffi.param_types[ffi.param_count] = ann;

        if (self.lexer.current.type != T_SYMBOL) {
            self.set_error("expected parameter name after type annotation");
            return false;
        }
        ffi.param_names[ffi.param_count] = self.get_current_symbol();
        self.lexer.advance();
        self.expect(T_RPAREN, ")");  // close param
        ffi.param_count++;
    }
    return !self.has_error;
}

fn Expr* Parser.parse_ffi_fn(Parser* self, Expr* e, SymbolId[8]* attrs, usz attr_count) {
    // attrs[0] = ffi, attrs[1] = λ/lambda, attrs[2] = library name
    if (attr_count < 3) {
        self.set_error("expected library name: [ffi λ libname]");
        return null;
    }

    ExprFfiFn* ffi = (ExprFfiFn*)mem::malloc(ExprFfiFn.sizeof);
    e.tag = E_FFI_FN;
    e.ffi_fn = ffi;

    ffi.lib_name = (*attrs)[2];
    ffi.is_variadic = false;
    ffi.has_return_type = false;
    ffi.param_count = 0;

    self.parse_ffi_fn_modifiers(ffi, attrs, attr_count);

    if (self.lexer.current.type != T_LPAREN) {
        self.set_error("expected ( for FFI function signature");
        return null;
    }
    self.lexer.advance();  // consume '('

    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected function name in FFI signature");
        return null;
    }
    ffi.fn_name = self.get_current_symbol();

    self.set_ffi_c_name(ffi);
    self.lexer.advance();

    if (!self.parse_ffi_typed_params(ffi)) return null;

    self.expect(T_RPAREN, ")");  // close function signature

    if (self.lexer.current.type == T_SYMBOL && self.lexer.current.text[0] == '^') {
        ffi.return_type = self.parse_type_annotation();
        ffi.has_return_type = ffi.return_type.has_annotation;
    }

    self.expect(T_RPAREN, ")");  // close define
    return e;
}
