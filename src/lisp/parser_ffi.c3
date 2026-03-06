module lisp;

import std::collections::list;

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
fn void Parser.parse_ffi_fn_modifiers(Parser* self, ExprFfiFn* ffi, List{SymbolId}* attrs) {
    for (usz i = 3; i < attrs.len(); i++) {
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

fn bool Parser.set_ffi_c_name(Parser* self, ExprFfiFn* ffi) {
    char[] sym_text = self.interp.symbols.get_name(ffi.fn_name);
    usz clen = sym_text.len;
    if (clen == usz.max) {
        self.set_error("FFI symbol name is too long");
        return false;
    }
    char* c_name = (char*)mem::malloc(clen + 1);
    if (c_name == null) {
        self.set_error("out of memory while parsing FFI symbol name");
        return false;
    }
    for (usz i = 0; i < clen; i++) {
        if (sym_text[i] == '-') {
            c_name[i] = '_';
        } else {
            c_name[i] = sym_text[i];
        }
    }
    c_name[clen] = 0;
    ffi.c_name = c_name;
    ffi.c_name_len = clen;
    return true;
}

fn bool Parser.ensure_ffi_param_capacity(Parser* self, ExprFfiFn* ffi, usz needed) {
    if (needed <= ffi.param_capacity) return true;

    usz new_cap = ffi.param_capacity > 0 ? ffi.param_capacity : 4;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }

    SymbolId* new_names = (SymbolId*)mem::realloc(ffi.param_names, SymbolId.sizeof * new_cap);
    if (new_names == null) {
        self.set_error("out of memory while parsing FFI parameters");
        return false;
    }
    ffi.param_names = new_names;

    TypeAnnotation* new_types = (TypeAnnotation*)mem::realloc(ffi.param_types, TypeAnnotation.sizeof * new_cap);
    if (new_types == null) {
        self.set_error("out of memory while parsing FFI parameters");
        return false;
    }
    ffi.param_types = new_types;
    ffi.param_capacity = new_cap;
    return true;
}

fn bool Parser.parse_ffi_typed_params(Parser* self, ExprFfiFn* ffi) {
    while (self.lexer.current.type == T_LPAREN && !self.has_error) {
        if (!self.ensure_ffi_param_capacity(ffi, ffi.param_count + 1)) return false;
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

fn Expr* Parser.parse_ffi_fn(Parser* self, Expr* e, List{SymbolId}* attrs) {
    // attrs[0] = ffi, attrs[1] = λ/lambda, attrs[2] = library name
    if (attrs.len() < 3) {
        self.set_error("expected library name: [ffi λ libname]");
        return null;
    }

    ExprFfiFn* ffi = (ExprFfiFn*)mem::malloc(ExprFfiFn.sizeof);
    e.tag = E_FFI_FN;
    e.ffi_fn = ffi;

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
        return null;
    }
    self.lexer.advance();  // consume '('

    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected function name in FFI signature");
        return null;
    }
    ffi.fn_name = self.get_current_symbol();

    if (!self.set_ffi_c_name(ffi)) return null;
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
