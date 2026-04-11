module lisp;

import std::collections::list;

bool g_parser_force_ffi_param_names_alloc_fail = false;
bool g_parser_force_ffi_param_types_alloc_fail = false;
bool g_parser_force_ffi_param_table_size_overflow = false;

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
    char* c_name = (char*)self.alloc_ast_bytes(clen + 1, "out of memory while parsing FFI symbol name");
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

    if (g_parser_force_ffi_param_table_size_overflow
        || new_cap > usz.max / SymbolId.sizeof
        || new_cap > usz.max / TypeAnnotation.sizeof) {
        self.set_error("FFI parameter table size overflow");
        return false;
    }

    usz name_bytes = SymbolId.sizeof * new_cap;
    usz type_bytes = TypeAnnotation.sizeof * new_cap;

    SymbolId* grown_names = g_parser_force_ffi_param_names_alloc_fail
        ? null
        : (SymbolId*)self.alloc_ast_bytes(name_bytes, "out of memory while allocating FFI parameter name table");
    if (grown_names == null) {
        self.set_error("out of memory while allocating FFI parameter name table");
        return false;
    }

    TypeAnnotation* grown_types = g_parser_force_ffi_param_types_alloc_fail
        ? null
        : (TypeAnnotation*)self.alloc_ast_bytes(type_bytes, "out of memory while allocating FFI parameter type table");
    if (grown_types == null) {
        self.set_error("out of memory while allocating FFI parameter type table");
        return false;
    }

    for (usz i = 0; i < ffi.param_count; i++) {
        grown_names[i] = ffi.param_names[i];
        grown_types[i] = ffi.param_types[i];
    }

    ffi.param_names = grown_names;
    ffi.param_types = grown_types;
    ffi.param_capacity = new_cap;
    return true;
}

fn bool Parser.parse_ffi_typed_params(Parser* self, ExprFfiFn* ffi) {
    while (self.lexer.current.type == T_LPAREN && !self.has_error) {
        if (ffi.param_count == usz.max) {
            self.set_error("FFI parameter count overflow");
            return false;
        }
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
