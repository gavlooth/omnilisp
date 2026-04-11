module lisp;

import std::core::mem;
import std::collections::list;

fn TypeAnnotation parser_empty_type_annotation() @inline {
    TypeAnnotation ann;
    ann.has_annotation = false;
    ann.is_compound = false;
    ann.is_dict = false;
    ann.base_type = 0;
    ann.params = null;
    ann.param_count = 0;
    ann.meta = null;
    ann.meta_count = 0;
    ann.val_literal.tag = NIL;
    ann.val_literal.int_value = 0;
    ann.val_literal.sym_value = 0;
    ann.has_val_literal = false;
    return ann;
}

fn bool parser_is_value_literal_ctor(Parser* self, SymbolId sym) @inline {
    return (uint)sym == (uint)self.interp.sym_Value;
}

fn bool parser_is_removed_val_ctor(Parser* self, SymbolId sym) @inline {
    char[] name = self.interp.symbols.get_name(sym);
    return name.len == 3 &&
           name[0] == 'V' &&
           name[1] == 'a' &&
           name[2] == 'l';
}

fn bool parser_is_bool_symbol(Parser* self, SymbolId sym) @inline {
    return (uint)sym == (uint)self.interp.sym_true || (uint)sym == (uint)self.interp.sym_false;
}

fn bool Parser.parse_value_literal_annotation(Parser* self, TypeAnnotation* ann) {
    if (self.lexer.current.type == T_INT) {
        ann.val_literal.tag = INT;
        ann.val_literal.int_value = self.lexer.current.int_value;
        ann.val_literal.sym_value = 0;
        ann.has_val_literal = true;
        self.lexer.advance();
        return true;
    }

    if (self.lexer.current.type == T_STRING) {
        ann.val_literal.tag = STRING;
        ann.val_literal.int_value = 0;
        ann.val_literal.sym_value = self.intern_or_error(self.lexer.current.text[:self.lexer.current.text_len], "failed to intern Value literal annotation");
        if (ann.val_literal.sym_value == INVALID_SYMBOL_ID) return false;
        ann.has_val_literal = true;
        self.lexer.advance();
        return true;
    }

    if (self.lexer.current.type == T_SYMBOL) {
        SymbolId sym = self.get_current_symbol();
        if (parser_is_bool_symbol(self, sym)) {
            ann.val_literal.tag = SYMBOL;
            ann.val_literal.int_value = 0;
            ann.val_literal.sym_value = sym;
            ann.has_val_literal = true;
            self.lexer.advance();
            return true;
        }
        ann.val_literal.tag = SYMBOL;
        ann.val_literal.int_value = 0;
        ann.val_literal.sym_value = sym;
        ann.has_val_literal = true;
        self.lexer.advance();
        return true;
    }

    if (self.lexer.current.type == T_QUOTE) {
        self.lexer.advance();
        if (self.lexer.current.type != T_SYMBOL) {
            self.set_error("Value literal dispatch expects quoted symbol after '");
            return false;
        }
        ann.val_literal.tag = SYMBOL;
        ann.val_literal.int_value = 0;
        ann.val_literal.sym_value = self.get_current_symbol();
        ann.has_val_literal = true;
        self.lexer.advance();
        return true;
    }

    self.set_error("Value literal dispatch expects int/string/symbol/bool literal");
    return false;
}

fn bool Parser.type_annotation_copy_params(Parser* self, TypeAnnotation* ann, List{SymbolId}* params) {
    if (params.len() == 0) return true;
    ann.params = (SymbolId*)self.alloc_ast_array_bytes(SymbolId.sizeof, params.len(), "out of memory while parsing type annotation");
    if (ann.params == null) return false;
    ann.param_count = params.len();
    for (usz i = 0; i < params.len(); i++) ann.params[i] = (*params)[i];
    return true;
}

fn bool Parser.type_annotation_copy_meta(Parser* self, TypeAnnotation* ann, List{MetaEntry}* meta) {
    if (meta.len() == 0) return true;
    ann.meta = (MetaEntry*)self.alloc_ast_array_bytes(MetaEntry.sizeof, meta.len(), "out of memory while parsing metadata dict");
    if (ann.meta == null) return false;
    ann.meta_count = meta.len();
    for (usz i = 0; i < meta.len(); i++) ann.meta[i] = (*meta)[i];
    return true;
}
