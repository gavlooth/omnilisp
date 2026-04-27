module lisp;

import std::core::mem;
import std::collections::list;

fn TypeAnnotation parser_empty_type_annotation() @inline {
    TypeAnnotation ann;
    ann.has_annotation = false;
    ann.is_compound = false;
    ann.is_dict = false;
    ann.base_type = (SymbolId)0;
    ann.params = null;
    ann.param_count = 0;
    ann.meta = null;
    ann.meta_count = 0;
    ann.val_literal.tag = NIL;
    ann.val_literal.int_value = 0;
    ann.val_literal.sym_value = (SymbolId)0;
    ann.has_val_literal = false;
    return ann;
}

fn bool parser_is_literal_type_ctor(Parser* self, SymbolId sym) @inline {
    return (uint)sym == (uint)self.interp.sym_Literal;
}

fn bool parser_is_removed_value_literal_ctor(Parser* self, SymbolId sym) @inline {
    char[] name = self.interp.symbols.get_name(sym);
    if (name.len == 3 &&
        name[0] == 'V' &&
        name[1] == 'a' &&
        name[2] == 'l') return true;
    return name.len == 5 &&
           name[0] == 'V' &&
           name[1] == 'a' &&
           name[2] == 'l' &&
           name[3] == 'u' &&
           name[4] == 'e';
}

fn SymbolId parser_normalize_type_annotation_base(Parser* self, SymbolId sym) @inline {
    char[] name = self.interp.symbols.get_name(sym);
    if (name.len == 4 &&
        name[0] == 'D' &&
        name[1] == 'i' &&
        name[2] == 'c' &&
        name[3] == 't') {
        return self.interp.sym_Dictionary;
    }
    return sym;
}

fn bool parser_is_bool_symbol(Parser* self, SymbolId sym) @inline {
    return (uint)sym == (uint)self.interp.sym_true || (uint)sym == (uint)self.interp.sym_false;
}

fn bool parser_parse_inline_literal_annotation_token(Parser* self, TypeAnnotation* ann, char* text, usz text_len) {
    if (text == null || text_len <= 2) return false;
    usz start = 2;
    while (start < text_len && text[start] == 0) start++;
    if (start >= text_len) return false;
    usz len = text_len - start;

    bool negative = false;
    usz idx = start;
    if (text[idx] == '-') {
        negative = true;
        idx++;
        if (idx >= text_len) {
            self.set_error("Literal shorthand integer annotation expects digits after '-'");
            return false;
        }
    }

    bool all_digits = true;
    for (usz i = idx; i < text_len; i++) {
        if (text[i] < '0' || text[i] > '9') {
            all_digits = false;
            break;
        }
    }

    if (all_digits) {
        long value = 0;
        long max_prefix = long.max / 10;
        long max_final_digit = negative ? 8 : 7;
        bool is_long_min_literal = false;
        for (usz i = idx; i < text_len; i++) {
            long digit = (long)(text[i] - '0');
            if (is_long_min_literal ||
                value > max_prefix ||
                (value == max_prefix && digit > max_final_digit)) {
                self.set_error("Literal annotation integer overflow");
                return false;
            }
            if (negative && value == max_prefix && digit == 8) {
                is_long_min_literal = true;
            } else {
                value = value * 10 + digit;
            }
        }
        ann.val_literal.tag = INT;
        ann.val_literal.int_value = is_long_min_literal ? long.min : (negative ? -value : value);
        ann.val_literal.sym_value = (SymbolId)0;
        ann.has_val_literal = true;
        return true;
    }

    if (len == 3 && text[start] == 'n' && text[start + 1] == 'i' && text[start + 2] == 'l') {
        ann.val_literal.tag = NIL;
        ann.val_literal.int_value = 0;
        ann.val_literal.sym_value = (SymbolId)0;
        ann.has_val_literal = true;
        return true;
    }

    if (len == 4 && text[start] == 't' && text[start + 1] == 'r' && text[start + 2] == 'u' && text[start + 3] == 'e') {
        ann.val_literal.tag = SYMBOL;
        ann.val_literal.int_value = 0;
        ann.val_literal.sym_value = self.interp.sym_true;
        ann.has_val_literal = true;
        return true;
    }

    if (len == 5 && text[start] == 'f' && text[start + 1] == 'a' && text[start + 2] == 'l' && text[start + 3] == 's' && text[start + 4] == 'e') {
        ann.val_literal.tag = SYMBOL;
        ann.val_literal.int_value = 0;
        ann.val_literal.sym_value = self.interp.sym_false;
        ann.has_val_literal = true;
        return true;
    }

    self.set_error("Literal shorthand symbol annotations use ^#'symbol");
    return false;
}

fn bool Parser.parse_literal_annotation(Parser* self, TypeAnnotation* ann) {
    if (self.lexer.current.type == T_INT) {
        ann.val_literal.tag = INT;
        ann.val_literal.int_value = self.lexer.current.int_value;
        ann.val_literal.sym_value = (SymbolId)0;
        ann.has_val_literal = true;
        self.lexer.advance();
        return true;
    }

    if (self.lexer.current.type == T_STRING) {
        ann.val_literal.tag = STRING;
        ann.val_literal.int_value = 0;
        ann.val_literal.sym_value = self.intern_or_error(self.lexer.current.text[:self.lexer.current.text_len], "failed to intern Literal annotation");
        if (ann.val_literal.sym_value == INVALID_SYMBOL_ID) return false;
        ann.has_val_literal = true;
        self.lexer.advance();
        return true;
    }

    if (self.lexer.current.type == T_SYMBOL) {
        SymbolId sym = self.get_current_symbol();
        char[] sym_name = self.interp.symbols.get_name(sym);
        if (sym_name.len == 3 &&
            sym_name[0] == 'n' &&
            sym_name[1] == 'i' &&
            sym_name[2] == 'l') {
            ann.val_literal.tag = NIL;
            ann.val_literal.int_value = 0;
            ann.val_literal.sym_value = (SymbolId)0;
            ann.has_val_literal = true;
            self.lexer.advance();
            return true;
        }
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
            self.set_error("Literal annotation expects quoted symbol after '");
            return false;
        }
        ann.val_literal.tag = SYMBOL;
        ann.val_literal.int_value = 0;
        ann.val_literal.sym_value = self.get_current_symbol();
        ann.has_val_literal = true;
        self.lexer.advance();
        return true;
    }

    self.set_error("Literal annotation expects int/string/symbol/bool/nil literal");
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
