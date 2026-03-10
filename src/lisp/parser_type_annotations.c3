module lisp;

import std::collections::list;

fn void Parser.parse_compound_type_annotation(Parser* self, TypeAnnotation* ann) {
    ann.has_annotation = true;
    ann.is_compound = true;
    self.lexer.advance();  // consume (

    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected type name after ^(");
        return;
    }
    ann.base_type = self.get_current_symbol();
    self.lexer.advance();

    if (parser_is_value_literal_ctor(self, ann.base_type)) {
        if (!self.parse_value_literal_annotation(ann)) return;
    } else {
        List{SymbolId} params;
        defer params.free();
        while (self.lexer.current.type == T_SYMBOL) {
            params.push(self.get_current_symbol());
            self.lexer.advance();
        }
        if (!self.type_annotation_copy_params(ann, &params)) return;
    }
    self.expect(T_RPAREN, ")");
}

fn void Parser.parse_dict_type_annotation(Parser* self, TypeAnnotation* ann) {
    ann.has_annotation = true;
    ann.is_dict = true;
    self.lexer.advance();  // consume {

    List{MetaEntry} meta_entries;
    defer meta_entries.free();
    while (self.lexer.current.type != T_RBRACE && !self.has_error) {
        MetaEntry entry = {};
        if (self.lexer.current.type == T_QUOTE) {
            self.lexer.advance();
            if (self.lexer.current.type != T_SYMBOL) {
                self.set_error("expected symbol after ' in metadata dict");
                return;
            }
            entry.key = self.get_current_symbol();
            self.lexer.advance();
        } else {
            self.set_error("expected 'key in metadata dict");
            return;
        }
        if (self.lexer.current.type == T_INT) {
            entry.is_int = true;
            entry.int_value = self.lexer.current.int_value;
            self.lexer.advance();
        } else if (self.lexer.current.type == T_SYMBOL) {
            entry.sym_value = self.get_current_symbol();
            self.lexer.advance();
        } else {
            self.set_error("expected value in metadata dict");
            return;
        }
        meta_entries.push(entry);
    }

    if (self.lexer.current.type == T_RBRACE) {
        if (!self.type_annotation_copy_meta(ann, &meta_entries)) return;
        self.lexer.advance();  // consume }
        return;
    }
    if (!self.has_error) self.set_error("expected } in metadata dict");
}

/**
 * Parse a ^Type annotation on a parameter.
 * Three forms:
 *   ^Int         — simple type (single token ^Int)
 *   ^(List Int)  — compound type (^ then parens)
 *   ^{'T Number} — metadata dictionary (^ then braces)
 */
fn TypeAnnotation Parser.parse_type_annotation(Parser* self) {
    TypeAnnotation ann = parser_empty_type_annotation();

    if (self.lexer.current.type != T_SYMBOL) return ann;
    if (self.lexer.current.text[0] != '^') return ann;

    if (self.lexer.current.text_len > 1) {
        // Form 1: ^TypeName — strip leading ^, intern rest as base_type
        ann.has_annotation = true;
        ann.base_type = self.interp.symbols.intern(self.lexer.current.text[1..self.lexer.current.text_len - 1]);
        self.lexer.advance();
        return ann;
    }

    // text is exactly "^" (1 char) — peek at next token
    self.lexer.advance();  // consume "^"

    if (self.lexer.current.type == T_LPAREN) {
        self.parse_compound_type_annotation(&ann);
        return ann;
    }

    if (self.lexer.current.type == T_LBRACE) {
        self.parse_dict_type_annotation(&ann);
        return ann;
    }

    self.set_error("expected type name, (, or { after ^");
    return ann;
}
