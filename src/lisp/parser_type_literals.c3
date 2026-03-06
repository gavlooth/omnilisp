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
    ann.val_literal = 0;
    ann.has_val_literal = false;
    return ann;
}

fn bool Parser.type_annotation_copy_params(Parser* self, TypeAnnotation* ann, List{SymbolId}* params) {
    if (params.len() == 0) return true;
    ann.params = (SymbolId*)mem::malloc(SymbolId.sizeof * params.len());
    if (ann.params == null) {
        self.set_error("out of memory while parsing type annotation");
        return false;
    }
    ann.param_count = params.len();
    for (usz i = 0; i < params.len(); i++) ann.params[i] = (*params)[i];
    return true;
}

fn bool Parser.type_annotation_copy_meta(Parser* self, TypeAnnotation* ann, List{MetaEntry}* meta) {
    if (meta.len() == 0) return true;
    ann.meta = (MetaEntry*)mem::malloc(MetaEntry.sizeof * meta.len());
    if (ann.meta == null) {
        self.set_error("out of memory while parsing metadata dict");
        return false;
    }
    ann.meta_count = meta.len();
    for (usz i = 0; i < meta.len(); i++) ann.meta[i] = (*meta)[i];
    return true;
}

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

    if (ann.base_type == self.interp.sym_Val && self.lexer.current.type == T_INT) {
        ann.has_val_literal = true;
        ann.val_literal = self.lexer.current.int_value;
        self.lexer.advance();
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

/**
 * Check for and parse postfix index access: expr.[index]
 * Returns the expression wrapped in E_INDEX if followed by .[, otherwise returns unchanged.
 *
 * OmniLisp dot-bracket syntax:
 *   arr.[0]           - array/list access by integer index
 *   dict.['key]       - dictionary access by quoted symbol key
 *   matrix.[i].[j]    - chained indexing
 */
fn Expr* Parser.parse_postfix_index(Parser* self, Expr* base) {
    if (self.has_error) return base;
    // Keep parsing postfix .[index] as long as we see .[
    while (self.lexer.current.type == T_DOT_BRACKET && !self.has_error) {
        self.lexer.advance();  // consume '.['

        // Parse the index expression
        Expr* idx = self.parse_expr();

        self.expect(T_RBRACKET, "]");

        // Wrap in E_INDEX
        Expr* indexed = self.alloc_expr_here();
        indexed.tag = E_INDEX;
        indexed.index.collection = base;
        indexed.index.index = idx;

        base = indexed;
    }
    return base;
}

/**
 * Parse a dict literal after the opening '{' has been consumed.
 * Desugars {k1 v1 k2 v2 ...} to (dict k1 v1 k2 v2 ...).
 */
fn Expr* Parser.parse_dict_literal(Parser* self) {
    Lexer* lex = self.lexer;
    Expr* e = self.alloc_expr_here();

    // Build E_CALL: func = E_VAR("dict"), args = collected expressions
    Expr* func = self.interp.alloc_expr();
    func.tag = E_VAR;
    func.var_expr.name = self.interp.symbols.intern("dict");

    List{Expr*} args;
    while (lex.current.type != T_RBRACE && !self.has_error) {
        Expr* arg = self.parse_expr();
        if (arg == null) { args.free(); return null; }
        args.push(arg);
    }

    e.tag = E_CALL;
    e.call = mem::malloc(ExprCall.sizeof);
    e.call.func = func;
    e.call.arg_count = args.len();
    e.call.args = (Expr**)mem::malloc(Expr*.sizeof * args.len());
    for (usz i = 0; i < args.len(); i++) { e.call.args[i] = args[i]; }
    args.free();

    if (e.call.arg_count % 2 != 0) {
        self.set_error("dict literal requires even number of elements");
        return null;
    }

    self.expect(T_RBRACE, "}");
    return self.parse_postfix_index(e);
}

/**
 * Parse an array literal after the opening '[' has been consumed.
 * Desugars [e1 e2 e3 ...] to (array e1 e2 e3 ...).
 */
fn Expr* Parser.parse_array_literal(Parser* self) {
    Lexer* lex = self.lexer;
    Expr* e = self.alloc_expr_here();

    // Build E_CALL: func = E_VAR("array"), args = collected expressions
    Expr* func = self.interp.alloc_expr();
    func.tag = E_VAR;
    func.var_expr.name = self.interp.symbols.intern("array");

    List{Expr*} args;
    while (lex.current.type != T_RBRACKET && !self.has_error) {
        Expr* arg = self.parse_expr();
        if (arg == null) { args.free(); return null; }
        args.push(arg);
    }

    e.tag = E_CALL;
    e.call = mem::malloc(ExprCall.sizeof);
    e.call.func = func;
    e.call.arg_count = args.len();
    e.call.args = (Expr**)mem::malloc(Expr*.sizeof * args.len());
    for (usz i = 0; i < args.len(); i++) { e.call.args[i] = args[i]; }
    args.free();

    self.expect(T_RBRACKET, "]");
    return self.parse_postfix_index(e);
}
