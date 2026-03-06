module lisp;

import std::collections::list;

/**
 * Handle special bracket defines: [macro], [effect], [relation], [schema]
 */
fn Expr* Parser.parse_define_macro_attr(Parser* self, Expr* e) {
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected macro name");
        return null;
    }
    SymbolId name = self.get_current_symbol();
    self.lexer.advance();

    e.tag = E_DEFMACRO;
    e.define_macro.name = name;
    e.define_macro.clauses = null;
    e.define_macro.clause_count = 0;

    List{MacroClause} clauses;
    defer clauses.free();

    while (self.lexer.current.type == T_LPAREN && !self.has_error) {
        self.lexer.advance();
        Pattern* pat = self.parse_pattern();
        Value* tmpl = self.parse_template_datum();
        self.expect(T_RPAREN, ")");
        clauses.push({ .pattern = pat, .tmpl = tmpl });
    }
    if (clauses.len() > 0) {
        e.define_macro.clauses = (MacroClause*)mem::malloc(MacroClause.sizeof * clauses.len());
        if (e.define_macro.clauses == null) {
            self.set_error("out of memory while parsing macro clauses");
            return null;
        }
        for (usz i = 0; i < clauses.len(); i++) e.define_macro.clauses[i] = clauses[i];
        e.define_macro.clause_count = clauses.len();
    }
    self.expect(T_RPAREN, ")");
    return e;
}

fn void Parser.parse_relation_attr_options(Parser* self, List{SymbolId}* attrs, SymbolId sym_key, SymbolId sym_index, SymbolId* key_col, SymbolId* index_col) {
    for (usz i = 2; i < attrs.len(); i++) {
        if ((uint)(*attrs)[i] == (uint)sym_key && i + 1 < attrs.len()) {
            *key_col = (*attrs)[++i];
        } else if ((uint)(*attrs)[i] == (uint)sym_index && i + 1 < attrs.len()) {
            *index_col = (*attrs)[++i];
        }
    }
}

fn Expr* Parser.build_define_relation_call(Parser* self, SymbolId db_name, SymbolId rel_name, List{SymbolId}* col_names) {
    usz col_count = col_names.len();
    usz total_args = 2 + col_count;
    Expr* func = self.interp.alloc_expr();
    func.tag = E_VAR;
    func.var_expr.name = self.interp.symbols.intern("__define-relation");

    Expr** call_args = (Expr**)mem::malloc(Expr*.sizeof * total_args);

    call_args[0] = self.interp.alloc_expr();
    call_args[0].tag = E_VAR;
    call_args[0].var_expr.name = db_name;

    call_args[1] = self.interp.alloc_expr();
    call_args[1].tag = E_LIT;
    call_args[1].lit.value = self.interp.alloc_value_root();
    call_args[1].lit.value.tag = SYMBOL;
    call_args[1].lit.value.sym_val = rel_name;

    for (usz i = 0; i < col_count; i++) {
        call_args[2 + i] = self.interp.alloc_expr();
        call_args[2 + i].tag = E_LIT;
        call_args[2 + i].lit.value = self.interp.alloc_value_root();
        call_args[2 + i].lit.value.tag = SYMBOL;
        call_args[2 + i].lit.value.sym_val = (*col_names)[i];
    }

    Expr* call = self.interp.alloc_expr();
    call.tag = E_CALL;
    call.call = mem::malloc(ExprCall.sizeof);
    call.call.func = func;
    call.call.args = call_args;
    call.call.arg_count = total_args;
    return call;
}

fn Expr* Parser.parse_define_relation_attr(Parser* self, Expr* e, List{SymbolId}* attrs) {
    if (attrs.len() < 2) {
        self.set_error("expected db after [relation");
        return null;
    }
    SymbolId db_name = (*attrs)[1];

    SymbolId key_col = 0;
    SymbolId index_col = 0;
    SymbolId sym_key = self.interp.symbols.intern("key");
    SymbolId sym_index = self.interp.symbols.intern("index");
    self.parse_relation_attr_options(attrs, sym_key, sym_index, &key_col, &index_col);

    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected relation name");
        return null;
    }
    SymbolId rel_name = self.get_current_symbol();
    self.lexer.advance();

    List{SymbolId} col_names;
    defer col_names.free();
    while (self.lexer.current.type == T_LPAREN && !self.has_error) {
        self.lexer.advance();  // consume (
        if (self.lexer.current.type == T_SYMBOL) {
            SymbolId first = self.get_current_symbol();
            if ((uint)first == (uint)sym_key || (uint)first == (uint)sym_index) {
                if ((uint)first == (uint)sym_key) { key_col = key_col; }
                if ((uint)first == (uint)sym_index) { index_col = index_col; }
                self.lexer.advance();
            }
        }
        if (self.lexer.current.type == T_SYMBOL && self.lexer.current.text[0] == '^') {
            self.lexer.advance();
        }
        if (self.lexer.current.type != T_SYMBOL) {
            self.set_error("expected column name");
            return null;
        }
        col_names.push(self.get_current_symbol());
        self.lexer.advance();
        self.expect(T_RPAREN, ")");
    }

    self.expect(T_RPAREN, ")");  // close define

    Expr* call = self.build_define_relation_call(db_name, rel_name, &col_names);

    e.tag = E_DEFINE;
    e.define.name = rel_name;
    e.define.value = call;
    return e;
}

fn Expr* Parser.parse_define_schema_attr(Parser* self, Expr* e) {
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected schema name");
        return null;
    }
    SymbolId name = self.get_current_symbol();
    self.lexer.advance();
    Value* body = self.parse_datum();
    self.expect(T_RPAREN, ")");

    e.tag = E_DEFINE;
    e.define.name = name;
    Expr* quote_expr = self.interp.alloc_expr();
    quote_expr.tag = E_QUOTE;
    quote_expr.quote.datum = body;
    e.define.value = quote_expr;
    return e;
}

fn Expr* Parser.parse_define_special(Parser* self, Expr* e, SymbolId annotation, List{SymbolId}* attrs) {
    if (annotation == self.interp.sym_macro) {
        return self.parse_define_macro_attr(e);
    }

    if (annotation == self.interp.sym_effect) {
        return self.parse_defeffect(e);
    }

    SymbolId sym_relation = self.interp.symbols.intern("relation");
    if (annotation == sym_relation) {
        return self.parse_define_relation_attr(e, attrs);
    }

    SymbolId sym_schema = self.interp.symbols.intern("schema");
    if (annotation == sym_schema) {
        return self.parse_define_schema_attr(e);
    }

    self.set_error("unknown define attribute");
    return null;
}
