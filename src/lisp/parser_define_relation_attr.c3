module lisp;

import std::collections::list;

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
