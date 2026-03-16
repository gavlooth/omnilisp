module lisp;

import std::collections::list;

fn bool Parser.parse_relation_column_spec(
    Parser* self,
    List{SymbolId}* col_names,
    SymbolId sym_key,
    SymbolId sym_index,
    SymbolId* key_col,
    SymbolId* index_col
) {
    self.lexer.advance();  // consume (

    bool has_key_role = false;
    bool has_index_role = false;
    while (self.lexer.current.type == T_SYMBOL) {
        SymbolId current = self.get_current_symbol();
        if ((uint)current == (uint)sym_key) {
            has_key_role = true;
            self.lexer.advance();
            continue;
        }
        if ((uint)current == (uint)sym_index) {
            has_index_role = true;
            self.lexer.advance();
            continue;
        }
        break;
    }

    if (self.lexer.current.type == T_SYMBOL && self.lexer.current.text[0] == '^') {
        (void)self.parse_type_annotation();
        if (self.has_error) return false;
    }

    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected column name");
        return false;
    }

    SymbolId col_name = self.get_current_symbol();
    col_names.push(col_name);
    if (has_key_role) *key_col = col_name;
    if (has_index_role) *index_col = col_name;

    self.lexer.advance();
    self.expect(T_RPAREN, ")");
    return !self.has_error;
}

fn Expr* Parser.build_define_relation_call(
    Parser* self,
    SymbolId db_name,
    SymbolId rel_name,
    List{SymbolId}* col_names,
    SymbolId key_col,
    SymbolId index_col
) {
    usz col_count = col_names.len();
    SymbolId sym_role_key = self.interp.symbols.intern("__relation-role-key");
    SymbolId sym_role_index = self.interp.symbols.intern("__relation-role-index");
    usz role_arg_count = 0;
    if (key_col != (SymbolId)0) role_arg_count++;
    if (index_col != (SymbolId)0) role_arg_count++;
    usz total_args = 2 + col_count + role_arg_count;
    Expr* func = self.interp.alloc_expr();
    func.tag = E_VAR;
    func.var_expr.name = self.interp.symbols.intern("__define-relation");

    Expr** call_args = (Expr**)ast_arena_alloc(&self.interp.ast_arena, Expr*.sizeof * total_args);

    call_args[0] = self.interp.alloc_expr();
    call_args[0].tag = E_VAR;
    call_args[0].var_expr.name = db_name;

    call_args[1] = self.interp.alloc_expr();
    call_args[1].tag = E_LIT;
    call_args[1].lit.value = self.interp.alloc_value_root();
    call_args[1].lit.value.tag = SYMBOL;
    call_args[1].lit.value.sym_val = rel_name;

    usz arg_index = 2;
    for (usz i = 0; i < col_count; i++) {
        SymbolId col_name = (*col_names)[i];
        if ((uint)col_name == (uint)key_col) {
            call_args[arg_index] = self.interp.alloc_expr();
            call_args[arg_index].tag = E_LIT;
            call_args[arg_index].lit.value = self.interp.alloc_value_root();
            call_args[arg_index].lit.value.tag = SYMBOL;
            call_args[arg_index].lit.value.sym_val = sym_role_key;
            arg_index++;
        }
        if ((uint)col_name == (uint)index_col) {
            call_args[arg_index] = self.interp.alloc_expr();
            call_args[arg_index].tag = E_LIT;
            call_args[arg_index].lit.value = self.interp.alloc_value_root();
            call_args[arg_index].lit.value.tag = SYMBOL;
            call_args[arg_index].lit.value.sym_val = sym_role_index;
            arg_index++;
        }

        call_args[arg_index] = self.interp.alloc_expr();
        call_args[arg_index].tag = E_LIT;
        call_args[arg_index].lit.value = self.interp.alloc_value_root();
        call_args[arg_index].lit.value.tag = SYMBOL;
        call_args[arg_index].lit.value.sym_val = col_name;
        arg_index++;
    }

    Expr* call = self.interp.alloc_expr();
    call.tag = E_CALL;
    call.call = (ExprCall*)ast_arena_alloc(&self.interp.ast_arena, ExprCall.sizeof);
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

    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected relation name");
        return null;
    }
    SymbolId rel_name = self.get_current_symbol();
    self.lexer.advance();

    List{SymbolId} col_names;
    defer col_names.free();
    while (self.lexer.current.type == T_LPAREN && !self.has_error) {
        if (!self.parse_relation_column_spec(&col_names, sym_key, sym_index, &key_col, &index_col)) {
            return null;
        }
    }

    self.expect(T_RPAREN, ")");  // close define

    Expr* call = self.build_define_relation_call(db_name, rel_name, &col_names, key_col, index_col);

    e.tag = E_DEFINE;
    e.define.name = rel_name;
    e.define.value = call;
    return e;
}
