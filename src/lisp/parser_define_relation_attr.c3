module lisp;

import std::collections::list;

fn bool Parser.parse_relation_column_spec(
    Parser* self,
    List{SymbolId}* col_names,
    SymbolId sym_key,
    SymbolId sym_unique,
    SymbolId sym_index,
    SymbolId sym_ref,
    SymbolId* key_col,
    List{SymbolId}* unique_cols,
    List{DeduceReferenceConstraint}* reference_constraints,
    SymbolId* index_col
) {
    self.lexer.advance();  // consume (

    bool has_key_role = false;
    bool has_unique_role = false;
    bool has_index_role = false;
    bool has_ref_role = false;
    SymbolId ref_target_relation = 0;
    SymbolId ref_target_column = 0;
    while (self.lexer.current.type == T_SYMBOL) {
        SymbolId current = self.get_current_symbol();
        if ((uint)current == (uint)sym_key) {
            has_key_role = true;
            self.lexer.advance();
            continue;
        }
        if ((uint)current == (uint)sym_unique) {
            has_unique_role = true;
            self.lexer.advance();
            continue;
        }
        if ((uint)current == (uint)sym_index) {
            has_index_role = true;
            self.lexer.advance();
            continue;
        }
        if ((uint)current == (uint)sym_ref) {
            self.lexer.advance();
            if (self.lexer.current.type != T_SYMBOL) {
                self.set_error("expected referenced relation name after ref");
                return false;
            }
            ref_target_relation = self.get_current_symbol();
            self.lexer.advance();
            if (self.lexer.current.type != T_SYMBOL) {
                self.set_error("expected referenced column name after ref relation");
                return false;
            }
            ref_target_column = self.get_current_symbol();
            has_ref_role = true;
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
    if (has_unique_role) unique_cols.push(col_name);
    if (has_ref_role) {
        reference_constraints.push({
            .local_column = col_name,
            .target_relation = ref_target_relation,
            .target_column = ref_target_column
        });
    }
    if (has_index_role) *index_col = col_name;

    self.lexer.advance();
    self.expect(T_RPAREN, ")");
    return !self.has_error;
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
    SymbolId sym_unique = self.interp.symbols.intern("unique");
    SymbolId sym_index = self.interp.symbols.intern("index");
    SymbolId sym_ref = self.interp.symbols.intern("ref");

    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected relation name");
        return null;
    }
    SymbolId rel_name = self.get_current_symbol();
    self.lexer.advance();

    List{SymbolId} col_names;
    defer col_names.free();
    List{SymbolId} unique_cols;
    defer unique_cols.free();
    List{DeduceReferenceConstraint} reference_constraints;
    defer reference_constraints.free();
    while (self.lexer.current.type == T_LPAREN && !self.has_error) {
        if (!self.parse_relation_column_spec(
            &col_names,
            sym_key,
            sym_unique,
            sym_index,
            sym_ref,
            &key_col,
            &unique_cols,
            &reference_constraints,
            &index_col
        )) {
            return null;
        }
    }

    self.expect(T_RPAREN, ")");  // close define

    Expr* call = self.build_define_relation_call(
        db_name,
        rel_name,
        &col_names,
        key_col,
        &unique_cols,
        &reference_constraints,
        index_col
    );

    e.tag = E_DEFINE;
    e.define.name = rel_name;
    e.define.value = call;
    return e;
}
