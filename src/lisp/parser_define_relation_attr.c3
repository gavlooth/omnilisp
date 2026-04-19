module lisp;

import std::collections::list;

fn bool Parser.parse_relation_column_spec(
    Parser* self,
    List{SymbolId}* col_names,
    SymbolId sym_key,
    SymbolId sym_unique,
    SymbolId sym_index,
    SymbolId sym_ref,
    SymbolId sym_check,
    SymbolId* key_col,
    List{SymbolId}* unique_cols,
    List{DeduceReferenceConstraint}* reference_constraints,
    List{DeduceCheckConstraint}* check_constraints,
    SymbolId* index_col
) {
    self.lexer.advance();  // consume (

    bool has_key_role = false;
    bool has_unique_role = false;
    bool has_index_role = false;
    bool has_ref_role = false;
    bool has_check_role = false;
    SymbolId ref_target_relation = (SymbolId)0;
    SymbolId ref_target_column = (SymbolId)0;
    SymbolId check_predicate = (SymbolId)0;
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
        if ((uint)current == (uint)sym_check) {
            self.lexer.advance();
            if (self.lexer.current.type != T_SYMBOL) {
                self.set_error("expected check predicate name after check");
                return false;
            }
            check_predicate = self.get_current_symbol();
            has_check_role = true;
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
    if (has_check_role) {
        check_constraints.push({
            .local_column = col_name,
            .predicate = check_predicate
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
    bool materialized_view = false;
    SymbolId materialized_refresh_policy = (SymbolId)0;
    SymbolId sym_materialized = self.intern_or_error("materialized", "relation definition: failed to intern materialized attribute");
    SymbolId sym_manual = self.intern_or_error("manual", "relation definition: failed to intern materialized refresh policy");
    SymbolId sym_on_read = self.intern_or_error("on-read", "relation definition: failed to intern materialized refresh policy");
    if (self.has_error) return null;
    for (usz i = 2; i < attrs.len(); i++) {
        SymbolId attr = (*attrs)[i];
        if ((uint)attr == (uint)sym_materialized) {
            materialized_view = true;
            materialized_refresh_policy = sym_manual;
            if (i + 1 < attrs.len()) {
                SymbolId next_attr = (*attrs)[i + 1];
                if ((uint)next_attr == (uint)sym_manual) {
                    materialized_refresh_policy = sym_manual;
                    i++;
                } else if ((uint)next_attr == (uint)sym_on_read) {
                    materialized_refresh_policy = sym_on_read;
                    i++;
                } else {
                    self.set_error("unknown materialized refresh policy");
                    return null;
                }
            }
            continue;
        }
        self.set_error("unknown relation attribute");
        return null;
    }

    SymbolId key_col = (SymbolId)0;
    SymbolId index_col = (SymbolId)0;
    SymbolId sym_key = self.intern_or_error("key", "relation definition: failed to intern column role");
    SymbolId sym_unique = self.intern_or_error("unique", "relation definition: failed to intern column role");
    SymbolId sym_index = self.intern_or_error("index", "relation definition: failed to intern column role");
    SymbolId sym_ref = self.intern_or_error("ref", "relation definition: failed to intern column role");
    SymbolId sym_check = self.intern_or_error("check", "relation definition: failed to intern column role");
    if (self.has_error) return null;

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
    List{DeduceCheckConstraint} check_constraints;
    defer check_constraints.free();
    while (self.lexer.current.type == T_LPAREN && !self.has_error) {
        if (!self.parse_relation_column_spec(
            &col_names,
            sym_key,
            sym_unique,
            sym_index,
            sym_ref,
            sym_check,
            &key_col,
            &unique_cols,
            &reference_constraints,
            &check_constraints,
            &index_col
        )) {
            return null;
        }
    }

    self.expect(T_RPAREN, ")");  // close define

    Expr* call = self.build_define_relation_call(
        db_name,
        rel_name,
        materialized_view,
        materialized_refresh_policy,
        &col_names,
        key_col,
        &unique_cols,
        &reference_constraints,
        &check_constraints,
        index_col
    );

    e.tag = E_DEFINE;
    e.define.name = rel_name;
    e.define.value = call;
    return e;
}
