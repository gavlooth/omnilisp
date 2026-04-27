module lisp;

import std::collections::list;

fn Expr* parser_relation_symbol_literal(Parser* self, SymbolId sym) {
    if (sym == INVALID_SYMBOL_ID) {
        self.set_error("relation definition: invalid symbol");
        return null;
    }
    Expr* e = self.alloc_expr();
    if (e == null) return null;
    e.tag = E_LIT;
    e.lit.value = self.alloc_root_value_or_error("out of memory while parsing relation symbol literal");
    if (e.lit.value == null) return null;
    e.lit.value.tag = SYMBOL;
    e.lit.value.sym_val = sym;
    return e;
}

fn Expr* parser_relation_var_ref(Parser* self, SymbolId sym) {
    if (sym == INVALID_SYMBOL_ID) {
        self.set_error("relation definition: invalid variable symbol");
        return null;
    }
    Expr* e = self.alloc_expr();
    if (e == null) return null;
    e.tag = E_VAR;
    e.var_expr.name = sym;
    return e;
}

fn bool parser_relation_add_count(Parser* self, usz* total, usz add, char[] message) {
    if (add > usz.max - *total) {
        self.set_error(message);
        return false;
    }
    *total += add;
    return true;
}

fn bool parser_relation_mul_count(Parser* self, usz count, usz factor, usz* out, char[] message) {
    if (factor != 0 && count > usz.max / factor) {
        self.set_error(message);
        return false;
    }
    *out = count * factor;
    return true;
}

fn Expr* Parser.build_define_relation_call(
    Parser* self,
    SymbolId db_name,
    SymbolId rel_name,
    bool materialized_view,
    SymbolId materialized_refresh_policy,
    List{SymbolId}* col_names,
    SymbolId key_col,
    List{SymbolId}* unique_cols,
    List{DeduceReferenceConstraint}* reference_constraints,
    List{DeduceCheckConstraint}* check_constraints,
    SymbolId index_col
) {
    usz col_count = col_names.len();
    SymbolId sym_role_key = self.intern_or_error("__relation-role-key", "relation definition: failed to intern role symbol");
    SymbolId sym_role_unique = self.intern_or_error("__relation-role-unique", "relation definition: failed to intern role symbol");
    SymbolId sym_role_index = self.intern_or_error("__relation-role-index", "relation definition: failed to intern role symbol");
    SymbolId sym_role_ref = self.intern_or_error("__relation-role-ref", "relation definition: failed to intern role symbol");
    SymbolId sym_role_check = self.intern_or_error("__relation-role-check", "relation definition: failed to intern role symbol");
    SymbolId sym_role_materialized = self.intern_or_error("__relation-role-materialized", "relation definition: failed to intern role symbol");
    SymbolId sym_role_materialized_manual = self.intern_or_error("__relation-role-materialized-manual", "relation definition: failed to intern role symbol");
    SymbolId sym_role_materialized_on_read = self.intern_or_error("__relation-role-materialized-on-read", "relation definition: failed to intern role symbol");
    if (self.has_error) return null;
    usz role_arg_count = 0;
    if (materialized_view && !parser_relation_add_count(self, &role_arg_count, 1, "relation definition argument count overflow")) return null;
    if (materialized_refresh_policy != (SymbolId)0 && !parser_relation_add_count(self, &role_arg_count, 1, "relation definition argument count overflow")) return null;
    if (key_col != (SymbolId)0 && !parser_relation_add_count(self, &role_arg_count, 1, "relation definition argument count overflow")) return null;
    if (unique_cols != null && !parser_relation_add_count(self, &role_arg_count, unique_cols.len(), "relation definition argument count overflow")) return null;
    if (reference_constraints != null) {
        usz ref_arg_count = 0;
        if (!parser_relation_mul_count(self, reference_constraints.len(), (usz)3, &ref_arg_count, "relation definition argument count overflow")) return null;
        if (!parser_relation_add_count(self, &role_arg_count, ref_arg_count, "relation definition argument count overflow")) return null;
    }
    if (check_constraints != null) {
        usz check_arg_count = 0;
        if (!parser_relation_mul_count(self, check_constraints.len(), (usz)2, &check_arg_count, "relation definition argument count overflow")) return null;
        if (!parser_relation_add_count(self, &role_arg_count, check_arg_count, "relation definition argument count overflow")) return null;
    }
    if (index_col != (SymbolId)0 && !parser_relation_add_count(self, &role_arg_count, 1, "relation definition argument count overflow")) return null;
    usz total_args = 2;
    if (!parser_relation_add_count(self, &total_args, col_count, "relation definition argument count overflow")) return null;
    if (!parser_relation_add_count(self, &total_args, role_arg_count, "relation definition argument count overflow")) return null;
    Expr* func = parser_relation_var_ref(self, self.intern_or_error("__define-relation", "relation definition: failed to intern helper name"));
    if (func == null) return null;

    Expr** call_args = (Expr**)self.alloc_ast_array_bytes(Expr*.sizeof, total_args, "out of memory while allocating relation definition arguments");
    if (call_args == null && total_args > 0) return null;

    call_args[0] = parser_relation_var_ref(self, db_name);
    if (call_args[0] == null) return null;

    call_args[1] = parser_relation_symbol_literal(self, rel_name);
    if (call_args[1] == null) return null;

    usz arg_index = 2;
    if (materialized_view) {
        call_args[arg_index] = parser_relation_symbol_literal(self, sym_role_materialized);
        if (call_args[arg_index] == null) return null;
        arg_index++;
        if (materialized_refresh_policy != (SymbolId)0) {
            SymbolId refresh_sym =
                materialized_refresh_policy == self.intern_or_error("manual", "relation definition: failed to intern refresh policy")
                    ? sym_role_materialized_manual
                    : materialized_refresh_policy == self.intern_or_error("on-read", "relation definition: failed to intern refresh policy")
                        ? sym_role_materialized_on_read
                        : materialized_refresh_policy;
            if (self.has_error) return null;
            call_args[arg_index] = parser_relation_symbol_literal(self, refresh_sym);
            if (call_args[arg_index] == null) return null;
            arg_index++;
        }
    }
    for (usz i = 0; i < col_count; i++) {
        SymbolId col_name = (*col_names)[i];
        if ((uint)col_name == (uint)key_col) {
            call_args[arg_index] = parser_relation_symbol_literal(self, sym_role_key);
            if (call_args[arg_index] == null) return null;
            arg_index++;
        }
        if (unique_cols != null) {
            foreach (unique_col : *unique_cols) {
                if ((uint)col_name != (uint)unique_col) continue;
                call_args[arg_index] = parser_relation_symbol_literal(self, sym_role_unique);
                if (call_args[arg_index] == null) return null;
                arg_index++;
                break;
            }
        }
        if (reference_constraints != null) {
            foreach (ref_constraint : *reference_constraints) {
                if ((uint)col_name != (uint)ref_constraint.local_column) continue;
                call_args[arg_index] = parser_relation_symbol_literal(self, sym_role_ref);
                if (call_args[arg_index] == null) return null;
                arg_index++;

                call_args[arg_index] = parser_relation_symbol_literal(self, ref_constraint.target_relation);
                if (call_args[arg_index] == null) return null;
                arg_index++;

                call_args[arg_index] = parser_relation_symbol_literal(self, ref_constraint.target_column);
                if (call_args[arg_index] == null) return null;
                arg_index++;
                break;
            }
        }
        if (check_constraints != null) {
            foreach (check_constraint : *check_constraints) {
                if ((uint)col_name != (uint)check_constraint.local_column) continue;
                call_args[arg_index] = parser_relation_symbol_literal(self, sym_role_check);
                if (call_args[arg_index] == null) return null;
                arg_index++;

                call_args[arg_index] = parser_relation_symbol_literal(self, check_constraint.predicate);
                if (call_args[arg_index] == null) return null;
                arg_index++;
                break;
            }
        }
        if ((uint)col_name == (uint)index_col) {
            call_args[arg_index] = parser_relation_symbol_literal(self, sym_role_index);
            if (call_args[arg_index] == null) return null;
            arg_index++;
        }

        call_args[arg_index] = parser_relation_symbol_literal(self, col_name);
        if (call_args[arg_index] == null) return null;
        arg_index++;
    }

    Expr* call = self.alloc_expr();
    if (call == null) return null;
    call.tag = E_CALL;
    call.call = (ExprCall*)self.alloc_ast_bytes(ExprCall.sizeof, "out of memory while allocating relation definition call");
    if (call.call == null) return null;
    call.call.func = func;
    call.call.args = call_args;
    call.call.arg_count = total_args;
    return call;
}
