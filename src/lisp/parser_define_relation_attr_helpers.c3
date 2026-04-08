module lisp;

import std::collections::list;

fn Expr* parser_relation_symbol_literal(Parser* self, SymbolId sym) {
    Expr* e = self.alloc_expr();
    if (e == null) return null;
    e.tag = E_LIT;
    e.lit.value = self.interp.alloc_value_root();
    e.lit.value.tag = SYMBOL;
    e.lit.value.sym_val = sym;
    return e;
}

fn Expr* parser_relation_var_ref(Parser* self, SymbolId sym) {
    Expr* e = self.alloc_expr();
    if (e == null) return null;
    e.tag = E_VAR;
    e.var_expr.name = sym;
    return e;
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
    SymbolId sym_role_key = self.interp.symbols.intern("__relation-role-key");
    SymbolId sym_role_unique = self.interp.symbols.intern("__relation-role-unique");
    SymbolId sym_role_index = self.interp.symbols.intern("__relation-role-index");
    SymbolId sym_role_ref = self.interp.symbols.intern("__relation-role-ref");
    SymbolId sym_role_check = self.interp.symbols.intern("__relation-role-check");
    SymbolId sym_role_materialized = self.interp.symbols.intern("__relation-role-materialized");
    SymbolId sym_role_materialized_manual = self.interp.symbols.intern("__relation-role-materialized-manual");
    SymbolId sym_role_materialized_on_read = self.interp.symbols.intern("__relation-role-materialized-on-read");
    usz role_arg_count = 0;
    if (materialized_view) role_arg_count++;
    if (materialized_refresh_policy != (SymbolId)0) role_arg_count++;
    if (key_col != (SymbolId)0) role_arg_count++;
    role_arg_count += unique_cols != null ? unique_cols.len() : 0;
    role_arg_count += reference_constraints != null ? reference_constraints.len() * 3 : 0;
    role_arg_count += check_constraints != null ? check_constraints.len() * 2 : 0;
    if (index_col != (SymbolId)0) role_arg_count++;
    usz total_args = 2 + col_count + role_arg_count;
    Expr* func = parser_relation_var_ref(self, self.interp.symbols.intern("__define-relation"));
    if (func == null) return null;

    Expr** call_args = (Expr**)self.alloc_ast_bytes(Expr*.sizeof * total_args, "out of memory while allocating relation definition arguments");
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
                materialized_refresh_policy == self.interp.symbols.intern("manual")
                    ? sym_role_materialized_manual
                    : materialized_refresh_policy == self.interp.symbols.intern("on-read")
                        ? sym_role_materialized_on_read
                        : materialized_refresh_policy;
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
