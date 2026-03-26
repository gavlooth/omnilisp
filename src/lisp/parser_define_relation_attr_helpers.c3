module lisp;

import std::collections::list;

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
    if (materialized_view) {
        call_args[arg_index] = self.interp.alloc_expr();
        call_args[arg_index].tag = E_LIT;
        call_args[arg_index].lit.value = self.interp.alloc_value_root();
        call_args[arg_index].lit.value.tag = SYMBOL;
        call_args[arg_index].lit.value.sym_val = sym_role_materialized;
        arg_index++;
        if (materialized_refresh_policy != (SymbolId)0) {
            call_args[arg_index] = self.interp.alloc_expr();
            call_args[arg_index].tag = E_LIT;
            call_args[arg_index].lit.value = self.interp.alloc_value_root();
            call_args[arg_index].lit.value.tag = SYMBOL;
            call_args[arg_index].lit.value.sym_val =
                materialized_refresh_policy == self.interp.symbols.intern("manual")
                    ? sym_role_materialized_manual
                    : materialized_refresh_policy == self.interp.symbols.intern("on-read")
                        ? sym_role_materialized_on_read
                        : materialized_refresh_policy;
            arg_index++;
        }
    }
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
        if (unique_cols != null) {
            foreach (unique_col : *unique_cols) {
                if ((uint)col_name != (uint)unique_col) continue;
                call_args[arg_index] = self.interp.alloc_expr();
                call_args[arg_index].tag = E_LIT;
                call_args[arg_index].lit.value = self.interp.alloc_value_root();
                call_args[arg_index].lit.value.tag = SYMBOL;
                call_args[arg_index].lit.value.sym_val = sym_role_unique;
                arg_index++;
                break;
            }
        }
        if (reference_constraints != null) {
            foreach (ref_constraint : *reference_constraints) {
                if ((uint)col_name != (uint)ref_constraint.local_column) continue;
                call_args[arg_index] = self.interp.alloc_expr();
                call_args[arg_index].tag = E_LIT;
                call_args[arg_index].lit.value = self.interp.alloc_value_root();
                call_args[arg_index].lit.value.tag = SYMBOL;
                call_args[arg_index].lit.value.sym_val = sym_role_ref;
                arg_index++;

                call_args[arg_index] = self.interp.alloc_expr();
                call_args[arg_index].tag = E_LIT;
                call_args[arg_index].lit.value = self.interp.alloc_value_root();
                call_args[arg_index].lit.value.tag = SYMBOL;
                call_args[arg_index].lit.value.sym_val = ref_constraint.target_relation;
                arg_index++;

                call_args[arg_index] = self.interp.alloc_expr();
                call_args[arg_index].tag = E_LIT;
                call_args[arg_index].lit.value = self.interp.alloc_value_root();
                call_args[arg_index].lit.value.tag = SYMBOL;
                call_args[arg_index].lit.value.sym_val = ref_constraint.target_column;
                arg_index++;
                break;
            }
        }
        if (check_constraints != null) {
            foreach (check_constraint : *check_constraints) {
                if ((uint)col_name != (uint)check_constraint.local_column) continue;
                call_args[arg_index] = self.interp.alloc_expr();
                call_args[arg_index].tag = E_LIT;
                call_args[arg_index].lit.value = self.interp.alloc_value_root();
                call_args[arg_index].lit.value.tag = SYMBOL;
                call_args[arg_index].lit.value.sym_val = sym_role_check;
                arg_index++;

                call_args[arg_index] = self.interp.alloc_expr();
                call_args[arg_index].tag = E_LIT;
                call_args[arg_index].lit.value = self.interp.alloc_value_root();
                call_args[arg_index].lit.value.tag = SYMBOL;
                call_args[arg_index].lit.value.sym_val = check_constraint.predicate;
                arg_index++;
                break;
            }
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
