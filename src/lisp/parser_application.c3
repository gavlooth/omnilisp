module lisp;

import std::collections::list;

fn bool Parser.collect_application_args(Parser* self, List{Expr*}* args) {
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        args.push(self.parse_expr());
    }
    return !self.has_error;
}

fn Expr* Parser.build_call_expr(Parser* self, Expr* func, List{Expr*}* args, usz line, usz col) {
    Expr* e = self.interp.alloc_expr();
    e.tag = E_CALL;
    e.call = (ExprCall*)ast_arena_alloc(&self.interp.ast_arena, ExprCall.sizeof);
    e.loc_line = line;
    e.loc_column = col;
    e.call.func = func;
    e.call.arg_count = args.len();
    e.call.args = (Expr**)ast_arena_alloc(&self.interp.ast_arena, Expr*.sizeof * args.len());
    for (usz i = 0; i < args.len(); i++) {
        e.call.args[i] = (*args)[i];
    }
    return e;
}

fn Expr* Parser.parse_application(Parser* self) {
    if (self.has_error) return null;
    usz app_line = self.lexer.current.line;
    usz app_col = self.lexer.current.column;
    Expr* func = self.parse_expr();

    List{Expr*} arg_list;
    if (!self.collect_application_args(&arg_list)) {
        arg_list.free();
        return null;
    }
    self.lexer.advance();
    Expr* e = self.build_call_expr(func, &arg_list, app_line, app_col);
    arg_list.free();

    usz placeholder_count = 0;
    usz indexed_placeholder_max = 0;
    for (usz i = 0; i < e.call.arg_count; i++) {
        Expr* arg = e.call.args[i];
        usz indexed_pos = 0;
        PlaceholderArgKind kind = self.classify_placeholder_arg(arg, &indexed_pos);
        if (kind == PLACEHOLDER_INDEXED_INVALID) {
            self.set_error("invalid indexed placeholder: expected _<positive-int> (for example _1, _2)");
            return null;
        }
        if (kind == PLACEHOLDER_PLAIN) {
            placeholder_count++;
            continue;
        }
        if (kind == PLACEHOLDER_INDEXED && indexed_pos > indexed_placeholder_max) {
            indexed_placeholder_max = indexed_pos;
        }
    }
    if (placeholder_count > 0 && indexed_placeholder_max > 0) {
        self.set_error("cannot mix _ and _n placeholders in the same call");
        return null;
    }

    if (placeholder_count > 0) {
        List{SymbolId} param_syms;
        self.replace_placeholders(e, &param_syms);
        Expr* body = self.wrap_with_placeholder_lambdas(e, &param_syms[0], param_syms.len(), app_line, app_col);
        param_syms.free();
        return self.parse_postfix_index(body);
    }

    if (indexed_placeholder_max > 0) {
        List{SymbolId} param_syms;
        for (usz i = 0; i < indexed_placeholder_max; i++) {
            param_syms.push(self.make_placeholder_param_sym());
        }
        self.replace_indexed_placeholders(e, &param_syms[0], param_syms.len());
        Expr* body = self.wrap_with_placeholder_lambdas(e, &param_syms[0], param_syms.len(), app_line, app_col);
        param_syms.free();
        return self.parse_postfix_index(body);
    }

    return self.parse_postfix_index(e);
}
