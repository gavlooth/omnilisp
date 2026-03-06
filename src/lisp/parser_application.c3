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
    e.call = mem::malloc(ExprCall.sizeof);
    e.loc_line = line;
    e.loc_column = col;
    e.call.func = func;
    e.call.arg_count = args.len();
    e.call.args = (Expr**)mem::malloc(Expr*.sizeof * args.len());
    for (usz i = 0; i < args.len(); i++) {
        e.call.args[i] = (*args)[i];
    }
    return e;
}

fn bool Parser.is_placeholder_arg(Parser* self, Expr* arg) @inline {
    return arg != null && arg.tag == E_VAR && (uint)arg.var_expr.name == (uint)self.interp.sym_placeholder;
}

fn SymbolId Parser.make_placeholder_param_sym(Parser* self) {
    self.interp.gensym_counter++;
    char[32] name_buf;
    usz pos = 0;
    name_buf[pos++] = '_';
    name_buf[pos++] = '_';
    name_buf[pos++] = 'p';
    char[20] num_buf;
    char[] num_str = int_to_string((long)self.interp.gensym_counter, &num_buf);
    for (usz j = 0; j < num_str.len && pos < 31; j++) {
        name_buf[pos++] = num_str[j];
    }
    return self.interp.symbols.intern(name_buf[:pos]);
}

fn void Parser.replace_placeholders(Parser* self, Expr* call, List{SymbolId}* param_syms) {
    for (usz i = 0; i < call.call.arg_count; i++) {
        Expr* arg = call.call.args[i];
        if (!self.is_placeholder_arg(arg)) continue;
        SymbolId psym = self.make_placeholder_param_sym();
        param_syms.push(psym);
        arg.var_expr.name = psym;
    }
}

// Multiple _ in one call produce a single multi-arity lambda, not a curried chain.
// (f _ b _) => (lambda (__p1 __p2) (f __p1 b __p2))
// This lets the result be passed directly to map/filter/foldl without extra wrapping.
fn Expr* Parser.wrap_with_placeholder_lambdas(Parser* self, Expr* body, SymbolId* param_syms, usz param_count, usz line, usz col) {
    Expr* lam = self.interp.alloc_expr();
    lam.tag = E_LAMBDA;
    lam.lambda = mem::malloc(ExprLambda.sizeof);
    lam.loc_line = line;
    lam.loc_column = col;
    lam.lambda.param = param_syms[0];
    lam.lambda.param_count = param_count;
    lam.lambda.params = (SymbolId*)mem::malloc(SymbolId.sizeof * param_count);
    for (usz i = 0; i < param_count; i++) {
        lam.lambda.params[i] = param_syms[i];
    }
    lam.lambda.param_annotations = null;
    lam.lambda.has_rest = false;
    lam.lambda.rest_param = 0;
    lam.lambda.has_typed_params = false;
    lam.lambda.body = body;
    return lam;
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
    for (usz i = 0; i < e.call.arg_count; i++) {
        if (self.is_placeholder_arg(e.call.args[i])) {
            placeholder_count++;
        }
    }
    if (placeholder_count > 0) {
        List{SymbolId} param_syms;
        self.replace_placeholders(e, &param_syms);
        Expr* body = self.wrap_with_placeholder_lambdas(e, &param_syms[0], param_syms.len(), app_line, app_col);
        param_syms.free();
        return self.parse_postfix_index(body);
    }

    return self.parse_postfix_index(e);
}
