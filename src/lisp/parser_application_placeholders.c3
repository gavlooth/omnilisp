module lisp;

import std::collections::list;

enum PlaceholderArgKind : char {
    PLACEHOLDER_NONE,
    PLACEHOLDER_PLAIN,
    PLACEHOLDER_INDEXED,
    PLACEHOLDER_INDEXED_INVALID,
}

fn bool Parser.is_placeholder_arg(Parser* self, Expr* arg) @inline {
    return arg != null && arg.tag == E_VAR && (uint)arg.var_expr.name == (uint)self.interp.sym_placeholder;
}

fn PlaceholderArgKind Parser.classify_placeholder_arg(Parser* self, Expr* arg, usz* indexed_pos) {
    if (arg == null || arg.tag != E_VAR) return PLACEHOLDER_NONE;
    if ((uint)arg.var_expr.name == (uint)self.interp.sym_placeholder) return PLACEHOLDER_PLAIN;

    char[] name = self.interp.symbols.get_name(arg.var_expr.name);
    if (name.len < 2 || name[0] != '_') return PLACEHOLDER_NONE;

    char first = name[1];
    if (first == '-') return PLACEHOLDER_INDEXED_INVALID;
    if (first < '0' || first > '9') return PLACEHOLDER_NONE;
    if (first == '0') return PLACEHOLDER_INDEXED_INVALID;

    usz value = (usz)(first - '0');
    for (usz i = 2; i < name.len; i++) {
        char ch = name[i];
        if (ch < '0' || ch > '9') return PLACEHOLDER_INDEXED_INVALID;
        usz digit = (usz)(ch - '0');
        if (value > (usz.max - digit) / 10) return PLACEHOLDER_INDEXED_INVALID;
        value = value * 10 + digit;
    }
    *indexed_pos = value;
    return PLACEHOLDER_INDEXED;
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

fn void Parser.replace_indexed_placeholders(Parser* self, Expr* call, SymbolId* param_syms, usz param_count) {
    for (usz i = 0; i < call.call.arg_count; i++) {
        Expr* arg = call.call.args[i];
        usz indexed_pos = 0;
        if (self.classify_placeholder_arg(arg, &indexed_pos) != PLACEHOLDER_INDEXED) continue;
        if (indexed_pos == 0 || indexed_pos > param_count) continue;
        arg.var_expr.name = param_syms[indexed_pos - 1];
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
