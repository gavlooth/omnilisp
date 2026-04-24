module lisp;

import main;
// =============================================================================
// SECTION 2.6: DEFMACRO (expr/value conversion + lookup)
// =============================================================================
fn Value* make_list1(Interp* interp, Value* a) @inline {
    return make_cons(interp, a, make_nil(interp));
}

fn Value* make_list2(Interp* interp, Value* a, Value* b) @inline {
    return make_cons(interp, a, make_list1(interp, b));
}

fn Value* make_list3(Interp* interp, Value* a, Value* b, Value* c) @inline {
    return make_cons(interp, a, make_list2(interp, b, c));
}

fn Value* make_tagged_form1(Interp* interp, SymbolId tag, Value* a) {
    return make_cons(interp, make_symbol(interp, tag), make_list1(interp, a));
}

fn Value* make_tagged_form2(Interp* interp, SymbolId tag, Value* a, Value* b) {
    return make_cons(interp, make_symbol(interp, tag), make_list2(interp, a, b));
}

fn Value* make_tagged_form3(Interp* interp, SymbolId tag, Value* a, Value* b, Value* c) {
    return make_cons(interp, make_symbol(interp, tag), make_list3(interp, a, b, c));
}

fn Value* expr_call_to_value(Expr* expr, Interp* interp) {
    Value* result = make_nil(interp);
    for (usz i = expr.call.arg_count; i > 0; i--) {
        result = make_cons(interp, expr_to_value(expr.call.args[i - 1], interp), result);
    }
    return make_cons(interp, expr_to_value(expr.call.func, interp), result);
}

fn Value* expr_lambda_to_value(Expr* expr, Interp* interp) {
    Value* params = make_nil(interp);
    for (usz i = expr.lambda.param_count; i > 0; i--) {
        params = make_cons(interp, make_symbol(interp, expr.lambda.params[i - 1]), params);
    }
    Value* body = expr_to_value(expr.lambda.body, interp);
    return make_tagged_form2(interp, interp.sym_lambda, params, body);
}

fn Value* expr_begin_to_value(Expr* expr, Interp* interp) {
    Value* items = make_nil(interp);
    for (usz i = expr.block.expr_count; i > 0; i--) {
        items = make_cons(interp, expr_to_value(expr.block.exprs[i - 1], interp), items);
    }
    return make_cons(interp, make_symbol(interp, interp.sym_block), items);
}

fn Value* expr_import_target_to_value(ExprImport* imp, Interp* interp) {
    if (imp.has_path && imp.path != null) return make_string(interp, imp.path[:imp.path_len]);
    return make_symbol(interp, imp.name);
}

fn Value* expr_import_selector_list_to_value(ExprImport* imp, Interp* interp) {
    Value* selectors = make_nil(interp);
    for (usz i = imp.import_count; i > 0; i--) {
        usz idx = i - 1;
        Value* item = make_symbol(interp, imp.imports[idx]);
        if ((uint)imp.aliases[idx] != 0) {
            item = make_list3(interp, item, macro_make_quoted_symbol(interp, interp.sym_as), make_symbol(interp, imp.aliases[idx]));
        }
        selectors = make_cons(interp, item, selectors);
    }
    return selectors;
}

fn Value* expr_import_to_value(SymbolId head, ExprImport* imp, Interp* interp) {
    Value* rest = make_nil(interp);
    if (imp.import_all) {
        rest = make_cons(interp, macro_make_quoted_symbol(interp, interp.sym_all), rest);
    } else if (imp.import_count > 0) {
        rest = make_cons(interp, expr_import_selector_list_to_value(imp, interp), rest);
    }
    rest = make_cons(interp, expr_import_target_to_value(imp, interp), rest);
    return make_cons(interp, make_symbol(interp, head), rest);
}

fn Value* expr_handle_to_value(Expr* expr, Interp* interp) {
    Value* rest = make_nil(interp);
    for (usz i = expr.handle.clause_count; i > 0; i--) {
        EffectClause* clause = &expr.handle.clauses[i - 1];
        Value* tag = clause.has_effect_tag_expr ? expr_to_value(clause.effect_tag_expr, interp) : make_symbol(interp, clause.effect_tag);
        Value* clause_value = make_list3(interp, tag, make_symbol(interp, clause.arg_name), expr_to_value(clause.handler_body, interp));
        rest = make_cons(interp, clause_value, rest);
    }
    rest = make_cons(interp, expr_to_value(expr.handle.body, interp), rest);
    if (expr.handle.strict_mode) {
        rest = make_cons(interp, make_symbol(interp, interp.symbols.intern("^strict")), rest);
    }
    return make_cons(interp, make_symbol(interp, interp.sym_handle), rest);
}

fn Value* expr_match_to_value(Expr* expr, Interp* interp) {
    Value* rest = make_nil(interp);
    for (usz i = expr.match.clause_count; i > 0; i--) {
        MatchClause* clause = &expr.match.clauses[i - 1];
        Value* clause_value = make_list2(interp, pattern_to_value(clause.pattern, interp), expr_to_value(clause.result, interp));
        rest = make_cons(interp, clause_value, rest);
    }
    rest = make_cons(interp, expr_to_value(expr.match.scrutinee, interp), rest);
    return make_cons(interp, make_symbol(interp, interp.sym_match), rest);
}

fn Value* expr_module_to_value(Expr* expr, Interp* interp) {
    Value* rest = macro_expr_array_to_value(interp, expr.module_expr.body, expr.module_expr.body_count);
    Value* exports = make_cons(interp, make_symbol(interp, interp.sym_export),
        macro_symbol_list_to_value(interp, expr.module_expr.exports, expr.module_expr.export_count));
    rest = make_cons(interp, exports, rest);
    rest = make_cons(interp, make_symbol(interp, expr.module_expr.name), rest);
    return make_cons(interp, make_symbol(interp, interp.sym_module), rest);
}

fn Value* expr_with_module_to_value(Expr* expr, Interp* interp) {
    Value* rest = macro_expr_array_to_value(interp, expr.with_module.body, expr.with_module.body_count);
    rest = make_cons(interp, expr_import_target_to_value(&expr.with_module.import_expr, interp), rest);
    return make_cons(interp, make_symbol(interp, interp.sym_with), rest);
}

fn Value* expr_export_from_to_value(Expr* expr, Interp* interp) {
    Value* spec = expr.export_from.all
        ? macro_make_quoted_symbol(interp, interp.sym_all)
        : macro_symbol_list_to_value(interp, expr.export_from.names, expr.export_from.name_count);
    return make_tagged_form2(interp, interp.sym_export_from, make_symbol(interp, expr.export_from.source_module), spec);
}

fn Value* type_annotation_to_caret_symbol_value(TypeAnnotation* ann, Interp* interp) {
    if (ann == null || !ann.has_annotation) return make_symbol(interp, interp.symbols.intern("^Any"));
    char[] base = interp.symbols.get_name(ann.base_type);
    char[256] buf;
    if (base.len + 1 > buf.len) return make_symbol(interp, ann.base_type);
    buf[0] = '^';
    for (usz i = 0; i < base.len; i++) buf[i + 1] = base[i];
    return make_symbol(interp, interp.symbols.intern(buf[:base.len + 1]));
}

fn Value* expr_ffi_annotation_to_value(Interp* interp, SymbolId kind, SymbolId lib_name = (SymbolId)0) {
    Value* items = make_nil(interp);
    if ((uint)lib_name != 0) items = make_cons(interp, make_symbol(interp, lib_name), items);
    items = make_cons(interp, make_symbol(interp, kind), items);
    items = make_cons(interp, make_symbol(interp, interp.sym_ffi), items);
    return items;
}

fn Value* expr_ffi_lib_to_value(Expr* expr, Interp* interp) {
    Value* ann = expr_ffi_annotation_to_value(interp, interp.sym_lib);
    Value* rest = make_list2(interp, make_symbol(interp, expr.ffi_lib.name), expr_to_value(expr.ffi_lib.path_expr, interp));
    return make_cons(interp, make_symbol(interp, interp.sym_define), make_cons(interp, ann, rest));
}

fn Value* expr_ffi_fn_signature_to_value(ExprFfiFn* ffi, Interp* interp) {
    Value* params = make_nil(interp);
    for (usz i = ffi.param_count; i > 0; i--) {
        usz idx = i - 1;
        Value* param = make_list2(interp, type_annotation_to_caret_symbol_value(&ffi.param_types[idx], interp), make_symbol(interp, ffi.param_names[idx]));
        params = make_cons(interp, param, params);
    }
    return make_cons(interp, make_symbol(interp, ffi.fn_name), params);
}

fn Value* expr_ffi_fn_to_value(Expr* expr, Interp* interp) {
    ExprFfiFn* ffi = expr.ffi_fn;
    Value* ann = expr_ffi_annotation_to_value(interp, interp.sym_lambda, ffi.lib_name);
    Value* ret = type_annotation_to_caret_symbol_value(&ffi.return_type, interp);
    Value* rest = make_list2(interp, expr_ffi_fn_signature_to_value(ffi, interp), ret);
    return make_cons(interp, make_symbol(interp, interp.sym_define), make_cons(interp, ann, rest));
}

fn Value* expr_to_value(Expr* expr, Interp* interp) {
    if (expr == null) return make_nil(interp);

    switch (expr.tag) {
        case E_LIT:
            return expr.lit.value;
        case E_VAR:
            return make_symbol(interp, expr.var_expr.name);
        case E_QUOTE: {
            return make_tagged_form1(interp, interp.sym_quote, expr.quote.datum);
        }
        case E_CALL:
            return expr_call_to_value(expr, interp);
        case E_APP: {
            Value* f = expr_to_value(expr.app.func, interp);
            Value* a = expr_to_value(expr.app.arg, interp);
            return make_list2(interp, f, a);
        }
        case E_IF: {
            Value* t = expr_to_value(expr.if_expr.test, interp);
            Value* th = expr_to_value(expr.if_expr.then_branch, interp);
            Value* el = expr_to_value(expr.if_expr.else_branch, interp);
            return make_tagged_form3(interp, interp.sym_if, t, th, el);
        }
        case E_LET: {
            Value* name_sym = make_symbol(interp, expr.let_expr.name);
            Value* init_val = expr_to_value(expr.let_expr.init, interp);
            // Preserve parser flat-pair let binding form: (let (name init) body).
            // Using nested pair form here loses bindings on value_to_expr round-trip.
            Value* bindings = make_list2(interp, name_sym, init_val);
            Value* body = expr_to_value(expr.let_expr.body, interp);
            return make_tagged_form2(interp, interp.sym_let, bindings, body);
        }
        case E_LAMBDA:
            return expr_lambda_to_value(expr, interp);
        case E_BLOCK:
            return expr_begin_to_value(expr, interp);
        case E_SET: {
            Value* name_sym = make_symbol(interp, expr.set_expr.name);
            Value* val = expr_to_value(expr.set_expr.value, interp);
            return make_tagged_form2(interp, interp.sym_set, name_sym, val);
        }
        case E_RESET: {
            Value* body = expr_to_value(expr.reset.body, interp);
            return make_tagged_form1(interp, interp.sym_checkpoint, body);
        }
        case E_SHIFT: {
            Value* k_name = make_symbol(interp, expr.shift.k_name);
            Value* body = expr_to_value(expr.shift.body, interp);
            return make_tagged_form2(interp, interp.sym_capture, k_name, body);
        }
        case E_PERFORM: {
            Value* tag = expr.perform.has_tag_expr ? expr_to_value(expr.perform.tag_expr, interp) : make_symbol(interp, expr.perform.tag);
            Value* arg = expr_to_value(expr.perform.arg, interp);
            return make_tagged_form2(interp, interp.sym_signal, tag, arg);
        }
        case E_RESOLVE: {
            Value* value = expr_to_value(expr.resolve.value, interp);
            return make_tagged_form1(interp, interp.sym_resolve, value);
        }
        case E_HANDLE:
            return expr_handle_to_value(expr, interp);
        case E_MATCH:
            return expr_match_to_value(expr, interp);
        case E_AND: {
            Value* left = expr_to_value(expr.and_expr.left, interp);
            Value* right = expr_to_value(expr.and_expr.right, interp);
            return make_tagged_form2(interp, interp.sym_and, left, right);
        }
        case E_OR: {
            Value* left = expr_to_value(expr.or_expr.left, interp);
            Value* right = expr_to_value(expr.or_expr.right, interp);
            return make_tagged_form2(interp, interp.sym_or, left, right);
        }
        case E_QUASIQUOTE: {
            Value* body = expr_to_value(expr.quasiquote.body, interp);
            return make_tagged_form1(interp, interp.sym_quasiquote, body);
        }
        case E_UNQUOTE: {
            Value* body = expr_to_value(expr.unquote.body, interp);
            return make_tagged_form1(interp, interp.sym_unquote, body);
        }
        case E_UNQUOTE_SPLICING: {
            Value* body = expr_to_value(expr.unquote_splicing.body, interp);
            return make_tagged_form1(interp, interp.sym_unquote_splicing, body);
        }
        case E_DEFINE: {
            Value* name_sym = make_symbol(interp, expr.define.name);
            Value* val = expr_to_value(expr.define.value, interp);
            return make_tagged_form2(interp, interp.sym_define, name_sym, val);
        }
        case E_MODULE:
            return expr_module_to_value(expr, interp);
        case E_IMPORT:
            return expr_import_to_value(interp.sym_import, &expr.import_expr, interp);
        case E_WITH_MODULE:
            return expr_with_module_to_value(expr, interp);
        case E_EXPORT_FROM:
            return expr_export_from_to_value(expr, interp);
        case E_FFI_LIB:
            return expr_ffi_lib_to_value(expr, interp);
        case E_FFI_FN:
            return expr_ffi_fn_to_value(expr, interp);
        default:
            return make_nil(interp);
    }
}
