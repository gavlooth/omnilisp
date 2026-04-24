module lisp;

import main;
import std::collections::list;

fn bool vte_ffi_annotation_symbol(Value* ann, Interp* interp, usz idx, SymbolId* out) {
    if (out != null) *out = (SymbolId)0;
    if (ann == null) return false;
    Value* item = null;
    if (ann.tag == ARRAY) {
        if (ann.array_val == null || idx >= ann.array_val.length) return false;
        item = ann.array_val.items[idx];
    } else if (ann.tag == CONS && is_list(ann)) {
        Value* cursor = ann;
        if (macro_value_name_text(cursor.cons_val.car, interp, "Array")) {
            cursor = cursor.cons_val.cdr;
        }
        for (usz i = 0; i < idx; i++) {
            if (!is_cons(cursor)) return false;
            cursor = cursor.cons_val.cdr;
        }
        if (!is_cons(cursor)) return false;
        item = cursor.cons_val.car;
    } else {
        return false;
    }
    if (item == null) return false;
    if (item.tag == SYMBOL) {
        if (out != null) *out = item.sym_val;
        return true;
    }
    if (item.tag == PRIMITIVE && item.prim_val != null) {
        usz name_len = 0;
        while (name_len < 64 && item.prim_val.name[name_len] != 0) name_len++;
        if (name_len == 0 || name_len >= 64) return false;
        SymbolId sym = interp.symbols.intern(item.prim_val.name[0:name_len]);
        if (sym == INVALID_SYMBOL_ID) return false;
        if (out != null) *out = sym;
        return true;
    }
    return false;
}

fn bool vte_is_ffi_annotation(Value* ann, Interp* interp) {
    SymbolId first = (SymbolId)0;
    return vte_ffi_annotation_symbol(ann, interp, 0, &first) && (uint)first == (uint)interp.sym_ffi;
}

fn bool vte_type_annotation_from_value(Value* value, Interp* interp, TypeAnnotation* out) {
    if (out == null) return false;
    *out = parser_empty_type_annotation();
    if (value == null || value.tag != SYMBOL) return false;
    char[] name = interp.symbols.get_name(value.sym_val);
    if (name.len <= 1 || name[0] != '^') return false;
    SymbolId base = interp.symbols.intern(name[1..name.len - 1]);
    if (base == INVALID_SYMBOL_ID) return false;
    char[] base_name = interp.symbols.get_name(base);
    if (base_name.len == 4 && base_name[0] == 'D' && base_name[1] == 'i' && base_name[2] == 'c' && base_name[3] == 't') {
        base = interp.sym_Dictionary;
    }
    out.has_annotation = true;
    out.base_type = base;
    return true;
}

fn bool vte_copy_ffi_c_name(ExprFfiFn* ffi, Interp* interp) {
    if (ffi == null) return false;
    char[] sym_text = interp.symbols.get_name(ffi.fn_name);
    char* c_name = (char*)macro_ast_array_alloc(interp, char.sizeof, sym_text.len + 1);
    if (c_name == null) return false;
    for (usz i = 0; i < sym_text.len; i++) {
        c_name[i] = sym_text[i] == '-' ? '_' : sym_text[i];
    }
    c_name[sym_text.len] = 0;
    ffi.c_name = c_name;
    ffi.c_name_len = sym_text.len;
    return true;
}

fn Expr* vte_ffi_lib_expr(Value* rest, Interp* interp) {
    if (!vte_rest_has_exact_arity(rest, 2)) return make_error_expr(interp, "value->expr: [ffi lib] expects name and path");
    Value* name_v = rest.cons_val.car;
    if (name_v == null || name_v.tag != SYMBOL) return make_error_expr(interp, "value->expr: [ffi lib] name must be a symbol");
    Expr* e = interp.alloc_expr();
    if (e == null) return make_error_expr(interp, "value->expr: failed to allocate ffi lib expression");
    e.tag = E_FFI_LIB;
    e.ffi_lib.name = name_v.sym_val;
    rest = rest.cons_val.cdr;
    e.ffi_lib.path_expr = rest_car_expr(rest, interp);
    return e;
}

fn Expr* vte_ffi_fn_expr(SymbolId lib_name, Value* signature_v, Value* return_v, Interp* interp, bool is_variadic = false) {
    if (signature_v == null || signature_v.tag != CONS || !is_list(signature_v)) {
        return make_error_expr(interp, "value->expr: [ffi lambda] signature must be a list");
    }
    Value* fn_name_v = signature_v.cons_val.car;
    if (fn_name_v == null || fn_name_v.tag != SYMBOL) {
        return make_error_expr(interp, "value->expr: [ffi lambda] function name must be a symbol");
    }

    ExprFfiFn* ffi = (ExprFfiFn*)ast_arena_alloc(&interp.ast_arena, ExprFfiFn.sizeof);
    if (ffi == null) return make_error_expr(interp, "value->expr: failed to allocate ffi function expression");
    *ffi = {};
    ffi.lib_name = lib_name;
    ffi.fn_name = fn_name_v.sym_val;
    ffi.is_variadic = is_variadic;
    if (!vte_copy_ffi_c_name(ffi, interp)) return make_error_expr(interp, "value->expr: failed to allocate ffi C symbol name");

    usz param_count = list_length(signature_v.cons_val.cdr);
    ffi.param_count = param_count;
    ffi.param_capacity = param_count;
    if (param_count > 0) {
        ffi.param_names = (SymbolId*)macro_ast_array_alloc(interp, SymbolId.sizeof, param_count);
        ffi.param_types = (TypeAnnotation*)macro_ast_array_alloc(interp, TypeAnnotation.sizeof, param_count);
        if (ffi.param_names == null || ffi.param_types == null) return make_error_expr(interp, "value->expr: failed to allocate ffi parameters");
    }

    Value* cursor = signature_v.cons_val.cdr;
    usz idx = 0;
    while (is_cons(cursor)) {
        Value* param_v = cursor.cons_val.car;
        if (param_v == null || param_v.tag != CONS || !is_list(param_v) || list_length(param_v) != 2) {
            return make_error_expr(interp, "value->expr: [ffi lambda] parameters must be (^Type name)");
        }
        if (!vte_type_annotation_from_value(param_v.cons_val.car, interp, &ffi.param_types[idx])) {
            return make_error_expr(interp, "value->expr: [ffi lambda] parameter requires ^Type annotation");
        }
        Value* param_name_v = param_v.cons_val.cdr.cons_val.car;
        if (param_name_v == null || param_name_v.tag != SYMBOL) {
            return make_error_expr(interp, "value->expr: [ffi lambda] parameter name must be a symbol");
        }
        ffi.param_names[idx] = param_name_v.sym_val;
        idx++;
        cursor = cursor.cons_val.cdr;
    }

    if (!vte_type_annotation_from_value(return_v, interp, &ffi.return_type)) {
        return make_error_expr(interp, "value->expr: [ffi lambda] requires ^Return annotation");
    }
    ffi.has_return_type = true;

    Expr* e = interp.alloc_expr();
    if (e == null) return make_error_expr(interp, "value->expr: failed to allocate ffi function expression");
    e.tag = E_FFI_FN;
    e.ffi_fn = ffi;
    return e;
}

fn Expr* vte_ffi_module_expr(Value* rest, Interp* interp) {
    if (!is_cons(rest) || !is_cons(rest.cons_val.cdr)) return make_error_expr(interp, "value->expr: [ffi module] expects library, path, and functions");
    Value* lib_v = rest.cons_val.car;
    if (lib_v == null || lib_v.tag != SYMBOL) return make_error_expr(interp, "value->expr: [ffi module] library must be a symbol");

    Value* lib_rest = make_cons(interp, lib_v, make_cons(interp, rest.cons_val.cdr.cons_val.car, make_nil(interp)));
    Expr* lib_expr = vte_ffi_lib_expr(lib_rest, interp);
    if (lib_expr == null || lib_expr.tag == E_LIT && lib_expr.lit.value != null && lib_expr.lit.value.tag == ERROR) return lib_expr;

    List{Expr*} items;
    items.push(lib_expr);
    Value* cursor = rest.cons_val.cdr.cons_val.cdr;
    while (is_cons(cursor)) {
        Value* sig_v = cursor.cons_val.car;
        cursor = cursor.cons_val.cdr;
        if (!is_cons(cursor)) {
            items.free();
            return make_error_expr(interp, "value->expr: [ffi module] function missing return annotation");
        }
        Expr* fn_expr = vte_ffi_fn_expr(lib_v.sym_val, sig_v, cursor.cons_val.car, interp);
        if (fn_expr == null || fn_expr.tag == E_LIT && fn_expr.lit.value != null && fn_expr.lit.value.tag == ERROR) {
            items.free();
            return fn_expr;
        }
        items.push(fn_expr);
        cursor = cursor.cons_val.cdr;
    }

    Expr* e = interp.alloc_expr();
    if (e == null) {
        items.free();
        return make_error_expr(interp, "value->expr: failed to allocate ffi module expression");
    }
    e.tag = E_BLOCK;
    e.block = (ExprBlock*)ast_arena_alloc(&interp.ast_arena, ExprBlock.sizeof);
    if (e.block == null) {
        items.free();
        return make_error_expr(interp, "value->expr: failed to allocate ffi module expression");
    }
    e.block.expr_count = items.len();
    e.block.exprs = macro_expr_list_to_array(interp, &items);
    bool ok = items.len() == 0 || e.block.exprs != null;
    items.free();
    return ok ? e : make_error_expr(interp, "value->expr: failed to allocate ffi module items");
}

fn Expr* vte_ffi_define_form(Value* ann, Value* rest, Interp* interp) {
    if (!vte_is_ffi_annotation(ann, interp)) return make_error_expr(interp, "value->expr: unsupported define annotation");
    SymbolId kind = (SymbolId)0;
    if (!vte_ffi_annotation_symbol(ann, interp, 1, &kind)) return make_error_expr(interp, "value->expr: [ffi] annotation requires a kind");
    if ((uint)kind == (uint)interp.sym_lib) return vte_ffi_lib_expr(rest, interp);
    if ((uint)kind == (uint)interp.sym_module) return vte_ffi_module_expr(rest, interp);
    if (symbol_is_lambda_spelling(&interp.symbols, kind, interp.sym_lambda)) {
        SymbolId lib_name = (SymbolId)0;
        if (!vte_ffi_annotation_symbol(ann, interp, 2, &lib_name)) return make_error_expr(interp, "value->expr: [ffi lambda] requires library name");
        if (!vte_rest_has_exact_arity(rest, 2)) return make_error_expr(interp, "value->expr: [ffi lambda] expects signature and return annotation");
        return vte_ffi_fn_expr(lib_name, rest.cons_val.car, rest.cons_val.cdr.cons_val.car, interp);
    }
    return make_error_expr(interp, "value->expr: unknown [ffi] annotation kind");
}

fn Expr* vte_ffi_define_flat_form(Value* rest, Interp* interp) {
    if (!is_cons(rest) || !is_cons(rest.cons_val.cdr)) return make_error_expr(interp, "value->expr: [ffi] annotation requires a kind");
    Value* kind_v = rest.cons_val.cdr.cons_val.car;
    if (kind_v == null || kind_v.tag != SYMBOL) return make_error_expr(interp, "value->expr: [ffi] annotation kind must be a symbol");
    Value* body = rest.cons_val.cdr.cons_val.cdr;
    if ((uint)kind_v.sym_val == (uint)interp.sym_lib) return vte_ffi_lib_expr(body, interp);
    if ((uint)kind_v.sym_val == (uint)interp.sym_module) return vte_ffi_module_expr(body, interp);
    if (symbol_is_lambda_spelling(&interp.symbols, kind_v.sym_val, interp.sym_lambda)) {
        if (!is_cons(body)) return make_error_expr(interp, "value->expr: [ffi lambda] requires library name");
        Value* lib_v = body.cons_val.car;
        if (lib_v == null || lib_v.tag != SYMBOL) return make_error_expr(interp, "value->expr: [ffi lambda] library must be a symbol");
        body = body.cons_val.cdr;
        if (!vte_rest_has_exact_arity(body, 2)) return make_error_expr(interp, "value->expr: [ffi lambda] expects signature and return annotation");
        return vte_ffi_fn_expr(lib_v.sym_val, body.cons_val.car, body.cons_val.cdr.cons_val.car, interp);
    }
    return make_error_expr(interp, "value->expr: unknown [ffi] annotation kind");
}

fn Expr* vte_resolve_form(Value* rest, Interp* interp) {
    if (!vte_rest_has_exact_arity(rest, 1)) return make_error_expr(interp, "value->expr: resolve expects exactly 1 argument");
    Expr* e = interp.alloc_expr();
    e.tag = E_RESOLVE;
    e.resolve.value = rest_car_expr(rest, interp);
    return e;
}

fn Expr* vte_handle_form(Value* rest, Interp* interp) {
    if (!is_cons(rest)) return make_error_expr(interp, "value->expr: handle expects body and at least one clause");

    bool strict = false;
    if (macro_value_is_symbol_text(rest.cons_val.car, interp, "^strict")) {
        strict = true;
        rest = rest.cons_val.cdr;
        if (!is_cons(rest)) return make_error_expr(interp, "value->expr: handle expects body after ^strict");
    }

    Expr* body = value_to_expr(rest.cons_val.car, interp);
    rest = rest.cons_val.cdr;

    List{EffectClause} clauses;
    while (is_cons(rest)) {
        Value* clause_v = rest.cons_val.car;
        if (clause_v == null || clause_v.tag != CONS || !is_list(clause_v) || list_length(clause_v) < 3) {
            clauses.free();
            return make_error_expr(interp, "value->expr: handle clauses must be (tag arg body...)");
        }

        EffectClause clause;
        clause.effect_tag = (SymbolId)0;
        clause.effect_tag_expr = null;
        clause.has_effect_tag_expr = false;
        clause.k_name = interp.sym__k;

        Value* cursor = clause_v;
        Value* tag_v = cursor.cons_val.car;
        if (tag_v != null && tag_v.tag == SYMBOL) {
            clause.effect_tag = tag_v.sym_val;
        } else {
            clause.effect_tag_expr = value_to_expr(tag_v, interp);
            clause.has_effect_tag_expr = true;
        }

        cursor = cursor.cons_val.cdr;
        Value* arg_v = cursor.cons_val.car;
        if (arg_v == null || arg_v.tag != SYMBOL) {
            clauses.free();
            return make_error_expr(interp, "value->expr: handle clause arg must be a symbol");
        }
        clause.arg_name = arg_v.sym_val;

        cursor = cursor.cons_val.cdr;
        clause.handler_body = vte_implicit_body(cursor, interp, "value->expr: handle clause expects body");
        clauses.push(clause);
        rest = rest.cons_val.cdr;
    }

    if (clauses.len() == 0) {
        clauses.free();
        return make_error_expr(interp, "value->expr: handle expects at least one clause");
    }

    Expr* e = interp.alloc_expr();
    e.tag = E_HANDLE;
    e.handle = (ExprHandle*)ast_arena_alloc(&interp.ast_arena, ExprHandle.sizeof);
    if (e.handle == null) {
        clauses.free();
        return make_error_expr(interp, "value->expr: failed to allocate handle expression");
    }
    e.handle.body = body;
    e.handle.strict_mode = strict;
    e.handle.clause_count = clauses.len();
    e.handle.clauses = macro_effect_clause_list_to_array(interp, &clauses);
    if (clauses.len() > 0 && e.handle.clauses == null) {
        clauses.free();
        return make_error_expr(interp, "value->expr: failed to allocate handle clauses");
    }
    clauses.free();
    return e;
}

fn Expr* vte_match_form(Value* rest, Interp* interp) {
    if (!is_cons(rest)) return make_error_expr(interp, "value->expr: match expects scrutinee");
    Expr* scrutinee = value_to_expr(rest.cons_val.car, interp);
    rest = rest.cons_val.cdr;

    List{MatchClause} clauses;
    while (is_cons(rest)) {
        Value* clause_v = rest.cons_val.car;
        if (clause_v == null || clause_v.tag != CONS || !is_list(clause_v) || list_length(clause_v) != 2) {
            clauses.free();
            return make_error_expr(interp, "value->expr: match clauses must be (pattern result)");
        }
        MatchClause clause;
        clause.pattern = vte_pattern_from_value(clause_v.cons_val.car, interp);
        clause.result = value_to_expr(clause_v.cons_val.cdr.cons_val.car, interp);
        clauses.push(clause);
        rest = rest.cons_val.cdr;
    }

    Expr* e = interp.alloc_expr();
    e.tag = E_MATCH;
    e.match = (ExprMatch*)ast_arena_alloc(&interp.ast_arena, ExprMatch.sizeof);
    if (e.match == null) {
        clauses.free();
        return make_error_expr(interp, "value->expr: failed to allocate match expression");
    }
    e.match.scrutinee = scrutinee;
    e.match.clause_count = clauses.len();
    e.match.clauses = macro_match_clause_list_to_array(interp, &clauses);
    if (clauses.len() > 0 && e.match.clauses == null) {
        clauses.free();
        return make_error_expr(interp, "value->expr: failed to allocate match clauses");
    }
    clauses.free();
    return e;
}

fn void vte_init_import_spec(ExprImport* imp) {
    imp.name = (SymbolId)0;
    imp.has_path = false;
    imp.path = null;
    imp.path_len = 0;
    imp.import_count = 0;
    imp.import_capacity = 0;
    imp.imports = null;
    imp.aliases = null;
    imp.import_all = false;
}

fn bool vte_import_target_into(Value* target, ExprImport* imp, Interp* interp) {
    if (target != null && target.tag == SYMBOL) {
        imp.name = target.sym_val;
        return true;
    }
    if (target != null && target.tag == STRING) {
        imp.has_path = true;
        imp.path_len = target.str_len;
        imp.path = (char*)macro_ast_array_alloc(interp, char.sizeof, target.str_len + 1);
        if (imp.path == null) return false;
        for (usz i = 0; i < target.str_len; i++) imp.path[i] = target.str_chars[i];
        imp.path[target.str_len] = 0;
        imp.name = interp.symbols.intern(target.str_chars[:target.str_len]);
        return imp.name != INVALID_SYMBOL_ID;
    }
    return false;
}

fn Expr* vte_fill_import_tail(Value* rest, ExprImport* imp, Interp* interp) {
    if (is_nil(rest)) return null;
    if (!is_cons(rest) || !is_nil(rest.cons_val.cdr)) {
        return make_error_expr(interp, "value->expr: import expects optional 'all or selector list");
    }

    Value* spec = rest.cons_val.car;
    if (macro_value_is_quoted_symbol(spec, interp, interp.sym_all)) {
        imp.import_all = true;
        return null;
    }

    if (spec == null || spec.tag != CONS || !is_list(spec)) {
        return make_error_expr(interp, "value->expr: import expects optional 'all or selector list");
    }

    List{SymbolId} imports;
    List{SymbolId} aliases;
    Value* cursor = spec;
    while (is_cons(cursor)) {
        Value* item = cursor.cons_val.car;
        if (item != null && item.tag == SYMBOL) {
            imports.push(item.sym_val);
            aliases.push((SymbolId)0);
        } else if (item != null && item.tag == CONS && is_list(item) && list_length(item) == 3 &&
                   item.cons_val.car != null && item.cons_val.car.tag == SYMBOL &&
                   macro_value_is_quoted_symbol(item.cons_val.cdr.cons_val.car, interp, interp.sym_as) &&
                   item.cons_val.cdr.cons_val.cdr.cons_val.car != null &&
                   item.cons_val.cdr.cons_val.cdr.cons_val.car.tag == SYMBOL) {
            imports.push(item.cons_val.car.sym_val);
            aliases.push(item.cons_val.cdr.cons_val.cdr.cons_val.car.sym_val);
        } else {
            imports.free();
            aliases.free();
            return make_error_expr(interp, "value->expr: invalid import selector");
        }
        cursor = cursor.cons_val.cdr;
    }

    imp.import_count = imports.len();
    imp.import_capacity = imports.len();
    imp.imports = macro_symbol_list_to_array(interp, &imports);
    imp.aliases = macro_symbol_list_to_array(interp, &aliases);
    bool ok = imports.len() == 0 || (imp.imports != null && imp.aliases != null);
    imports.free();
    aliases.free();
    return ok ? null : make_error_expr(interp, "value->expr: failed to allocate import selectors");
}

fn Expr* vte_import_form(Value* rest, Interp* interp) {
    if (!is_cons(rest)) return make_error_expr(interp, "value->expr: import expects module target");
    Expr* e = interp.alloc_expr();
    e.tag = E_IMPORT;
    vte_init_import_spec(&e.import_expr);
    if (!vte_import_target_into(rest.cons_val.car, &e.import_expr, interp)) {
        return make_error_expr(interp, "value->expr: import target must be a symbol or string path");
    }
    rest = rest.cons_val.cdr;
    if (Expr* err = vte_fill_import_tail(rest, &e.import_expr, interp)) return err;
    return e;
}

fn Expr* vte_with_form(Value* rest, Interp* interp) {
    if (!is_cons(rest) || !is_cons(rest.cons_val.cdr)) return make_error_expr(interp, "value->expr: with expects module target and body");
    Expr* e = interp.alloc_expr();
    e.tag = E_WITH_MODULE;
    e.with_module = (ExprWithModule*)ast_arena_alloc(&interp.ast_arena, ExprWithModule.sizeof);
    if (e.with_module == null) return make_error_expr(interp, "value->expr: failed to allocate with expression");
    vte_init_import_spec(&e.with_module.import_expr);
    if (!vte_import_target_into(rest.cons_val.car, &e.with_module.import_expr, interp)) {
        return make_error_expr(interp, "value->expr: with target must be a symbol or string path");
    }

    List{Expr*} body;
    rest = rest.cons_val.cdr;
    while (is_cons(rest)) {
        body.push(value_to_expr(rest.cons_val.car, interp));
        rest = rest.cons_val.cdr;
    }
    e.with_module.body_count = body.len();
    e.with_module.body_capacity = body.len();
    e.with_module.body = macro_expr_list_to_array(interp, &body);
    if (body.len() > 0 && e.with_module.body == null) {
        body.free();
        return make_error_expr(interp, "value->expr: failed to allocate with body");
    }
    body.free();
    return e;
}

fn Expr* vte_module_form(Value* rest, Interp* interp) {
    if (!is_cons(rest) || rest.cons_val.car == null || rest.cons_val.car.tag != SYMBOL) {
        return make_error_expr(interp, "value->expr: module expects module name");
    }
    SymbolId name = rest.cons_val.car.sym_val;
    rest = rest.cons_val.cdr;
    if (!is_cons(rest)) return make_error_expr(interp, "value->expr: module expects export list");
    Value* export_form = rest.cons_val.car;
    if (export_form == null || export_form.tag != CONS || !is_list(export_form) ||
        !macro_value_is_symbol(export_form.cons_val.car, interp.sym_export)) {
        return make_error_expr(interp, "value->expr: module expects (export ...) list");
    }

    List{SymbolId} exports;
    Value* export_cursor = export_form.cons_val.cdr;
    while (is_cons(export_cursor)) {
        if (export_cursor.cons_val.car == null || export_cursor.cons_val.car.tag != SYMBOL) {
            exports.free();
            return make_error_expr(interp, "value->expr: module export names must be symbols");
        }
        exports.push(export_cursor.cons_val.car.sym_val);
        export_cursor = export_cursor.cons_val.cdr;
    }

    List{Expr*} body;
    rest = rest.cons_val.cdr;
    while (is_cons(rest)) {
        body.push(value_to_expr(rest.cons_val.car, interp));
        rest = rest.cons_val.cdr;
    }

    Expr* e = interp.alloc_expr();
    e.tag = E_MODULE;
    e.module_expr = (ExprModule*)ast_arena_alloc(&interp.ast_arena, ExprModule.sizeof);
    if (e.module_expr == null) {
        exports.free();
        body.free();
        return make_error_expr(interp, "value->expr: failed to allocate module expression");
    }
    e.module_expr.name = name;
    e.module_expr.export_count = exports.len();
    e.module_expr.export_capacity = exports.len();
    e.module_expr.exports = macro_symbol_list_to_array(interp, &exports);
    e.module_expr.body_count = body.len();
    e.module_expr.body_capacity = body.len();
    e.module_expr.body = macro_expr_list_to_array(interp, &body);
    bool ok = (exports.len() == 0 || e.module_expr.exports != null) && (body.len() == 0 || e.module_expr.body != null);
    exports.free();
    body.free();
    if (!ok) return make_error_expr(interp, "value->expr: failed to allocate module expression");
    return e;
}

fn Expr* vte_export_from_form(Value* rest, Interp* interp) {
    if (!is_cons(rest) || rest.cons_val.car == null || rest.cons_val.car.tag != SYMBOL) {
        return make_error_expr(interp, "value->expr: export-from expects source module");
    }
    Expr* e = interp.alloc_expr();
    e.tag = E_EXPORT_FROM;
    e.export_from.source_module = rest.cons_val.car.sym_val;
    e.export_from.name_count = 0;
    e.export_from.name_capacity = 0;
    e.export_from.names = null;
    e.export_from.all = false;

    rest = rest.cons_val.cdr;
    if (!is_cons(rest) || !is_nil(rest.cons_val.cdr)) return make_error_expr(interp, "value->expr: export-from expects 'all or name list");
    Value* spec = rest.cons_val.car;
    if (macro_value_is_quoted_symbol(spec, interp, interp.sym_all)) {
        e.export_from.all = true;
        return e;
    }
    if (spec == null || spec.tag != CONS || !is_list(spec)) return make_error_expr(interp, "value->expr: export-from expects 'all or name list");

    List{SymbolId} names;
    Value* cursor = spec;
    while (is_cons(cursor)) {
        if (cursor.cons_val.car == null || cursor.cons_val.car.tag != SYMBOL) {
            names.free();
            return make_error_expr(interp, "value->expr: export-from names must be symbols");
        }
        names.push(cursor.cons_val.car.sym_val);
        cursor = cursor.cons_val.cdr;
    }
    e.export_from.name_count = names.len();
    e.export_from.name_capacity = names.len();
    e.export_from.names = macro_symbol_list_to_array(interp, &names);
    bool ok = names.len() == 0 || e.export_from.names != null;
    names.free();
    return ok ? e : make_error_expr(interp, "value->expr: failed to allocate export-from names");
}

fn Expr* vte_pipe_form(Value* rest, Interp* interp) {
    if (!is_cons(rest)) return make_error_expr(interp, "value->expr: |> expects an initial value");
    Expr* acc = value_to_expr(rest.cons_val.car, interp);
    rest = rest.cons_val.cdr;
    while (is_cons(rest)) {
        Expr* step = value_to_expr(rest.cons_val.car, interp);
        if (step != null && step.tag == E_CALL) {
            usz new_count = step.call.arg_count + 1;
            Expr** new_args = (Expr**)macro_ast_array_alloc(interp, Expr*.sizeof, new_count);
            if (new_args == null) return make_error_expr(interp, "value->expr: failed to allocate pipe call arguments");
            for (usz i = 0; i < step.call.arg_count; i++) new_args[i] = step.call.args[i];
            new_args[step.call.arg_count] = acc;
            step.call.args = new_args;
            step.call.arg_count = new_count;
            acc = step;
        } else {
            Expr* call = interp.alloc_expr();
            call.tag = E_CALL;
            call.call = (ExprCall*)ast_arena_alloc(&interp.ast_arena, ExprCall.sizeof);
            if (call.call == null) return make_error_expr(interp, "value->expr: failed to allocate pipe call");
            call.call.func = step;
            call.call.arg_count = 1;
            call.call.args = (Expr**)macro_ast_array_alloc(interp, Expr*.sizeof, 1);
            if (call.call.args == null) return make_error_expr(interp, "value->expr: failed to allocate pipe call arguments");
            call.call.args[0] = acc;
            acc = call;
        }
        rest = rest.cons_val.cdr;
    }
    return acc;
}

fn Expr* vte_explain_form(Value* rest, Interp* interp) {
    if (!vte_rest_has_exact_arity(rest, 2)) return make_error_expr(interp, "value->expr: explain expects selector and target");
    Expr* selector = value_to_expr(rest.cons_val.car, interp);
    rest = rest.cons_val.cdr;
    Expr* target = value_to_expr(rest.cons_val.car, interp);
    Expr* thunk = make_nullary_lambda_expr(interp, target);
    if (thunk == null) return make_error_expr(interp, "value->expr: failed to allocate explain thunk");
    Expr* func = vte_var_expr(interp, interp.symbols.intern("explain"));
    if (func == null) return make_error_expr(interp, "value->expr: failed to allocate explain call");
    Expr* e = interp.alloc_expr();
    e.tag = E_CALL;
    e.call = (ExprCall*)ast_arena_alloc(&interp.ast_arena, ExprCall.sizeof);
    if (e.call == null) return make_error_expr(interp, "value->expr: failed to allocate explain call");
    e.call.func = func;
    e.call.arg_count = 2;
    e.call.args = (Expr**)macro_ast_array_alloc(interp, Expr*.sizeof, 2);
    if (e.call.args == null) return make_error_expr(interp, "value->expr: failed to allocate explain call args");
    e.call.args[0] = selector;
    e.call.args[1] = thunk;
    return e;
}

fn Expr* vte_with_continuation_form(Value* rest, Interp* interp) {
    if (!is_cons(rest) || rest.cons_val.car == null || rest.cons_val.car.tag != SYMBOL) {
        return make_error_expr(interp, "value->expr: with-continuation expects continuation name and body");
    }
    SymbolId k_name = rest.cons_val.car.sym_val;
    rest = rest.cons_val.cdr;
    Expr* body = vte_implicit_body(rest, interp, "value->expr: with-continuation expects body");
    Expr* e = interp.alloc_expr();
    e.tag = E_LET;
    e.let_expr.name = k_name;
    e.let_expr.init = vte_var_expr(interp, interp.sym__k);
    e.let_expr.body = body;
    e.let_expr.is_recursive = false;
    return e;
}

fn Expr* vte_special_form(SymbolId sym, Value* rest, Interp* interp) {
    if ((uint)sym == (uint)interp.sym_if)               return vte_if_form(rest, interp);
    if ((uint)sym == (uint)interp.sym_quote) {
        if (!vte_rest_has_exact_arity(rest, 1)) return make_error_expr(interp, "value->expr: quote expects exactly 1 argument");
        Expr* e = interp.alloc_expr();
        e.tag = E_QUOTE;
        e.quote.datum = rest.cons_val.car;
        return e;
    }
    if ((uint)sym == (uint)interp.sym_block)            return vte_begin(rest, interp);
    if (symbol_is_lambda_spelling(&interp.symbols, sym, interp.sym_lambda)) return vte_lambda(rest, interp);
    if ((uint)sym == (uint)interp.sym_let)              return vte_let(rest, interp);
    if ((uint)sym == (uint)interp.sym_define)           return vte_define_or_set_form(rest, interp, false);
    if ((uint)sym == (uint)interp.sym_set)              return vte_define_or_set_form(rest, interp, true);
    if ((uint)sym == (uint)interp.sym_and)              return vte_and_or_form(rest, interp, true);
    if ((uint)sym == (uint)interp.sym_or)               return vte_and_or_form(rest, interp, false);
    if ((uint)sym == (uint)interp.sym_handle)           return vte_handle_form(rest, interp);
    if ((uint)sym == (uint)interp.sym_resolve)          return vte_resolve_form(rest, interp);
    if ((uint)sym == (uint)interp.sym_with)             return vte_with_form(rest, interp);
    if ((uint)sym == (uint)interp.sym_with_continuation)return vte_with_continuation_form(rest, interp);
    if ((uint)sym == (uint)interp.sym_match)            return vte_match_form(rest, interp);
    if ((uint)sym == (uint)interp.sym_module)           return vte_module_form(rest, interp);
    if ((uint)sym == (uint)interp.sym_import)           return vte_import_form(rest, interp);
    if ((uint)sym == (uint)interp.sym_export_from)      return vte_export_from_form(rest, interp);
    if ((uint)sym == (uint)interp.sym_pipe)             return vte_pipe_form(rest, interp);
    SymbolId explain_sym = interp.symbols.intern("explain");
    if (explain_sym != INVALID_SYMBOL_ID && (uint)sym == (uint)explain_sym) return vte_explain_form(rest, interp);
    if ((uint)sym == (uint)interp.sym_checkpoint) {
        if (!vte_rest_has_exact_arity(rest, 1)) return make_error_expr(interp, "value->expr: checkpoint expects exactly 1 argument");
        Expr* e = interp.alloc_expr();
        e.tag = E_RESET;
        e.reset.body = rest_car_expr(rest, interp);
        return e;
    }
    if ((uint)sym == (uint)interp.sym_capture) {
        if (!vte_rest_has_exact_arity(rest, 2)) return make_error_expr(interp, "value->expr: capture expects continuation name and body");
        Value* k_name = rest.cons_val.car;
        rest = rest.cons_val.cdr;
        if (k_name == null || k_name.tag != SYMBOL) return make_error_expr(interp, "value->expr: capture continuation name must be a symbol");
        Expr* e = interp.alloc_expr();
        e.tag = E_SHIFT;
        e.shift.k_name = k_name.sym_val;
        e.shift.body = rest_car_expr(rest, interp);
        return e;
    }
    if ((uint)sym == (uint)interp.sym_quasiquote) {
        if (!vte_rest_has_exact_arity(rest, 1)) return make_error_expr(interp, "value->expr: quasiquote expects exactly 1 argument");
        Expr* e = interp.alloc_expr();
        e.tag = E_QUASIQUOTE;
        e.quasiquote.body = rest_car_expr(rest, interp);
        return e;
    }
    if ((uint)sym == (uint)interp.sym_unquote) {
        if (!vte_rest_has_exact_arity(rest, 1)) return make_error_expr(interp, "value->expr: unquote expects exactly 1 argument");
        Expr* e = interp.alloc_expr();
        e.tag = E_UNQUOTE;
        e.unquote.body = rest_car_expr(rest, interp);
        return e;
    }
    if ((uint)sym == (uint)interp.sym_unquote_splicing) {
        if (!vte_rest_has_exact_arity(rest, 1)) return make_error_expr(interp, "value->expr: unquote-splicing expects exactly 1 argument");
        Expr* e = interp.alloc_expr();
        e.tag = E_UNQUOTE_SPLICING;
        e.unquote_splicing.body = rest_car_expr(rest, interp);
        return e;
    }
    if ((uint)sym == (uint)interp.sym_signal) {
        if (!vte_rest_has_exact_arity(rest, 2)) return make_error_expr(interp, "value->expr: signal expects tag and payload");
        if (rest.cons_val.car == null || rest.cons_val.car.tag != SYMBOL) return make_error_expr(interp, "value->expr: signal tag must be a symbol");
        Expr* e = interp.alloc_expr();
        e.tag = E_PERFORM;
        e.perform.tag = rest.cons_val.car.sym_val;
        e.perform.tag_expr = null;
        e.perform.has_tag_expr = false;
        rest = rest.cons_val.cdr;
        e.perform.arg = rest_car_expr(rest, interp);
        return e;
    }
    return null;
}
