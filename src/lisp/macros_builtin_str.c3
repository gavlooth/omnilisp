module lisp;

import std::collections::list;
import main;

fn bool macro_str_symbol_name_eq(SymbolTable* symbols, SymbolId sym, char[] expected) {
    char[] name = symbols.get_name(sym);
    if (name.len != expected.len) return false;
    for (usz i = 0; i < expected.len; i++) {
        if (name[i] != expected[i]) return false;
    }
    return true;
}

fn bool macro_str_is_call_named(Expr* expr, Interp* interp, char[] name) {
    if (expr == null || expr.tag != E_CALL || expr.call == null) return false;
    if (expr.call.func == null || expr.call.func.tag != E_VAR) return false;
    return macro_str_symbol_name_eq(&interp.symbols, expr.call.func.var_expr.name, name);
}

fn bool macro_str_find_hole_end(char[] text, usz start, usz* end_out) {
    usz depth = 1;
    bool in_string = false;
    bool escaped = false;
    usz i = start;
    while (i < text.len) {
        char c = text[i];
        if (in_string) {
            if (escaped) {
                escaped = false;
            } else if (c == '\\') {
                escaped = true;
            } else if (c == '"') {
                in_string = false;
            }
            i++;
            continue;
        }

        if (c == '"') {
            in_string = true;
        } else if (c == '{') {
            depth++;
        } else if (c == '}') {
            depth--;
            if (depth == 0) {
                *end_out = i;
                return true;
            }
        }
        i++;
    }
    return false;
}

fn Expr* macro_str_make_string_lit(Interp* interp, char[] text) {
    Expr* e = interp.alloc_expr();
    if (e == null) return null;
    e.tag = E_LIT;
    e.lit.value = make_string(interp, text);
    return e;
}

fn Expr* macro_str_make_var(Interp* interp, char[] name) {
    SymbolId sym = interp.symbols.intern(name);
    if (sym == INVALID_SYMBOL_ID) return make_error_expr(interp, "str: failed to intern expansion symbol");
    Expr* e = interp.alloc_expr();
    if (e == null) return null;
    e.tag = E_VAR;
    e.var_expr.name = sym;
    return e;
}

fn Expr* macro_str_make_call(Interp* interp, char[] name, List{Expr*}* args) {
    Expr* func = macro_str_make_var(interp, name);
    if (func == null) return null;
    if (func.tag == E_LIT && func.lit.value != null && func.lit.value.tag == ERROR) return func;

    Expr* e = interp.alloc_expr();
    if (e == null) return null;
    e.tag = E_CALL;
    e.call = (ExprCall*)ast_arena_alloc(&interp.ast_arena, ExprCall.sizeof);
    if (e.call == null) return make_error_expr(interp, "str: failed to allocate expansion call");
    e.call.func = func;
    e.call.arg_count = (*args).len();
    e.call.args = (Expr**)macro_ast_array_alloc(interp, Expr*.sizeof, (*args).len());
    if ((*args).len() > 0 && e.call.args == null) {
        return make_error_expr(interp, "str: failed to allocate expansion call arguments");
    }
    for (usz i = 0; i < (*args).len(); i++) {
        e.call.args[i] = (*args)[i];
    }
    return e;
}

fn Expr* macro_str_make_string_call(Interp* interp, Expr* expr) {
    List{Expr*} args;
    args.push(expr);
    Expr* result = macro_str_make_call(interp, "String", &args);
    args.free();
    return result;
}

fn bool macro_str_push_literal_part(List{Expr*}* parts, Interp* interp, char[] text, usz start, usz end) {
    if (end <= start) return true;
    Expr* lit = macro_str_make_string_lit(interp, text[start:end - start]);
    if (lit == null) return false;
    parts.push(lit);
    return true;
}

fn Expr* macro_expand_str_call(Expr* call_expr, Interp* interp) {
    if (!macro_str_is_call_named(call_expr, interp, "str")) return null;
    if (call_expr.call.arg_count != 1) return make_error_expr(interp, "str: expected 1 string literal argument");
    Expr* arg = call_expr.call.args[0];
    if (arg == null || arg.tag != E_LIT || arg.lit.value == null || arg.lit.value.tag != STRING) {
        return make_error_expr(interp, "str: expected a string literal");
    }

    char[] text = arg.lit.value.str_chars[:arg.lit.value.str_len];
    List{Expr*} parts;
    usz literal_start = 0;
    usz i = 0;
    while (i < text.len) {
        char c = text[i];
        if (c == '{') {
            if (i + 1 < text.len && text[i + 1] == '{') {
                if (!macro_str_push_literal_part(&parts, interp, text, literal_start, i)) {
                    parts.free();
                    return make_error_expr(interp, "str: failed to allocate literal segment");
                }
                Expr* lit_brace = macro_str_make_string_lit(interp, "{");
                if (lit_brace == null) {
                    parts.free();
                    return make_error_expr(interp, "str: failed to allocate literal segment");
                }
                parts.push(lit_brace);
                i += 2;
                literal_start = i;
                continue;
            }

            if (!macro_str_push_literal_part(&parts, interp, text, literal_start, i)) {
                parts.free();
                return make_error_expr(interp, "str: failed to allocate literal segment");
            }
            usz hole_end = 0;
            if (!macro_str_find_hole_end(text, i + 1, &hole_end)) {
                parts.free();
                return make_error_expr(interp, "str: unmatched '{' in interpolation string");
            }
            if (hole_end == i + 1) {
                parts.free();
                return make_error_expr(interp, "str: empty interpolation expression");
            }

            Expr* hole = parse(text[i + 1:hole_end - i - 1], interp);
            if (hole == null) {
                parts.free();
                return make_error_expr(interp, "str: invalid interpolation expression");
            }
            Expr* rendered = macro_str_make_string_call(interp, hole);
            if (rendered == null) {
                parts.free();
                return make_error_expr(interp, "str: failed to allocate interpolation expression");
            }
            parts.push(rendered);
            i = hole_end + 1;
            literal_start = i;
            continue;
        }

        if (c == '}') {
            if (i + 1 < text.len && text[i + 1] == '}') {
                if (!macro_str_push_literal_part(&parts, interp, text, literal_start, i)) {
                    parts.free();
                    return make_error_expr(interp, "str: failed to allocate literal segment");
                }
                Expr* lit_brace = macro_str_make_string_lit(interp, "}");
                if (lit_brace == null) {
                    parts.free();
                    return make_error_expr(interp, "str: failed to allocate literal segment");
                }
                parts.push(lit_brace);
                i += 2;
                literal_start = i;
                continue;
            }
            parts.free();
            return make_error_expr(interp, "str: unmatched '}' in interpolation string");
        }
        i++;
    }

    if (!macro_str_push_literal_part(&parts, interp, text, literal_start, text.len)) {
        parts.free();
        return make_error_expr(interp, "str: failed to allocate literal segment");
    }

    if (parts.len() == 0) {
        parts.free();
        return macro_str_make_string_lit(interp, "");
    }
    if (parts.len() == 1) {
        Expr* only = parts[0];
        parts.free();
        return only;
    }

    Expr* expanded = macro_str_make_call(interp, "string-append", &parts);
    parts.free();
    return expanded;
}

fn Expr* expand_builtin_macro_call_site(Expr* expr, Interp* interp) {
    if (Expr* str_expanded = macro_expand_str_call(expr, interp)) return str_expanded;
    return null;
}
