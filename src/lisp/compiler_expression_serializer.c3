module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 4c: EXPRESSION SERIALIZER
// =============================================================================

/**
 * Serialize an Expr* back to Pika source text.
 * This is used to convert continuation forms (reset/shift/handle/perform)
 * into source strings that can be evaluated by the interpreter at runtime.
 *
 * The serialized output is appended to the compiler's output buffer temporarily
 * via a separate List{char} buffer.
 */
// --- serialize_expr_to_buf helpers ---

fn void Compiler.serialize_lambda_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(lambda (");
    // Check for zero-arg lambda
    if ((uint)expr.lambda.param == 0xFFFFFFFF) {
        // zero-arg
    } else {
        char[] pname = self.interp.symbols.get_name(expr.lambda.param);
        self.buf_append(buf, pname);
    }
    self.buf_append(buf, ") ");
    self.serialize_expr_to_buf(expr.lambda.body, buf);
    buf.push(')');
}

fn void Compiler.serialize_let_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    if (expr.let_expr.is_recursive) {
        self.buf_append(buf, "(let ^rec (");
    } else {
        self.buf_append(buf, "(let (");
    }
    char[] let_name = self.interp.symbols.get_name(expr.let_expr.name);
    self.buf_append(buf, let_name);
    buf.push(' ');
    self.serialize_expr_to_buf(expr.let_expr.init, buf);
    self.buf_append(buf, ") ");
    self.serialize_expr_to_buf(expr.let_expr.body, buf);
    buf.push(')');
}

fn void Compiler.serialize_call_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    buf.push('(');
    self.serialize_expr_to_buf(expr.call.func, buf);
    for (usz i = 0; i < expr.call.arg_count; i++) {
        buf.push(' ');
        self.serialize_expr_to_buf(expr.call.args[i], buf);
    }
    buf.push(')');
}

fn void Compiler.serialize_handle_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(handle ");
    self.serialize_expr_to_buf(expr.handle.body, buf);
    for (usz i = 0; i < expr.handle.clause_count; i++) {
        self.buf_append(buf, " ((");
        char[] etag = self.interp.symbols.get_name(expr.handle.clauses[i].effect_tag);
        self.buf_append(buf, etag);
        buf.push(' ');
        char[] ek = self.interp.symbols.get_name(expr.handle.clauses[i].k_name);
        self.buf_append(buf, ek);
        buf.push(' ');
        char[] ea = self.interp.symbols.get_name(expr.handle.clauses[i].arg_name);
        self.buf_append(buf, ea);
        self.buf_append(buf, ") ");
        self.serialize_expr_to_buf(expr.handle.clauses[i].handler_body, buf);
        buf.push(')');
    }
    buf.push(')');
}

fn void Compiler.serialize_match_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(match ");
    self.serialize_expr_to_buf(expr.match.scrutinee, buf);
    for (usz i = 0; i < expr.match.clause_count; i++) {
        buf.push(' ');
        self.serialize_pattern_to_buf(expr.match.clauses[i].pattern, buf);
        buf.push(' ');
        self.serialize_expr_to_buf(expr.match.clauses[i].result, buf);
    }
    buf.push(')');
}

fn void Compiler.serialize_defmacro_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(define [macro] ");
    char[] macro_name = self.interp.symbols.get_name(expr.define_macro.name);
    self.buf_append(buf, macro_name);
    for (usz i = 0; i < expr.define_macro.clause_count; i++) {
        self.buf_append(buf, " (");
        self.serialize_pattern_to_buf(expr.define_macro.clauses[i].pattern, buf);
        buf.push(' ');
        self.buf_append(buf, "(");
        self.serialize_value_to_buf(expr.define_macro.clauses[i].tmpl, buf);
        self.buf_append(buf, "))");
    }
    buf.push(')');
}

fn void Compiler.serialize_module_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(module ");
    char[] mod_name = self.interp.symbols.get_name(expr.module_expr.name);
    self.buf_append(buf, mod_name);
    self.buf_append(buf, " (export");
    for (usz i = 0; i < expr.module_expr.export_count; i++) {
        buf.push(' ');
        char[] exp_name = self.interp.symbols.get_name(expr.module_expr.exports[i]);
        self.buf_append(buf, exp_name);
    }
    buf.push(')');
    for (usz i = 0; i < expr.module_expr.body_count; i++) {
        buf.push(' ');
        self.serialize_expr_to_buf(expr.module_expr.body[i], buf);
    }
    buf.push(')');
}

fn void Compiler.serialize_import_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(import ");
    char[] imp_name = self.interp.symbols.get_name(expr.import_expr.name);
    self.buf_append(buf, imp_name);
    if (expr.import_expr.import_all) {
        self.buf_append(buf, " :all");
    } else if (expr.import_expr.import_count > 0) {
        self.buf_append(buf, " (");
        for (usz i = 0; i < expr.import_expr.import_count; i++) {
            if (i > 0) buf.push(' ');
            char[] sel_name = self.interp.symbols.get_name(expr.import_expr.imports[i]);
            self.buf_append(buf, sel_name);
            if ((uint)expr.import_expr.aliases[i] != 0) {
                self.buf_append(buf, " :as ");
                char[] alias_name = self.interp.symbols.get_name(expr.import_expr.aliases[i]);
                self.buf_append(buf, alias_name);
            }
        }
        buf.push(')');
    }
    buf.push(')');
}

fn void Compiler.serialize_export_from_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    self.buf_append(buf, "(export-from ");
    char[] efmod = self.interp.symbols.get_name(expr.export_from.source_module);
    self.buf_append(buf, efmod);
    if (expr.export_from.all) {
        self.buf_append(buf, " :all)");
    } else {
        self.buf_append(buf, " (");
        for (usz i = 0; i < expr.export_from.name_count; i++) {
            if (i > 0) buf.push(' ');
            char[] ef_name = self.interp.symbols.get_name(expr.export_from.names[i]);
            self.buf_append(buf, ef_name);
        }
        self.buf_append(buf, "))");
    }
}

fn void Compiler.serialize_expr_to_buf(Compiler* self, Expr* expr, List{char}* buf) {
    if (expr == null) {
        self.buf_append(buf, "nil");
        return;
    }

    switch (expr.tag) {
        case E_LIT:
            self.serialize_value_to_buf(expr.lit.value, buf);

        case E_VAR:
            char[] name = self.interp.symbols.get_name(expr.var_expr.name);
            self.buf_append(buf, name);

        case E_LAMBDA:
            self.serialize_lambda_to_buf(expr, buf);

        case E_APP:
            buf.push('(');
            self.serialize_expr_to_buf(expr.app.func, buf);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.app.arg, buf);
            buf.push(')');

        case E_IF:
            self.buf_append(buf, "(if ");
            self.serialize_expr_to_buf(expr.if_expr.test, buf);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.if_expr.then_branch, buf);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.if_expr.else_branch, buf);
            buf.push(')');

        case E_LET:
            self.serialize_let_to_buf(expr, buf);

        case E_DEFINE:
            self.buf_append(buf, "(define ");
            char[] def_name = self.interp.symbols.get_name(expr.define.name);
            self.buf_append(buf, def_name);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.define.value, buf);
            buf.push(')');

        case E_QUOTE:
            buf.push('\'');
            self.serialize_value_to_buf(expr.quote.datum, buf);

        case E_RESET:
            self.buf_append(buf, "(reset ");
            self.serialize_expr_to_buf(expr.reset.body, buf);
            buf.push(')');

        case E_SHIFT:
            self.buf_append(buf, "(shift ");
            char[] k_name = self.interp.symbols.get_name(expr.shift.k_name);
            self.buf_append(buf, k_name);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.shift.body, buf);
            buf.push(')');

        case E_PERFORM:
            self.buf_append(buf, "(perform ");
            char[] tag_name = self.interp.symbols.get_name(expr.perform.tag);
            self.buf_append(buf, tag_name);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.perform.arg, buf);
            buf.push(')');

        case E_RESOLVE:
            self.buf_append(buf, "(resolve ");
            self.serialize_expr_to_buf(expr.resolve.value, buf);
            buf.push(')');

        case E_HANDLE:
            self.serialize_handle_to_buf(expr, buf);

        case E_AND:
            self.buf_append(buf, "(and ");
            self.serialize_expr_to_buf(expr.and_expr.left, buf);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.and_expr.right, buf);
            buf.push(')');

        case E_OR:
            self.buf_append(buf, "(or ");
            self.serialize_expr_to_buf(expr.or_expr.left, buf);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.or_expr.right, buf);
            buf.push(')');

        case E_BEGIN:
            self.buf_append(buf, "(begin");
            for (usz i = 0; i < expr.begin.expr_count; i++) {
                buf.push(' ');
                self.serialize_expr_to_buf(expr.begin.exprs[i], buf);
            }
            buf.push(')');

        case E_CALL:
            self.serialize_call_to_buf(expr, buf);

        case E_SET:
            self.buf_append(buf, "(set! ");
            char[] set_name = self.interp.symbols.get_name(expr.set_expr.name);
            self.buf_append(buf, set_name);
            buf.push(' ');
            self.serialize_expr_to_buf(expr.set_expr.value, buf);
            buf.push(')');

        case E_MATCH:
            self.serialize_match_to_buf(expr, buf);

        case E_INDEX:
            self.serialize_expr_to_buf(expr.index.collection, buf);
            self.buf_append(buf, ".[");
            self.serialize_expr_to_buf(expr.index.index, buf);
            buf.push(']');

        case E_PATH:
            for (usz i = 0; i < expr.path.segment_count; i++) {
                if (i > 0) buf.push('.');
                char[] seg = self.interp.symbols.get_name(expr.path.segments[i]);
                self.buf_append(buf, seg);
            }

        case E_QUASIQUOTE:
            buf.push('`');
            self.serialize_expr_to_buf(expr.quasiquote.body, buf);

        case E_UNQUOTE:
            buf.push(',');
            self.serialize_expr_to_buf(expr.unquote.body, buf);

        case E_UNQUOTE_SPLICING:
            self.buf_append(buf, ",@");
            self.serialize_expr_to_buf(expr.unquote_splicing.body, buf);

        case E_DEFMACRO:
            self.serialize_defmacro_to_buf(expr, buf);

        case E_MODULE:
            self.serialize_module_to_buf(expr, buf);

        case E_IMPORT:
            self.serialize_import_to_buf(expr, buf);

        case E_EXPORT_FROM:
            self.serialize_export_from_to_buf(expr, buf);

        default:
            self.buf_append(buf, "nil");
    }
}

fn void Compiler.serialize_value_to_buf(Compiler* self, Value* v, List{char}* buf) {
    if (v == null || v.tag == NIL) {
        self.buf_append(buf, "nil");
        return;
    }

    switch (v.tag) {
        case INT:
            // Convert int to string
            long n = v.int_val;
            if (n < 0) {
                buf.push('-');
                n = -n;
            }
            if (n == 0) {
                buf.push('0');
            } else {
                char[32] digits;
                usz dpos = 0;
                while (n > 0 && dpos < 31) {
                    digits[dpos++] = (char)('0' + (n % 10));
                    n /= 10;
                }
                for (isz di = (isz)dpos - 1; di >= 0; di--) {
                    buf.push(digits[(usz)di]);
                }
            }

        case DOUBLE:
            char[64] dbuf;
            char[] dslice = io::bprintf(&dbuf, "%.15g", v.double_val)!!;
            self.buf_append(buf, dslice);

        case STRING:
            buf.push('"');
            for (usz i = 0; i < v.str_len; i++) {
                char c = v.str_chars[i];
                if (c == '"') {
                    buf.push('\\');
                    buf.push('"');
                } else if (c == '\\') {
                    buf.push('\\');
                    buf.push('\\');
                } else if (c == '\n') {
                    buf.push('\\');
                    buf.push('n');
                } else {
                    buf.push(c);
                }
            }
            buf.push('"');

        case SYMBOL:
            char[] sym_name = self.interp.symbols.get_name(v.sym_val);
            self.buf_append(buf, sym_name);

        case CONS:
            buf.push('(');
            self.serialize_value_to_buf(v.cons_val.car, buf);
            Value* rest = v.cons_val.cdr;
            while (rest != null && rest.tag == CONS) {
                buf.push(' ');
                self.serialize_value_to_buf(rest.cons_val.car, buf);
                rest = rest.cons_val.cdr;
            }
            if (rest != null && rest.tag != NIL) {
                self.buf_append(buf, " . ");
                self.serialize_value_to_buf(rest, buf);
            }
            buf.push(')');

        default:
            self.buf_append(buf, "nil");
    }
}

fn void Compiler.serialize_pattern_to_buf(Compiler* self, Pattern* pat, List{char}* buf) {
    if (pat == null) {
        buf.push('_');
        return;
    }

    switch (pat.tag) {
        case PAT_WILDCARD:
            buf.push('_');

        case PAT_VAR:
            char[] vname = self.interp.symbols.get_name(pat.var_name);
            self.buf_append(buf, vname);

        case PAT_LIT:
            self.serialize_value_to_buf(pat.lit_value, buf);

        case PAT_QUOTE:
            buf.push('\'');
            self.serialize_value_to_buf(pat.quote_datum, buf);

        case PAT_SEQ:
            buf.push('[');
            for (usz i = 0; i < pat.elem_count; i++) {
                if (i > 0) buf.push(' ');
                self.serialize_pattern_to_buf(pat.elements[i], buf);
            }
            if (pat.rest_pos == REST_MIDDLE) {
                self.buf_append(buf, " .. ");
                char[] rname = self.interp.symbols.get_name(pat.rest_binding);
                self.buf_append(buf, rname);
            }
            buf.push(']');

        case PAT_CONS:
            buf.push('(');
            self.serialize_pattern_to_buf(pat.car_pat, buf);
            self.buf_append(buf, " . ");
            self.serialize_pattern_to_buf(pat.cdr_pat, buf);
            buf.push(')');

        default:
            buf.push('_');
    }
}

fn void Compiler.buf_append(Compiler* self, List{char}* buf, char[] s) {
    foreach (c : s) {
        buf.push(c);
    }
}

/**
 * Emit the serialized form of an expression as a C3 string literal
 * directly into the compiler output. Escapes special characters.
 */
fn void Compiler.emit_serialized_expr(Compiler* self, Expr* expr) {
    List{char} buf;
    self.serialize_expr_to_buf(expr, &buf);

    // Emit the serialized source as a C3 string literal with escaping
    self.emit("\"");
    for (usz i = 0; i < buf.len(); i++) {
        char c = buf.entries[i];
        if (c == '"') {
            self.emit("\\\"");
        } else if (c == '\\') {
            self.emit("\\\\");
        } else if (c == '\n') {
            self.emit("\\n");
        } else if (c == '\t') {
            self.emit("\\t");
        } else {
            self.emit_char(c);
        }
    }
    self.emit("\"");

    buf.free();
}

/**
 * Collect free variables from a continuation expression (reset/shift/handle/perform).
 * These are variables that need to be injected into the interpreter before evaluation.
 * Returns the list of free variables (caller must free).
 */
fn List{SymbolId} Compiler.collect_cont_free_vars(Compiler* self, Expr* expr) {
    List{SymbolId} bound;
    List{SymbolId} free;
    self.for_delegation = true;
    self.find_free_vars(expr, &bound, &free, null);
    self.for_delegation = false;
    bound.free();
    return free;
}

