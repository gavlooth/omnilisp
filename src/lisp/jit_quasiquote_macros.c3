module lisp;

import std::io;
import std::collections::list;
import main;

int g_jit_qq_force_cons_alloc_fail_nth = 0;

fn bool jit_qq_should_force_cons_alloc_fail() @inline {
    if (g_jit_qq_force_cons_alloc_fail_nth <= 0) return false;
    g_jit_qq_force_cons_alloc_fail_nth--;
    return g_jit_qq_force_cons_alloc_fail_nth == 0;
}

fn EvalResult jit_qq_make_cons_result(Value* car, Value* cdr, Interp* interp, char[] message) {
    if (jit_qq_should_force_cons_alloc_fail()) return eval_error(message);
    Value* out = make_cons(interp, car, cdr);
    if (out == null) return eval_error(message);
    if (out.tag == ERROR) return eval_error(out.str_chars[:out.str_len]);
    return eval_ok(out);
}

fn Value* jit_eval_quasiquote(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    EvalResult r = jit_qq_impl(expr.quasiquote.body, env, interp, 0);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

enum QqState : char {
    QQ_ENTER,
    QQ_WRAP_UNQUOTE,
    QQ_WRAP_UNQUOTE_SPLICING,
    QQ_WRAP_QUASIQUOTE,
    QQ_COMBINE_APP,
}

struct QqWorkItem {
    Expr* expr;
    usz depth;
    QqState state;
}

const usz QQ_WORK_INITIAL_CAP = 256;
const usz QQ_RESULT_INITIAL_CAP = 256;

fn EvalResult jit_qq_impl(Expr* tmpl, Env* env, Interp* interp, usz depth) {
    if (tmpl == null) return eval_ok(make_nil(interp));

    QqWorkItem* work = (QqWorkItem*)mem::malloc(QqWorkItem.sizeof * QQ_WORK_INITIAL_CAP);
    if (work == null) return eval_error("quasiquote: out of memory");
    defer mem::free(work);
    usz work_cap = QQ_WORK_INITIAL_CAP;
    usz work_count = 0;

    EvalResult* results = (EvalResult*)mem::malloc(EvalResult.sizeof * QQ_RESULT_INITIAL_CAP);
    if (results == null) return eval_error("quasiquote: out of memory");
    defer mem::free(results);
    usz result_cap = QQ_RESULT_INITIAL_CAP;
    usz result_count = 0;

    work[work_count] = { .expr = tmpl, .depth = depth, .state = QQ_ENTER };
    work_count++;

    bool aborted = false;
    EvalResult abort_result;

    while (work_count > 0 && !aborted) {
        if (interp.flags.resolve_terminal_pending) break;

        work_count--;
        QqWorkItem item = work[work_count];
        Expr* e = item.expr;
        usz d = item.depth;

        switch (item.state) {
            case QQ_ENTER:
                switch (e.tag) {
                    case E_UNQUOTE:
                        if (d == 0) {
                            EvalResult r = jit_eval_to_result(e.unquote.body, env, interp);
                            if (r.error.has_error || interp.flags.resolve_terminal_pending) {
                                aborted = true;
                                abort_result = r;
                            } else {
                                if (result_count >= result_cap) {
                                    usz new_cap = result_cap * 2;
                                    EvalResult* new_results = (EvalResult*)mem::malloc(EvalResult.sizeof * new_cap);
                                    if (new_results == null) return eval_error("quasiquote: out of memory");
                                    for (usz i = 0; i < result_count; i++) new_results[i] = results[i];
                                    mem::free(results);
                                    results = new_results;
                                    result_cap = new_cap;
                                }
                                results[result_count] = r;
                                result_count++;
                            }
                        } else {
                            if (work_count + 2 > work_cap) {
                                usz new_cap = work_cap * 2;
                                QqWorkItem* new_work = (QqWorkItem*)mem::malloc(QqWorkItem.sizeof * new_cap);
                                if (new_work == null) return eval_error("quasiquote: out of memory");
                                for (usz i = 0; i < work_count; i++) new_work[i] = work[i];
                                mem::free(work);
                                work = new_work;
                                work_cap = new_cap;
                            }
                            work[work_count] = { .expr = e, .depth = d, .state = QQ_WRAP_UNQUOTE };
                            work_count++;
                            work[work_count] = { .expr = e.unquote.body, .depth = d - 1, .state = QQ_ENTER };
                            work_count++;
                        }
                    case E_UNQUOTE_SPLICING:
                        if (d == 0) {
                            aborted = true;
                            abort_result = eval_error(",@ in non-list context");
                        } else {
                            if (work_count + 2 > work_cap) {
                                usz new_cap = work_cap * 2;
                                QqWorkItem* new_work = (QqWorkItem*)mem::malloc(QqWorkItem.sizeof * new_cap);
                                if (new_work == null) return eval_error("quasiquote: out of memory");
                                for (usz i = 0; i < work_count; i++) new_work[i] = work[i];
                                mem::free(work);
                                work = new_work;
                                work_cap = new_cap;
                            }
                            work[work_count] = { .expr = e, .depth = d, .state = QQ_WRAP_UNQUOTE_SPLICING };
                            work_count++;
                            work[work_count] = { .expr = e.unquote_splicing.body, .depth = d - 1, .state = QQ_ENTER };
                            work_count++;
                        }
                    case E_QUASIQUOTE:
                        if (work_count + 2 > work_cap) {
                            usz new_cap = work_cap * 2;
                            QqWorkItem* new_work = (QqWorkItem*)mem::malloc(QqWorkItem.sizeof * new_cap);
                            if (new_work == null) return eval_error("quasiquote: out of memory");
                            for (usz i = 0; i < work_count; i++) new_work[i] = work[i];
                            mem::free(work);
                            work = new_work;
                            work_cap = new_cap;
                        }
                        work[work_count] = { .expr = e, .depth = d, .state = QQ_WRAP_QUASIQUOTE };
                        work_count++;
                        work[work_count] = { .expr = e.quasiquote.body, .depth = d + 1, .state = QQ_ENTER };
                        work_count++;
                    case E_LIT:
                        if (result_count >= result_cap) {
                            usz new_cap = result_cap * 2;
                            EvalResult* new_results = (EvalResult*)mem::malloc(EvalResult.sizeof * new_cap);
                            if (new_results == null) return eval_error("quasiquote: out of memory");
                            for (usz i = 0; i < result_count; i++) new_results[i] = results[i];
                            mem::free(results);
                            results = new_results;
                            result_cap = new_cap;
                        }
                        results[result_count] = eval_ok(e.lit.value);
                        result_count++;
                    case E_VAR:
                        if (result_count >= result_cap) {
                            usz new_cap = result_cap * 2;
                            EvalResult* new_results = (EvalResult*)mem::malloc(EvalResult.sizeof * new_cap);
                            if (new_results == null) return eval_error("quasiquote: out of memory");
                            for (usz i = 0; i < result_count; i++) new_results[i] = results[i];
                            mem::free(results);
                            results = new_results;
                            result_cap = new_cap;
                        }
                        results[result_count] = eval_ok(make_symbol(interp, e.var_expr.name));
                        result_count++;
                    case E_CALL:
                        EvalResult r = jit_qq_expand_call(e, env, interp, d);
                        if (r.error.has_error || interp.flags.resolve_terminal_pending) {
                            aborted = true;
                            abort_result = r;
                        } else {
                            if (result_count >= result_cap) {
                                usz new_cap = result_cap * 2;
                                EvalResult* new_results = (EvalResult*)mem::malloc(EvalResult.sizeof * new_cap);
                                if (new_results == null) return eval_error("quasiquote: out of memory");
                                for (usz i = 0; i < result_count; i++) new_results[i] = results[i];
                                mem::free(results);
                                results = new_results;
                                result_cap = new_cap;
                            }
                            results[result_count] = r;
                            result_count++;
                        }
                    case E_APP:
                        if (work_count + 3 > work_cap) {
                            usz new_cap = work_cap * 2;
                            QqWorkItem* new_work = (QqWorkItem*)mem::malloc(QqWorkItem.sizeof * new_cap);
                            if (new_work == null) return eval_error("quasiquote: out of memory");
                            for (usz i = 0; i < work_count; i++) new_work[i] = work[i];
                            mem::free(work);
                            work = new_work;
                            work_cap = new_cap;
                        }
                        work[work_count] = { .expr = e, .depth = d, .state = QQ_COMBINE_APP };
                        work_count++;
                        work[work_count] = { .expr = e.app.arg, .depth = d, .state = QQ_ENTER };
                        work_count++;
                        work[work_count] = { .expr = e.app.func, .depth = d, .state = QQ_ENTER };
                        work_count++;
                    case E_QUOTE:
                        if (result_count >= result_cap) {
                            usz new_cap = result_cap * 2;
                            EvalResult* new_results = (EvalResult*)mem::malloc(EvalResult.sizeof * new_cap);
                            if (new_results == null) return eval_error("quasiquote: out of memory");
                            for (usz i = 0; i < result_count; i++) new_results[i] = results[i];
                            mem::free(results);
                            results = new_results;
                            result_cap = new_cap;
                        }
                        results[result_count] = eval_ok(e.quote.datum);
                        result_count++;
                    default:
                        if (result_count >= result_cap) {
                            usz new_cap = result_cap * 2;
                            EvalResult* new_results = (EvalResult*)mem::malloc(EvalResult.sizeof * new_cap);
                            if (new_results == null) return eval_error("quasiquote: out of memory");
                            for (usz i = 0; i < result_count; i++) new_results[i] = results[i];
                            mem::free(results);
                            results = new_results;
                            result_cap = new_cap;
                        }
                        results[result_count] = eval_ok(make_nil(interp));
                        result_count++;
                }
            case QQ_WRAP_UNQUOTE: {
                result_count--;
                EvalResult inner = results[result_count];
                if (inner.error.has_error || interp.flags.resolve_terminal_pending) {
                    aborted = true;
                    abort_result = inner;
                    break;
                }
                Value* sym = make_symbol(interp, interp.sym_unquote);
                EvalResult rest = jit_qq_make_cons_result(inner.value, make_nil(interp), interp, "quasiquote: failed to allocate pair");
                if (rest.error.has_error || interp.flags.resolve_terminal_pending) {
                    aborted = true;
                    abort_result = rest;
                    break;
                }
                EvalResult final = jit_qq_make_cons_result(sym, rest.value, interp, "quasiquote: failed to allocate pair");
                if (final.error.has_error || interp.flags.resolve_terminal_pending) {
                    aborted = true;
                    abort_result = final;
                    break;
                }
                if (result_count >= result_cap) {
                    usz new_cap = result_cap * 2;
                    EvalResult* new_results = (EvalResult*)mem::malloc(EvalResult.sizeof * new_cap);
                    if (new_results == null) return eval_error("quasiquote: out of memory");
                    for (usz i = 0; i < result_count; i++) new_results[i] = results[i];
                    mem::free(results);
                    results = new_results;
                    result_cap = new_cap;
                }
                results[result_count] = final;
                result_count++;
            }
            case QQ_WRAP_UNQUOTE_SPLICING: {
                result_count--;
                EvalResult inner = results[result_count];
                if (inner.error.has_error || interp.flags.resolve_terminal_pending) {
                    aborted = true;
                    abort_result = inner;
                    break;
                }
                Value* sym = make_symbol(interp, interp.sym_unquote_splicing);
                EvalResult rest = jit_qq_make_cons_result(inner.value, make_nil(interp), interp, "quasiquote: failed to allocate pair");
                if (rest.error.has_error || interp.flags.resolve_terminal_pending) {
                    aborted = true;
                    abort_result = rest;
                    break;
                }
                EvalResult final = jit_qq_make_cons_result(sym, rest.value, interp, "quasiquote: failed to allocate pair");
                if (final.error.has_error || interp.flags.resolve_terminal_pending) {
                    aborted = true;
                    abort_result = final;
                    break;
                }
                if (result_count >= result_cap) {
                    usz new_cap = result_cap * 2;
                    EvalResult* new_results = (EvalResult*)mem::malloc(EvalResult.sizeof * new_cap);
                    if (new_results == null) return eval_error("quasiquote: out of memory");
                    for (usz i = 0; i < result_count; i++) new_results[i] = results[i];
                    mem::free(results);
                    results = new_results;
                    result_cap = new_cap;
                }
                results[result_count] = final;
                result_count++;
            }
            case QQ_WRAP_QUASIQUOTE: {
                result_count--;
                EvalResult inner = results[result_count];
                if (inner.error.has_error || interp.flags.resolve_terminal_pending) {
                    aborted = true;
                    abort_result = inner;
                    break;
                }
                Value* sym = make_symbol(interp, interp.sym_quasiquote);
                EvalResult rest = jit_qq_make_cons_result(inner.value, make_nil(interp), interp, "quasiquote: failed to allocate pair");
                if (rest.error.has_error || interp.flags.resolve_terminal_pending) {
                    aborted = true;
                    abort_result = rest;
                    break;
                }
                EvalResult final = jit_qq_make_cons_result(sym, rest.value, interp, "quasiquote: failed to allocate pair");
                if (final.error.has_error || interp.flags.resolve_terminal_pending) {
                    aborted = true;
                    abort_result = final;
                    break;
                }
                if (result_count >= result_cap) {
                    usz new_cap = result_cap * 2;
                    EvalResult* new_results = (EvalResult*)mem::malloc(EvalResult.sizeof * new_cap);
                    if (new_results == null) return eval_error("quasiquote: out of memory");
                    for (usz i = 0; i < result_count; i++) new_results[i] = results[i];
                    mem::free(results);
                    results = new_results;
                    result_cap = new_cap;
                }
                results[result_count] = final;
                result_count++;
            }
            case QQ_COMBINE_APP: {
                result_count--;
                EvalResult a = results[result_count];
                if (a.error.has_error || interp.flags.resolve_terminal_pending) {
                    aborted = true;
                    abort_result = a;
                    break;
                }
                result_count--;
                EvalResult f = results[result_count];
                if (f.error.has_error || interp.flags.resolve_terminal_pending) {
                    aborted = true;
                    abort_result = f;
                    break;
                }
                EvalResult rest = jit_qq_make_cons_result(a.value, make_nil(interp), interp, "quasiquote: failed to allocate pair");
                if (rest.error.has_error || interp.flags.resolve_terminal_pending) {
                    aborted = true;
                    abort_result = rest;
                    break;
                }
                EvalResult final = jit_qq_make_cons_result(f.value, rest.value, interp, "quasiquote: failed to allocate pair");
                if (final.error.has_error || interp.flags.resolve_terminal_pending) {
                    aborted = true;
                    abort_result = final;
                    break;
                }
                if (result_count >= result_cap) {
                    usz new_cap = result_cap * 2;
                    EvalResult* new_results = (EvalResult*)mem::malloc(EvalResult.sizeof * new_cap);
                    if (new_results == null) return eval_error("quasiquote: out of memory");
                    for (usz i = 0; i < result_count; i++) new_results[i] = results[i];
                    mem::free(results);
                    results = new_results;
                    result_cap = new_cap;
                }
                results[result_count] = final;
                result_count++;
            }
        }
    }

    if (aborted) return abort_result;
    if (result_count == 0) return eval_ok(make_nil(interp));
    result_count--;
    return results[result_count];
}

fn EvalResult jit_qq_expand_elements(Expr*[] elements, Env* env, Interp* interp, usz depth) {
    Value* result = make_nil(interp);
    for (usz i = elements.len; i > 0; i--) {
        Expr* elem = elements[i - 1];
        if (elem.tag == E_UNQUOTE_SPLICING && depth == 0) {
            EvalResult splice_result = jit_eval_to_result(elem.unquote_splicing.body, env, interp);
            if (splice_result.error.has_error) return splice_result;
            if (interp.flags.resolve_terminal_pending) return splice_result;
            Value* spliced = splice_result.value;
            if (is_nil(spliced)) {
                // Nothing to splice
            } else if (is_cons(spliced)) {
                usz item_cap = 16;
                Value** items = (Value**)mem::malloc(Value*.sizeof * item_cap);
                if (items == null) return eval_error("quasiquote: out of memory");
                defer mem::free(items);
                usz item_count = 0;
                Value* tmp = spliced;
                while (is_cons(tmp)) {
                    if (item_count >= item_cap) {
                        usz new_cap = item_cap * 2;
                        Value** new_items = (Value**)mem::malloc(Value*.sizeof * new_cap);
                        if (new_items == null) return eval_error("quasiquote: out of memory");
                        for (usz k = 0; k < item_count; k++) new_items[k] = items[k];
                        mem::free(items);
                        items = new_items;
                        item_cap = new_cap;
                    }
                    items[item_count] = tmp.cons_val.car;
                    item_count++;
                    tmp = tmp.cons_val.cdr;
                }
                if (!is_nil(tmp)) {
                    return eval_error(",@ value is not a proper list");
                }
                for (usz j = item_count; j > 0; j--) {
                    EvalResult next = jit_qq_make_cons_result(items[j - 1], result, interp, "quasiquote: failed to allocate pair");
                    if (next.error.has_error) return next;
                    result = next.value;
                }
            } else {
                return eval_error(",@ value is not a list");
            }
        } else {
            EvalResult expanded = jit_qq_impl(elem, env, interp, depth);
            if (expanded.error.has_error) return expanded;
            if (interp.flags.resolve_terminal_pending) return expanded;
            EvalResult next = jit_qq_make_cons_result(expanded.value, result, interp, "quasiquote: failed to allocate pair");
            if (next.error.has_error) return next;
            result = next.value;
        }
    }
    return eval_ok(result);
}

fn Expr* jit_qq_call_element(Expr* tmpl, usz index) @inline {
    if (index == 0) return tmpl.call.func;
    return tmpl.call.args[index - 1];
}

fn EvalResult jit_qq_expand_call(Expr* tmpl, Env* env, Interp* interp, usz depth) {
    usz total = tmpl.call.arg_count + 1;
    Value* result = make_nil(interp);
    for (usz i = total; i > 0; i--) {
        Expr* elem = jit_qq_call_element(tmpl, i - 1);
        if (elem.tag == E_UNQUOTE_SPLICING && depth == 0) {
            EvalResult splice_result = jit_eval_to_result(elem.unquote_splicing.body, env, interp);
            if (splice_result.error.has_error) return splice_result;
            if (interp.flags.resolve_terminal_pending) return splice_result;
            Value* spliced = splice_result.value;
            if (is_nil(spliced)) {
                // Nothing to splice
            } else if (is_cons(spliced)) {
                usz item_cap = 16;
                Value** items = (Value**)mem::malloc(Value*.sizeof * item_cap);
                if (items == null) return eval_error("quasiquote: out of memory");
                defer mem::free(items);
                usz item_count = 0;
                Value* tmp = spliced;
                while (is_cons(tmp)) {
                    if (item_count >= item_cap) {
                        usz new_cap = item_cap * 2;
                        Value** new_items = (Value**)mem::malloc(Value*.sizeof * new_cap);
                        if (new_items == null) return eval_error("quasiquote: out of memory");
                        for (usz k = 0; k < item_count; k++) new_items[k] = items[k];
                        mem::free(items);
                        items = new_items;
                        item_cap = new_cap;
                    }
                    items[item_count] = tmp.cons_val.car;
                    item_count++;
                    tmp = tmp.cons_val.cdr;
                }
                if (!is_nil(tmp)) {
                    return eval_error(",@ value is not a proper list");
                }
                for (usz j = item_count; j > 0; j--) {
                    EvalResult next = jit_qq_make_cons_result(items[j - 1], result, interp, "quasiquote: failed to allocate pair");
                    if (next.error.has_error) return next;
                    result = next.value;
                }
            } else {
                return eval_error(",@ value is not a list");
            }
        } else {
            EvalResult expanded = jit_qq_impl(elem, env, interp, depth);
            if (expanded.error.has_error) return expanded;
            if (interp.flags.resolve_terminal_pending) return expanded;
            EvalResult next = jit_qq_make_cons_result(expanded.value, result, interp, "quasiquote: failed to allocate pair");
            if (next.error.has_error) return next;
            result = next.value;
        }
    }
    return eval_ok(result);
}

// define-macro helper — called from JIT-compiled code.
// Delegates to the interpreter's eval_define_macro.
fn Value* jit_eval_define_macro(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    EvalResult r = eval_define_macro(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

// JIT-native effects — no eval() dependency.
fn Value* jit_exec_reset(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    EvalResult r = jit_reset_impl(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn Value* jit_exec_shift(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    // Returns Value* directly (fits in RAX, no hidden pointer).
    // EvalResult is >16 bytes, so the System V ABI uses a hidden stack pointer.
    // After stack_ctx_clone that pointer is stale.
    return jit_shift_impl(expr, env, interp);
}

fn Value* jit_exec_handle(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    EvalResult r = jit_handle_impl(expr, env, interp);
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}

fn Value* jit_exec_perform(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    return jit_signal_impl(expr, env, interp);
}
