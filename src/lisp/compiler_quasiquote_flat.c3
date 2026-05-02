module lisp;

import std::io;
import std::collections::list;
import main;

// =============================================================================
// SECTION 8a: STATEMENT-LEVEL COMPILATION (quasiquote lowering)
// =============================================================================

enum AotQqState : char {
    AOT_QQ_ENTER,
    AOT_QQ_WRAP_UNQUOTE,
    AOT_QQ_WRAP_UNQUOTE_SPLICING,
    AOT_QQ_WRAP_QUASIQUOTE,
}

struct AotQqWorkItem {
    Expr* expr;
    usz depth;
    AotQqState state;
}

const usz AOT_QQ_WORK_INITIAL_CAP = 256;
const usz AOT_QQ_RESULT_INITIAL_CAP = 256;

/**
 * compile_qq_flat — Flat-style quasiquote compilation.
 * Emits statements that build the quoted structure, storing result in _rN.
 * At depth 0, unquote sub-expressions are compiled as normal expressions.
 */
fn usz Compiler.compile_qq_marker_pair(Compiler* self, char[] marker, usz inner_r) {
    if (self.has_error || inner_r == usz.max) return usz.max;
    self.emit_return_if_sequence_should_stop(inner_r);
    usz nil_r = self.emit_nil_temp();

    usz rest_r = self.next_result();
    self.emit_temp_decl(rest_r);
    self.emit(" = aot::cons(");
    self.emit_temp_ref(inner_r);
    self.emit(", ");
    self.emit_temp_ref(nil_r);
    self.emit(");\n");
    self.emit_return_if_sequence_should_stop(rest_r);

    usz sym_r = self.next_result();
    self.emit_temp_decl(sym_r);
    self.emit(" = aot::make_symbol(\"");
    self.emit(marker);
    self.emit("\");\n");

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::cons(");
    self.emit_temp_ref(sym_r);
    self.emit(", ");
    self.emit_temp_ref(rest_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_qq_var_symbol(Compiler* self, SymbolId sym) {
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::make_symbol(\"");
    char[] name = self.interp.symbols.get_name(sym);
    self.emit_escaped(name);
    self.emit("\");\n");
    return id;
}

fn usz Compiler.compile_qq_app_list(Compiler* self, Expr* tmpl, usz depth) {
    usz func_r = self.compile_qq_flat(tmpl.app.func, depth);
    if (self.has_error || func_r == usz.max) return usz.max;
    self.emit_return_if_sequence_should_stop(func_r);
    usz arg_r = self.compile_qq_flat(tmpl.app.arg, depth);
    if (self.has_error || arg_r == usz.max) return usz.max;
    self.emit_return_if_sequence_should_stop(arg_r);
    usz nil_r = self.emit_nil_temp();

    usz inner_r = self.next_result();
    self.emit_temp_decl(inner_r);
    self.emit(" = aot::cons(");
    self.emit_temp_ref(arg_r);
    self.emit(", ");
    self.emit_temp_ref(nil_r);
    self.emit(");\n");
    if (self.has_error || inner_r == usz.max) return usz.max;
    self.emit_return_if_sequence_should_stop(inner_r);

    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::cons(");
    self.emit_temp_ref(func_r);
    self.emit(", ");
    self.emit_temp_ref(inner_r);
    self.emit(");\n");
    return id;
}

fn usz Compiler.compile_qq_flat(Compiler* self, Expr* tmpl, usz depth) {
    if (tmpl == null) {
        return self.emit_nil_temp();
    }

    AotQqWorkItem* work = (AotQqWorkItem*)mem::malloc(AotQqWorkItem.sizeof * AOT_QQ_WORK_INITIAL_CAP);
    if (work == null) {
        self.set_compile_error("quasiquote: out of memory");
        return usz.max;
    }
    defer mem::free(work);
    usz work_cap = AOT_QQ_WORK_INITIAL_CAP;
    usz work_count = 0;

    usz* results = (usz*)mem::malloc(usz.sizeof * AOT_QQ_RESULT_INITIAL_CAP);
    if (results == null) {
        self.set_compile_error("quasiquote: out of memory");
        return usz.max;
    }
    defer mem::free(results);
    usz result_cap = AOT_QQ_RESULT_INITIAL_CAP;
    usz result_count = 0;

    work[work_count] = { .expr = tmpl, .depth = depth, .state = AOT_QQ_ENTER };
    work_count++;

    while (work_count > 0 && !self.has_error) {
        work_count--;
        AotQqWorkItem item = work[work_count];
        Expr* e = item.expr;
        usz d = item.depth;

        switch (item.state) {
            case AOT_QQ_ENTER:
                switch (e.tag) {
                    case E_UNQUOTE:
                        if (d == 0) {
                            usz body_r = self.compile_to_temp(e.unquote.body);
                            if (self.has_error || body_r == usz.max) return usz.max;
                            self.emit_return_if_sequence_should_stop(body_r);
                            if (result_count >= result_cap) {
                                usz new_cap = result_cap * 2;
                                usz* new_results = (usz*)mem::malloc(usz.sizeof * new_cap);
                                if (new_results == null) {
                                    self.set_compile_error("quasiquote: out of memory");
                                    return usz.max;
                                }
                                for (usz i = 0; i < result_count; i++) new_results[i] = results[i];
                                mem::free(results);
                                results = new_results;
                                result_cap = new_cap;
                            }
                            results[result_count] = body_r;
                            result_count++;
                        } else {
                            if (work_count + 2 > work_cap) {
                                usz new_cap = work_cap * 2;
                                AotQqWorkItem* new_work = (AotQqWorkItem*)mem::malloc(AotQqWorkItem.sizeof * new_cap);
                                if (new_work == null) {
                                    self.set_compile_error("quasiquote: out of memory");
                                    return usz.max;
                                }
                                for (usz i = 0; i < work_count; i++) new_work[i] = work[i];
                                mem::free(work);
                                work = new_work;
                                work_cap = new_cap;
                            }
                            work[work_count] = { .expr = e, .depth = d, .state = AOT_QQ_WRAP_UNQUOTE };
                            work_count++;
                            work[work_count] = { .expr = e.unquote.body, .depth = d - 1, .state = AOT_QQ_ENTER };
                            work_count++;
                        }
                    case E_UNQUOTE_SPLICING:
                        if (d == 0) {
                            self.set_compile_error("compiler: unquote-splicing is only valid in quasiquote list context");
                            return usz.max;
                        }
                        if (work_count + 2 > work_cap) {
                            usz new_cap = work_cap * 2;
                            AotQqWorkItem* new_work = (AotQqWorkItem*)mem::malloc(AotQqWorkItem.sizeof * new_cap);
                            if (new_work == null) {
                                self.set_compile_error("quasiquote: out of memory");
                                return usz.max;
                            }
                            for (usz i = 0; i < work_count; i++) new_work[i] = work[i];
                            mem::free(work);
                            work = new_work;
                            work_cap = new_cap;
                        }
                        work[work_count] = { .expr = e, .depth = d, .state = AOT_QQ_WRAP_UNQUOTE_SPLICING };
                        work_count++;
                        work[work_count] = { .expr = e.unquote_splicing.body, .depth = d - 1, .state = AOT_QQ_ENTER };
                        work_count++;
                    case E_QUASIQUOTE:
                        if (work_count + 2 > work_cap) {
                            usz new_cap = work_cap * 2;
                            AotQqWorkItem* new_work = (AotQqWorkItem*)mem::malloc(AotQqWorkItem.sizeof * new_cap);
                            if (new_work == null) {
                                self.set_compile_error("quasiquote: out of memory");
                                return usz.max;
                            }
                            for (usz i = 0; i < work_count; i++) new_work[i] = work[i];
                            mem::free(work);
                            work = new_work;
                            work_cap = new_cap;
                        }
                        work[work_count] = { .expr = e, .depth = d, .state = AOT_QQ_WRAP_QUASIQUOTE };
                        work_count++;
                        work[work_count] = { .expr = e.quasiquote.body, .depth = d + 1, .state = AOT_QQ_ENTER };
                        work_count++;
                    case E_LIT: {
                        usz id = self.next_result();
                        self.emit_temp_decl(id);
                        self.emit(" = ");
                        self.compile_literal(e.lit.value);
                        self.emit(";\n");
                        if (result_count >= result_cap) {
                            usz new_cap = result_cap * 2;
                            usz* new_results = (usz*)mem::malloc(usz.sizeof * new_cap);
                            if (new_results == null) {
                                self.set_compile_error("quasiquote: out of memory");
                                return usz.max;
                            }
                            for (usz i = 0; i < result_count; i++) new_results[i] = results[i];
                            mem::free(results);
                            results = new_results;
                            result_cap = new_cap;
                        }
                        results[result_count] = id;
                        result_count++;
                    }
                    case E_VAR: {
                        usz id = self.compile_qq_var_symbol(e.var_expr.name);
                        if (self.has_error || id == usz.max) return usz.max;
                        if (result_count >= result_cap) {
                            usz new_cap = result_cap * 2;
                            usz* new_results = (usz*)mem::malloc(usz.sizeof * new_cap);
                            if (new_results == null) {
                                self.set_compile_error("quasiquote: out of memory");
                                return usz.max;
                            }
                            for (usz i = 0; i < result_count; i++) new_results[i] = results[i];
                            mem::free(results);
                            results = new_results;
                            result_cap = new_cap;
                        }
                        results[result_count] = id;
                        result_count++;
                    }
                    case E_QUOTE: {
                        usz id = self.next_result();
                        self.emit_temp_decl(id);
                        self.emit(" = ");
                        self.compile_literal(e.quote.datum);
                        self.emit(";\n");
                        if (result_count >= result_cap) {
                            usz new_cap = result_cap * 2;
                            usz* new_results = (usz*)mem::malloc(usz.sizeof * new_cap);
                            if (new_results == null) {
                                self.set_compile_error("quasiquote: out of memory");
                                return usz.max;
                            }
                            for (usz i = 0; i < result_count; i++) new_results[i] = results[i];
                            mem::free(results);
                            results = new_results;
                            result_cap = new_cap;
                        }
                        results[result_count] = id;
                        result_count++;
                    }
                    case E_APP: {
                        usz app_r = self.compile_qq_app_list(e, d);
                        if (self.has_error || app_r == usz.max) return usz.max;
                        if (result_count >= result_cap) {
                            usz new_cap = result_cap * 2;
                            usz* new_results = (usz*)mem::malloc(usz.sizeof * new_cap);
                            if (new_results == null) {
                                self.set_compile_error("quasiquote: out of memory");
                                return usz.max;
                            }
                            for (usz i = 0; i < result_count; i++) new_results[i] = results[i];
                            mem::free(results);
                            results = new_results;
                            result_cap = new_cap;
                        }
                        results[result_count] = app_r;
                        result_count++;
                    }
                    case E_CALL: {
                        usz call_r = self.compile_qq_call_flat(e, d);
                        if (self.has_error || call_r == usz.max) return usz.max;
                        if (result_count >= result_cap) {
                            usz new_cap = result_cap * 2;
                            usz* new_results = (usz*)mem::malloc(usz.sizeof * new_cap);
                            if (new_results == null) {
                                self.set_compile_error("quasiquote: out of memory");
                                return usz.max;
                            }
                            for (usz i = 0; i < result_count; i++) new_results[i] = results[i];
                            mem::free(results);
                            results = new_results;
                            result_cap = new_cap;
                        }
                        results[result_count] = call_r;
                        result_count++;
                    }
                    default:
                        self.set_compile_error("compiler: unsupported quasiquote template expression in AOT lowering");
                        return usz.max;
                }
            case AOT_QQ_WRAP_UNQUOTE: {
                result_count--;
                usz inner_r = results[result_count];
                if (self.has_error || inner_r == usz.max) return usz.max;
                usz id = self.compile_qq_marker_pair("unquote", inner_r);
                if (self.has_error || id == usz.max) return usz.max;
                results[result_count] = id;
                result_count++;
            }
            case AOT_QQ_WRAP_UNQUOTE_SPLICING: {
                result_count--;
                usz inner_r = results[result_count];
                if (self.has_error || inner_r == usz.max) return usz.max;
                usz id = self.compile_qq_marker_pair("unquote-splicing", inner_r);
                if (self.has_error || id == usz.max) return usz.max;
                results[result_count] = id;
                result_count++;
            }
            case AOT_QQ_WRAP_QUASIQUOTE: {
                result_count--;
                usz inner_r = results[result_count];
                if (self.has_error || inner_r == usz.max) return usz.max;
                usz id = self.compile_qq_marker_pair("quasiquote", inner_r);
                if (self.has_error || id == usz.max) return usz.max;
                results[result_count] = id;
                result_count++;
            }
        }
    }

    if (self.has_error) return usz.max;
    if (result_count == 0) return self.emit_nil_temp();
    result_count--;
    return results[result_count];
}
