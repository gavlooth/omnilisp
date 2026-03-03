module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 5: MAIN COMPILATION
// =============================================================================

/**
 * Compile a complete program to C3 source code.
 * Prepends stdlib definitions so HOFs are available.
 */
fn char[] Compiler.compile_program(Compiler* self, char[] source) {
    // Prepend stdlib prelude to user source using bulk copy
    usz total = STDLIB_PRELUDE.len + 1 + source.len;
    char* full_buf = (char*)mem::malloc(total);
    defer mem::free(full_buf);
    mem::copy(full_buf, STDLIB_PRELUDE.ptr, STDLIB_PRELUDE.len);
    full_buf[STDLIB_PRELUDE.len] = ' ';
    mem::copy(&full_buf[STDLIB_PRELUDE.len + 1], source.ptr, source.len);
    char[] full = full_buf[:total];

    // Parse the program
    Lexer lex;
    lex.init(full);
    Parser p;
    p.init(&lex, self.interp);

    List{Expr*} exprs;
    while (!lex.at_end() && !p.has_error) {
        Expr* e = p.parse_expr();
        if (e != null) {
            exprs.push(e);
        }
    }
    defer exprs.free();

    if (p.has_error) {
        io::printfn("Syntax Error at line %d, column %d: %s", (int)p.error_line, (int)p.error_col, (ZString)&p.error_msg[0]);
        return "";
    }

    // First pass: collect all top-level defines (including inside module bodies)
    foreach (expr : exprs) {
        if (expr.tag == E_DEFINE) {
            self.defined_globals.push(expr.define.name);
        } else if (expr.tag == E_MODULE) {
            // Module body defines become globals when compiled inline
            for (usz mi = 0; mi < expr.module_expr.body_count; mi++) {
                if (expr.module_expr.body[mi].tag == E_DEFINE) {
                    self.defined_globals.push(expr.module_expr.body[mi].define.name);
                }
            }
        }
    }

    // Prescan for mutable captures (before lambda scanning)
    foreach (expr : exprs) {
        self.prescan_mutable_captures(expr);
    }

    // Second pass: scan all lambdas to generate definitions
    foreach (expr : exprs) {
        self.scan_lambdas(expr);
    }

    // D2: Two-pass compilation — compile all code into a temp buffer first
    // so we discover all referenced prims before emitting global declarations.
    DString code_buf;
    code_buf.init(mem, 8192);
    DString orig_output = self.output;
    self.output = code_buf;

    // Emit lambda struct and function definitions (compiles lambda bodies → discovers prims)
    self.emit_lambda_definitions();

    // Mark where lambda defs end and main() begins
    usz lambda_defs_end = self.output.str_view().len;

    // Emit main function body
    self.emit("fn int main() {\n");
    self.indent++;
    self.emit_line("aot::aot_init();");
    self.emit_newline();

    // Initialize all global variables to nil
    if (self.defined_globals.len() > 0) {
        self.emit_line("// Initialize global variables to nil");
        foreach (name : self.defined_globals) {
            self.emit_indent();
            self.emit_symbol_name(name);
            self.emit(" = aot::make_nil();\n");
        }
        self.emit_newline();
    }

    // Mark position for prim init insertion
    usz prim_init_pos = self.output.str_view().len;

    self.emit_newline();

    // Clear declared vars for main scope
    self.declared_vars.free();

    // Compile each top-level expression
    isz last_non_define = -1;
    if (self.print_last) {
        for (usz i = 0; i < exprs.len(); i++) {
            if (exprs[i].tag != E_DEFINE) {
                last_non_define = (isz)i;
            }
        }
    }

    bool print_all_active = false;
    for (usz i = 0; i < exprs.len(); i++) {
        if (exprs[i].tag == E_DEFINE) {
            char[] dname = self.interp.symbols.get_name(exprs[i].define.name);
            if (self.str_eq(dname, "__e2e_start__")) {
                print_all_active = true;
                usz init_r = self.compile_to_temp(exprs[i].define.value);
                self.emit_indent();
                self.emit_symbol_name(exprs[i].define.name);
                self.emit(" = ");
                self.emit_temp_ref(init_r);
                self.emit(";\n");
                continue;
            }
        }

        bool should_print = (self.print_all && print_all_active)
            || (self.print_last && (isz)i == last_non_define);

        if (should_print) {
            usz r = self.compile_to_temp(exprs[i]);
            self.emit_indent();
            self.emit("aot::print_value(");
            self.emit_temp_ref(r);
            self.emit(");\n");
        } else if (exprs[i].tag == E_DEFINE) {
            usz init_r = self.compile_to_temp(exprs[i].define.value);
            self.emit_indent();
            self.emit_symbol_name(exprs[i].define.name);
            self.emit(" = ");
            self.emit_temp_ref(init_r);
            self.emit(";\n");
        } else {
            self.compile_to_temp(exprs[i]);
        }
    }

    self.emit_main_end();

    // Swap back to original output — now we know all referenced prims
    code_buf = self.output;
    self.output = orig_output;

    // Assemble final output: prelude + globals + compiled code (with prim init inserted)
    self.emit_prelude();
    self.emit_global_declarations();

    char[] code_str = code_buf.str_view();

    if (self.referenced_prims.len() > 0 && prim_init_pos <= code_str.len) {
        // Emit code before prim init point
        self.emit(code_str[:prim_init_pos]);
        // Emit prim init code
        self.emit("    // Initialize cached primitives\n");
        foreach (sym : self.referenced_prims) {
            self.emit("    ");
            self.emit_prim_global_name(sym);
            self.emit(" = ");
            char[] init_code = prim_hash_lookup(sym);
            self.emit(init_code);
            self.emit(";\n");
        }
        self.emit("\n");
        // Emit rest of code after init point
        if (prim_init_pos < code_str.len) {
            self.emit(code_str[prim_init_pos..]);
        }
    } else {
        self.emit(code_str);
    }

    code_buf.free();

    return self.get_output();
}

/**
 * Scan expression tree for lambdas and register them.
 */
fn void Compiler.scan_lambdas(Compiler* self, Expr* expr) {
    List{SymbolId} empty;
    self.scan_lambdas_with_scope(expr, &empty);
    empty.free();
}

// Returns true if any E_LAMBDA was found in the subtree (replaces body_creates_closure)
// --- scan_lambdas_with_scope helpers ---

fn bool Compiler.scan_lambdas_lambda(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    List{SymbolId} own_bound;
    // Add all params to bound set for free-var scanning
    if (expr.lambda.param_count > 1) {
        for (usz pi = 0; pi < expr.lambda.param_count; pi++) {
            own_bound.push(expr.lambda.params[pi]);
        }
    } else if ((uint)expr.lambda.param != 0xFFFFFFFF) {
        own_bound.push(expr.lambda.param);
    }
    // For variadic lambdas, add all fixed params + rest param to bound set
    if (expr.lambda.has_rest) {
        for (usz pi = 0; pi < expr.lambda.param_count; pi++) {
            own_bound.push(expr.lambda.params[pi]);
        }
        if ((uint)expr.lambda.rest_param != 0) {
            own_bound.push(expr.lambda.rest_param);
        }
    }
    List{SymbolId} free;
    self.find_free_vars(expr.lambda.body, &own_bound, &free, enclosing_bound);

    LambdaDef def;
    def.id = self.lambda_counter++;
    def.param = expr.lambda.param;
    def.body = expr.lambda.body;
    def.capture_count = 0;
    def.capture_capacity = LAMBDA_CAPTURE_INITIAL;
    def.captures = (SymbolId*)mem::malloc(SymbolId.sizeof * def.capture_capacity);
    // Store variadic info
    def.has_rest = expr.lambda.has_rest;
    def.rest_param = expr.lambda.rest_param;
    def.param_count = expr.lambda.param_count;
    def.param_capacity = expr.lambda.param_count < LAMBDA_CAPTURE_INITIAL ? LAMBDA_CAPTURE_INITIAL : expr.lambda.param_count;
    def.params = (SymbolId*)mem::malloc(SymbolId.sizeof * def.param_capacity);
    for (usz pi = 0; pi < expr.lambda.param_count; pi++) {
        def.params[pi] = expr.lambda.params[pi];
    }

    foreach (f : free) {
        if (def.capture_count >= def.capture_capacity) {
            usz new_cap = def.capture_capacity * 2;
            SymbolId* new_caps = (SymbolId*)mem::malloc(SymbolId.sizeof * new_cap);
            for (usz ci = 0; ci < def.capture_count; ci++) new_caps[ci] = def.captures[ci];
            mem::free(def.captures);
            def.captures = new_caps;
            def.capture_capacity = new_cap;
        }
        def.captures[def.capture_count++] = f;
    }

    // Recursively scan body — the return value tells us if body has nested lambdas
    List{SymbolId} new_enclosing;
    foreach (b : *enclosing_bound) {
        new_enclosing.push(b);
    }
    // Add all params to enclosing scope for nested lambda free var analysis
    if (expr.lambda.param_count > 1) {
        for (usz pi = 0; pi < expr.lambda.param_count; pi++) {
            new_enclosing.push(expr.lambda.params[pi]);
        }
    } else if ((uint)expr.lambda.param != 0xFFFFFFFF) {
        new_enclosing.push(expr.lambda.param);
    }
    if (expr.lambda.has_rest && (uint)expr.lambda.rest_param != 0) {
        new_enclosing.push(expr.lambda.rest_param);
    }
    bool body_has_lambda = self.scan_lambdas_with_scope(expr.lambda.body, &new_enclosing);

    // Set creates_closure from recursive scan (replaces body_creates_closure)
    def.creates_closure = body_has_lambda;
    if (def.id < 256) {
        self.lambda_creates_closure[def.id] = body_has_lambda;
    }

    self.lambda_defs.push(def);

    new_enclosing.free();
    own_bound.free();
    free.free();
    return true;  // This node IS a lambda
}

fn bool Compiler.scan_lambdas_let(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    List{SymbolId} let_scope;
    foreach (b : *enclosing_bound) {
        let_scope.push(b);
    }
    let_scope.push(expr.let_expr.name);
    bool let_found;
    if (expr.let_expr.is_recursive) {
        let_found = self.scan_lambdas_with_scope(expr.let_expr.init, &let_scope);
    } else {
        let_found = self.scan_lambdas_with_scope(expr.let_expr.init, enclosing_bound);
    }
    bool let_body = self.scan_lambdas_with_scope(expr.let_expr.body, &let_scope);
    let_scope.free();
    return let_found || let_body;
}

fn bool Compiler.scan_lambdas_call(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    bool c_found = self.scan_lambdas_with_scope(expr.call.func, enclosing_bound);
    for (usz i = 0; i < expr.call.arg_count; i++) {
        c_found |= self.scan_lambdas_with_scope(expr.call.args[i], enclosing_bound);
    }
    return c_found;
}

fn bool Compiler.scan_lambdas_reset(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    // Wrap body as synthetic lambda: (lambda (_eff_dummy) body)
    SymbolId dummy_param = self.interp.symbols.intern("_eff_dummy");
    Expr* orig_body = expr.reset.body;
    Expr* wrapper = self.interp.alloc_expr();
    wrapper.tag = E_LAMBDA;
    wrapper.lambda = (ExprLambda*)mem::malloc(ExprLambda.sizeof);
    wrapper.loc_line = expr.loc_line;
    wrapper.loc_column = expr.loc_column;
    wrapper.lambda.param = dummy_param;
    wrapper.lambda.param_count = 1;
    wrapper.lambda.params = (SymbolId*)mem::malloc(SymbolId.sizeof * 1);
    wrapper.lambda.params[0] = dummy_param;
    wrapper.lambda.param_annotations = null;
    wrapper.lambda.has_rest = false;
    wrapper.lambda.rest_param = 0;
    wrapper.lambda.has_typed_params = false;
    wrapper.lambda.body = orig_body;
    expr.reset.body = wrapper;
    return self.scan_lambdas_with_scope(wrapper, enclosing_bound);
}

fn bool Compiler.scan_lambdas_shift(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    // Wrap body as synthetic lambda: (lambda (k_name) body)
    Expr* orig_body = expr.shift.body;
    Expr* wrapper = self.interp.alloc_expr();
    wrapper.tag = E_LAMBDA;
    wrapper.lambda = (ExprLambda*)mem::malloc(ExprLambda.sizeof);
    wrapper.loc_line = expr.loc_line;
    wrapper.loc_column = expr.loc_column;
    wrapper.lambda.param = expr.shift.k_name;
    wrapper.lambda.param_count = 1;
    wrapper.lambda.params = (SymbolId*)mem::malloc(SymbolId.sizeof * 1);
    wrapper.lambda.params[0] = expr.shift.k_name;
    wrapper.lambda.param_annotations = null;
    wrapper.lambda.has_rest = false;
    wrapper.lambda.rest_param = 0;
    wrapper.lambda.has_typed_params = false;
    wrapper.lambda.body = orig_body;
    expr.shift.body = wrapper;
    return self.scan_lambdas_with_scope(wrapper, enclosing_bound);
}

fn bool Compiler.scan_lambdas_handle(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    // Wrap body as synthetic lambda
    SymbolId dummy_param = self.interp.symbols.intern("_eff_dummy");
    Expr* orig_body = expr.handle.body;
    Expr* body_wrapper = self.interp.alloc_expr();
    body_wrapper.tag = E_LAMBDA;
    body_wrapper.lambda = (ExprLambda*)mem::malloc(ExprLambda.sizeof);
    body_wrapper.loc_line = expr.loc_line;
    body_wrapper.loc_column = expr.loc_column;
    body_wrapper.lambda.param = dummy_param;
    body_wrapper.lambda.param_count = 1;
    body_wrapper.lambda.params = (SymbolId*)mem::malloc(SymbolId.sizeof * 1);
    body_wrapper.lambda.params[0] = dummy_param;
    body_wrapper.lambda.param_annotations = null;
    body_wrapper.lambda.has_rest = false;
    body_wrapper.lambda.rest_param = 0;
    body_wrapper.lambda.has_typed_params = false;
    body_wrapper.lambda.body = orig_body;
    expr.handle.body = body_wrapper;
    bool h_found = self.scan_lambdas_with_scope(body_wrapper, enclosing_bound);

    // Wrap each handler clause as synthetic lambda: (lambda (_pair) (let (k (car _pair)) (let (arg (cdr _pair)) body)))
    SymbolId pair_param = self.interp.symbols.intern("_eff_pair");
    SymbolId sym_car = self.interp.symbols.intern("car");
    SymbolId sym_cdr = self.interp.symbols.intern("cdr");
    for (usz i = 0; i < expr.handle.clause_count; i++) {
        Expr* orig_handler = expr.handle.clauses[i].handler_body;
        SymbolId k_name = expr.handle.clauses[i].k_name;
        SymbolId arg_name = expr.handle.clauses[i].arg_name;

        // Build (cdr _pair) for arg extraction
        Expr* pair_ref2 = self.interp.alloc_expr();
        pair_ref2.tag = E_VAR;
        pair_ref2.var_expr.name = pair_param;
        Expr* cdr_fn = self.interp.alloc_expr();
        cdr_fn.tag = E_VAR;
        cdr_fn.var_expr.name = sym_cdr;
        Expr* cdr_call = self.interp.alloc_expr();
        cdr_call.tag = E_APP;
        cdr_call.app.func = cdr_fn;
        cdr_call.app.arg = pair_ref2;

        // Build: (let (arg_name (cdr _pair)) orig_handler)
        Expr* let_arg = self.interp.alloc_expr();
        let_arg.tag = E_LET;
        let_arg.let_expr.name = arg_name;
        let_arg.let_expr.init = cdr_call;
        let_arg.let_expr.body = orig_handler;
        let_arg.let_expr.is_recursive = false;

        // Build (car _pair) for k extraction
        Expr* pair_ref1 = self.interp.alloc_expr();
        pair_ref1.tag = E_VAR;
        pair_ref1.var_expr.name = pair_param;
        Expr* car_fn = self.interp.alloc_expr();
        car_fn.tag = E_VAR;
        car_fn.var_expr.name = sym_car;
        Expr* car_call = self.interp.alloc_expr();
        car_call.tag = E_APP;
        car_call.app.func = car_fn;
        car_call.app.arg = pair_ref1;

        // Build: (let (k_name (car _pair)) (let (arg_name ...) ...))
        Expr* let_k = self.interp.alloc_expr();
        let_k.tag = E_LET;
        let_k.let_expr.name = k_name;
        let_k.let_expr.init = car_call;
        let_k.let_expr.body = let_arg;
        let_k.let_expr.is_recursive = false;

        // Lambda wrapper: (lambda (_pair) (let (k ...) (let (arg ...) body)))
        Expr* handler_wrapper = self.interp.alloc_expr();
        handler_wrapper.tag = E_LAMBDA;
        handler_wrapper.lambda = (ExprLambda*)mem::malloc(ExprLambda.sizeof);
        handler_wrapper.loc_line = expr.loc_line;
        handler_wrapper.loc_column = expr.loc_column;
        handler_wrapper.lambda.param = pair_param;
        handler_wrapper.lambda.param_count = 1;
        handler_wrapper.lambda.params = (SymbolId*)mem::malloc(SymbolId.sizeof * 1);
        handler_wrapper.lambda.params[0] = pair_param;
        handler_wrapper.lambda.param_annotations = null;
        handler_wrapper.lambda.has_rest = false;
        handler_wrapper.lambda.rest_param = 0;
        handler_wrapper.lambda.has_typed_params = false;
        handler_wrapper.lambda.body = let_k;
        expr.handle.clauses[i].handler_body = handler_wrapper;
        h_found |= self.scan_lambdas_with_scope(handler_wrapper, enclosing_bound);
    }
    return h_found;
}

fn bool Compiler.scan_lambdas_with_scope(Compiler* self, Expr* expr, List{SymbolId}* enclosing_bound) {
    if (expr == null) return false;

    switch (expr.tag) {
        case E_LAMBDA:
            return self.scan_lambdas_lambda(expr, enclosing_bound);

        case E_APP:
            bool app_f = self.scan_lambdas_with_scope(expr.app.func, enclosing_bound);
            return self.scan_lambdas_with_scope(expr.app.arg, enclosing_bound) || app_f;

        case E_IF:
            bool if_t = self.scan_lambdas_with_scope(expr.if_expr.test, enclosing_bound);
            bool if_th = self.scan_lambdas_with_scope(expr.if_expr.then_branch, enclosing_bound);
            return self.scan_lambdas_with_scope(expr.if_expr.else_branch, enclosing_bound) || if_t || if_th;

        case E_LET:
            return self.scan_lambdas_let(expr, enclosing_bound);

        case E_DEFINE:
            return self.scan_lambdas_with_scope(expr.define.value, enclosing_bound);

        case E_AND:
            bool and_l = self.scan_lambdas_with_scope(expr.and_expr.left, enclosing_bound);
            return self.scan_lambdas_with_scope(expr.and_expr.right, enclosing_bound) || and_l;

        case E_OR:
            bool or_l = self.scan_lambdas_with_scope(expr.or_expr.left, enclosing_bound);
            return self.scan_lambdas_with_scope(expr.or_expr.right, enclosing_bound) || or_l;

        case E_MATCH:
            bool m_found = self.scan_lambdas_with_scope(expr.match.scrutinee, enclosing_bound);
            for (usz i = 0; i < expr.match.clause_count; i++) {
                m_found |= self.scan_lambdas_with_scope(expr.match.clauses[i].result, enclosing_bound);
            }
            return m_found;

        case E_CALL:
            return self.scan_lambdas_call(expr, enclosing_bound);

        case E_INDEX:
            bool idx_c = self.scan_lambdas_with_scope(expr.index.collection, enclosing_bound);
            return self.scan_lambdas_with_scope(expr.index.index, enclosing_bound) || idx_c;

        case E_PATH:
            return false;

        case E_RESET:
            return self.scan_lambdas_reset(expr, enclosing_bound);

        case E_SHIFT:
            return self.scan_lambdas_shift(expr, enclosing_bound);

        case E_PERFORM:
            return self.scan_lambdas_with_scope(expr.perform.arg, enclosing_bound);

        case E_RESOLVE:
            return self.scan_lambdas_with_scope(expr.resolve.value, enclosing_bound);

        case E_HANDLE:
            return self.scan_lambdas_handle(expr, enclosing_bound);

        case E_BEGIN:
            bool b_found = false;
            for (usz i = 0; i < expr.begin.expr_count; i++) {
                b_found |= self.scan_lambdas_with_scope(expr.begin.exprs[i], enclosing_bound);
            }
            return b_found;

        case E_SET:
            return self.scan_lambdas_with_scope(expr.set_expr.value, enclosing_bound);

        case E_QUASIQUOTE:
            return self.scan_lambdas_with_scope(expr.quasiquote.body, enclosing_bound);

        case E_UNQUOTE:
            return self.scan_lambdas_with_scope(expr.unquote.body, enclosing_bound);

        case E_UNQUOTE_SPLICING:
            return self.scan_lambdas_with_scope(expr.unquote_splicing.body, enclosing_bound);

        case E_DEFMACRO:
            return false;

        case E_MODULE:
            bool mod_found = false;
            for (usz i = 0; i < expr.module_expr.body_count; i++) {
                List{SymbolId} mod_empty;
                mod_found |= self.scan_lambdas_with_scope(expr.module_expr.body[i], &mod_empty);
                mod_empty.free();
            }
            return mod_found;

        case E_IMPORT:
        case E_EXPORT_FROM:
            return false;

        default:
            return false;
    }
}

