module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 5: MAIN COMPILATION (program pipeline)
// =============================================================================
/**
 * Compile a complete program to C3 source code.
 * Prepends stdlib definitions so HOFs are available.
 */
fn bool Compiler.parse_program_exprs(Compiler* self, char[] full, List{Expr*}* exprs) {
    Lexer lex;
    lex.init(full);
    defer lex.destroy();
    Parser p;
    p.init(&lex, self.interp);

    while (!lex.at_end() && !p.has_error) {
        Expr* e = p.parse_expr();
        if (e != null) {
            exprs.push(e);
        }
    }

    if (p.has_error) {
        io::printfn("Syntax Error at line %d, column %d: %s", (int)p.error_line, (int)p.error_col, (ZString)&p.error_msg[0]);
        return false;
    }
    return true;
}

fn void Compiler.collect_top_level_defines(Compiler* self, List{Expr*}* exprs) {
    foreach (expr : *exprs) {
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
}

fn isz find_last_non_define(List{Expr*}* exprs) {
    isz last_non_define = -1;
    for (usz i = 0; i < exprs.len(); i++) {
        if ((*exprs)[i].tag != E_DEFINE) {
            last_non_define = (isz)i;
        }
    }
    return last_non_define;
}

fn void Compiler.emit_top_level_exprs(Compiler* self, List{Expr*}* exprs) {
    isz last_non_define = -1;
    if (self.print_last) {
        last_non_define = find_last_non_define(exprs);
    }

    bool print_all_active = false;
    for (usz i = 0; i < exprs.len(); i++) {
        if ((*exprs)[i].tag == E_DEFINE) {
            char[] dname = self.interp.symbols.get_name((*exprs)[i].define.name);
            if (self.str_eq(dname, "__e2e_start__")) {
                print_all_active = true;
                usz init_r = self.compile_to_temp((*exprs)[i].define.value);
                self.emit_indent();
                self.emit_symbol_name((*exprs)[i].define.name);
                self.emit(" = ");
                self.emit_temp_ref(init_r);
                self.emit(";\n");
                continue;
            }
        }

        bool should_print = (self.print_all && print_all_active)
            || (self.print_last && (isz)i == last_non_define);

        if (should_print) {
            usz r = self.compile_to_temp((*exprs)[i]);
            self.emit_indent();
            self.emit("aot::print_value(");
            self.emit_temp_ref(r);
            self.emit(");\n");
        } else if ((*exprs)[i].tag == E_DEFINE) {
            usz init_r = self.compile_to_temp((*exprs)[i].define.value);
            self.emit_indent();
            self.emit_symbol_name((*exprs)[i].define.name);
            self.emit(" = ");
            self.emit_temp_ref(init_r);
            self.emit(";\n");
        } else {
            self.compile_to_temp((*exprs)[i]);
        }
    }
}

fn void Compiler.emit_code_with_prim_inits(Compiler* self, char[] code_str, usz prim_init_pos) {
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
        return;
    }
    self.emit(code_str);
}

fn char[] Compiler.build_program_source(Compiler* self, char[] source, char** full_buf_out) {
    usz total = STDLIB_PRELUDE.len + 1 + source.len;
    char* full_buf = (char*)mem::malloc(total);
    mem::copy(full_buf, STDLIB_PRELUDE.ptr, STDLIB_PRELUDE.len);
    full_buf[STDLIB_PRELUDE.len] = ' ';
    mem::copy(&full_buf[STDLIB_PRELUDE.len + 1], source.ptr, source.len);
    *full_buf_out = full_buf;
    return full_buf[:total];
}

fn void Compiler.prepare_program_analysis(Compiler* self, List{Expr*}* exprs) {
    self.collect_top_level_defines(exprs);
    foreach (expr : *exprs) {
        self.prescan_mutable_captures(expr);
    }
    foreach (expr : *exprs) {
        self.scan_lambdas(expr);
    }
}

fn void Compiler.emit_compiled_main_body(Compiler* self, List{Expr*}* exprs, usz* prim_init_pos) {
    self.emit_lambda_definitions();
    self.emit("fn int main() {\n");
    self.indent++;
    self.emit_line("aot::aot_init();");
    self.emit_newline();

    if (self.defined_globals.len() > 0) {
        self.emit_line("// Initialize global variables to nil");
        foreach (name : self.defined_globals) {
            self.emit_indent();
            self.emit_symbol_name(name);
            self.emit(" = aot::make_nil();\n");
        }
        self.emit_newline();
    }

    *prim_init_pos = self.output.str_view().len;
    self.emit_newline();
    self.declared_vars.free();
    self.emit_top_level_exprs(exprs);
    self.emit_main_end();
}

fn char[] Compiler.compile_program(Compiler* self, char[] source) {
    char* full_buf = null;
    char[] full = self.build_program_source(source, &full_buf);
    defer mem::free(full_buf);

    List{Expr*} exprs;
    defer exprs.free();
    if (!self.parse_program_exprs(full, &exprs)) return "";
    self.prepare_program_analysis(&exprs);

    // D2: Two-pass compilation — compile all code into a temp buffer first
    // so we discover all referenced prims before emitting global declarations.
    DString code_buf;
    code_buf.init(mem, 8192);
    DString orig_output = self.output;
    self.output = code_buf;

    usz prim_init_pos = self.output.str_view().len;
    self.emit_compiled_main_body(&exprs, &prim_init_pos);

    // Swap back to original output — now we know all referenced prims
    code_buf = self.output;
    self.output = orig_output;

    // Assemble final output: prelude + globals + compiled code (with prim init inserted)
    self.emit_prelude();
    self.emit_global_declarations();

    char[] code_str = code_buf.str_view();
    self.emit_code_with_prim_inits(code_str, prim_init_pos);

    code_buf.free();

    return self.get_output();
}
