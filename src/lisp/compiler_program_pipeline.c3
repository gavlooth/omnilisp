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
