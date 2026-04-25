module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 2: COMPILER INITIALIZATION
// =============================================================================

fn void Compiler.init(Compiler* self, Interp* interp) {
    self.interp = interp;
    // Lists auto-initialize when declared
    self.indent = 0;
    self.temp_counter = 0;
    self.lambda_counter = 0;
    self.has_error = false;
    self.error_message = "";
    self.output.init(mem, 8192);
    init_prim_hash(interp);
    if (g_prim_hash_init_failed) {
        self.set_compile_error("compiler: failed to initialize primitive hash table");
        return;
    }
    self.compiled_module_count = 0;
    self.compiled_module_capacity = 16;
    usz registry_bytes = 0;
    bool registry_size_ok = !g_compiler_force_compiled_modules_init_size_overflow
        && compiler_checked_mul_usz(CompiledModule.sizeof, self.compiled_module_capacity, &registry_bytes);
    if (!registry_size_ok) {
        self.compiled_module_capacity = 0;
        self.set_compile_error("compiler: compiled module registry allocation size overflow");
        return;
    }
    self.compiled_modules = g_compiler_force_compiled_modules_init_alloc_fail
        ? null
        : (CompiledModule*)mem::malloc(registry_bytes);
    if (self.compiled_modules == null) {
        self.compiled_module_capacity = 0;
        self.set_compile_error("compiler: out of memory while initializing compiled module registry");
    }
}

fn void Compiler.free(Compiler* self) {
    self.output.free();
    self.current_captures.free();
    self.defined_globals.free();
    foreach (&def : self.lambda_defs) {
        self.free_lambda_def(def);
    }
    self.lambda_defs.free();
    self.mutable_captures.free();
    self.declared_vars.free();
    self.referenced_prims.free();
    self.module_open_exports.free();
    self.module_open_aliases.free();
    self.module_private_symbols.free();
    self.module_private_backings.free();
    for (usz i = 0; i < self.compiled_module_count; i++) {
        if (self.compiled_modules[i].exports != null) {
            mem::free(self.compiled_modules[i].exports);
        }
        if (self.compiled_modules[i].locals != null) {
            mem::free(self.compiled_modules[i].locals);
        }
        if (self.compiled_modules[i].local_backings != null) {
            mem::free(self.compiled_modules[i].local_backings);
        }
    }
    if (self.compiled_modules != null) {
        mem::free(self.compiled_modules);
        self.compiled_modules = null;
    }
}

fn bool Compiler.is_declared(Compiler* self, SymbolId sym) {
    foreach (v : self.declared_vars) {
        if (v == sym) return true;
    }
    return false;
}

fn bool Compiler.is_global(Compiler* self, SymbolId sym) {
    foreach (g : self.defined_globals) {
        if (g == sym) return true;
    }
    return false;
}

fn void Compiler.set_compile_error(Compiler* self, char[] msg) {
    if (self.has_error) return;
    self.has_error = true;
    self.error_message = msg;
}

fn void Compiler.set_compile_error_formatted(Compiler* self, char[] prefix, usz line, usz column, char[] detail) {
    if (self.has_error) return;
    self.has_error = true;
    if (try msg = io::bprintf(&self.error_storage, "%s at line %d, column %d: %s", (ZString)prefix, (int)line, (int)column, (ZString)detail)) {
        self.error_message = msg;
        return;
    }
    self.error_message = prefix;
}

fn void Compiler.set_unsupported_expr_error(Compiler* self, ExprTag tag) {
    switch (tag) {
        case E_FFI_LIB:
            self.set_compile_error("compiler: [ffi lib] is not supported by AOT lowering");
            return;
        case E_FFI_FN:
            self.set_compile_error("compiler: [ffi λ] is not supported by AOT lowering");
            return;
        default:
            self.set_compile_error("compiler: encountered an unsupported expression during AOT lowering");
            return;
    }
}


fn void Compiler.mark_declared(Compiler* self, SymbolId sym) {
    if (!self.is_declared(sym)) {
        self.declared_vars.push(sym);
    }
}
