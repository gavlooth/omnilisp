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
    self.output.init(mem, 8192);
    init_prim_hash(interp);
    self.compiled_module_count = 0;
    self.compiled_module_capacity = 16;
    self.compiled_modules = (CompiledModule*)mem::malloc(CompiledModule.sizeof * 16);
}

fn void Compiler.free(Compiler* self) {
    self.output.free();
    self.current_captures.free();
    self.defined_globals.free();
    self.lambda_defs.free();
    self.mutable_captures.free();
    self.declared_vars.free();
    self.referenced_prims.free();
    for (usz i = 0; i < self.compiled_module_count; i++) {
        if (self.compiled_modules[i].exports != null) {
            mem::free(self.compiled_modules[i].exports);
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


fn void Compiler.mark_declared(Compiler* self, SymbolId sym) {
    if (!self.is_declared(sym)) {
        self.declared_vars.push(sym);
    }
}

