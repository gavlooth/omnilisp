module lisp;

fn bool interp_init_checked_mul_usz(usz a, usz b, usz* out) @inline {
    if (b != 0 && a > usz.max / b) return false;
    *out = a * b;
    return true;
}

fn bool interp_init_symbol(Interp* self, SymbolId* out, char[] name) @inline {
    if (self == null || out == null) return false;
    SymbolId sid = self.symbols.intern(name);
    if (sid == INVALID_SYMBOL_ID) return false;
    *out = sid;
    return true;
}

fn bool interp_init_core_symbols(Interp* self) {
    return interp_init_symbol(self, &self.sym_lambda, "lambda") &&
        interp_init_symbol(self, &self.sym_if, "if") &&
        interp_init_symbol(self, &self.sym_let, "let") &&
        interp_init_symbol(self, &self.sym_define, "define") &&
        interp_init_symbol(self, &self.sym_quote, "quote") &&
        interp_init_symbol(self, &self.sym_checkpoint, "checkpoint") &&
        interp_init_symbol(self, &self.sym_capture, "capture") &&
        interp_init_symbol(self, &self.sym_perform, "perform") &&
        interp_init_symbol(self, &self.sym_signal, "signal") &&
        interp_init_symbol(self, &self.sym_handle, "handle") &&
        interp_init_symbol(self, &self.sym_resolve, "resolve") &&
        interp_init_symbol(self, &self.sym_with_continuation, "with-continuation") &&
        interp_init_symbol(self, &self.sym__k, "__k") &&
        interp_init_symbol(self, &self.sym_match, "match") &&
        interp_init_symbol(self, &self.sym_and, "and") &&
        interp_init_symbol(self, &self.sym_or, "or") &&
        interp_init_symbol(self, &self.sym_true, "true") &&
        interp_init_symbol(self, &self.sym_false, "false") &&
        interp_init_symbol(self, &self.sym_block, "block") &&
        interp_init_symbol(self, &self.sym_set, "set!") &&
        interp_init_symbol(self, &self.sym_quasiquote, "quasiquote") &&
        interp_init_symbol(self, &self.sym_unquote, "unquote") &&
        interp_init_symbol(self, &self.sym_unquote_splicing, "unquote-splicing") &&
        interp_init_symbol(self, &self.sym_defmacro, "defmacro") &&
        interp_init_symbol(self, &self.sym_dotdot, "..") &&
        interp_init_symbol(self, &self.sym_macro, "macro");
}

fn bool interp_init_type_symbols(Interp* self) {
    return interp_init_symbol(self, &self.sym_type, "type") &&
        interp_init_symbol(self, &self.sym_struct, "struct") &&
        interp_init_symbol(self, &self.sym_abstract, "abstract") &&
        interp_init_symbol(self, &self.sym_union, "union") &&
        interp_init_symbol(self, &self.sym_alias, "alias") &&
        interp_init_symbol(self, &self.sym_effect, "effect") &&
        interp_init_symbol(self, &self.sym_ffi, "ffi") &&
        interp_init_symbol(self, &self.sym_lib, "lib") &&
        interp_init_symbol(self, &self.sym_ForeignHandle, "ForeignHandle") &&
        interp_init_symbol(self, &self.sym_Void, "Void") &&
        interp_init_symbol(self, &self.sym_Integer, "Integer") &&
        interp_init_symbol(self, &self.sym_BigInteger, "BigInteger") &&
        interp_init_symbol(self, &self.sym_BigFloat, "BigFloat") &&
        interp_init_symbol(self, &self.sym_BigComplex, "BigComplex") &&
        interp_init_symbol(self, &self.sym_Complex128, "Complex128") &&
        interp_init_symbol(self, &self.sym_Complex64, "Complex64") &&
        interp_init_symbol(self, &self.sym_Float, "Float") &&
        interp_init_symbol(self, &self.sym_Float32, "Float32") &&
        interp_init_symbol(self, &self.sym_Double, "Float64") &&
        interp_init_symbol(self, &self.sym_String, "String") &&
        interp_init_symbol(self, &self.sym_Symbol, "Symbol") &&
        interp_init_symbol(self, &self.sym_List, "List") &&
        interp_init_symbol(self, &self.sym_Boolean, "Boolean") &&
        interp_init_symbol(self, &self.sym_Nil, "Nil") &&
        interp_init_symbol(self, &self.sym_Closure, "Closure") &&
        interp_init_symbol(self, &self.sym_Array, "Array") &&
        interp_init_symbol(self, &self.sym_Dictionary, "Dictionary") &&
        interp_init_symbol(self, &self.sym_Set, "Set") &&
        interp_init_symbol(self, &self.sym_Any, "Any") &&
        interp_init_symbol(self, &self.sym_Value, "Value") &&
        interp_init_symbol(self, &self.sym_Number, "Number") &&
        interp_init_symbol(self, &self.sym_Collection, "Collection") &&
        interp_init_symbol(self, &self.sym_Iterator, "Iterator") &&
        interp_init_symbol(self, &self.sym_TimePoint, "TimePoint") &&
        interp_init_symbol(self, &self.sym_Coroutine, "Coroutine") &&
        interp_init_symbol(self, &self.sym_Tensor, "Tensor");
}

fn bool interp_init_misc_symbols(Interp* self) {
    return interp_init_symbol(self, &self.sym_raise, "raise") &&
        interp_init_symbol(self, &self.sym_car, "car") &&
        interp_init_symbol(self, &self.sym_cdr, "cdr") &&
        interp_init_symbol(self, &self.sym_placeholder, "__placeholder") &&
        interp_init_symbol(self, &self.sym_pipe, "|>") &&
        interp_init_symbol(self, &self.sym_question, "?");
}

fn void interp_init_module_state(Interp* self) {
    assert(interp_init_symbol(self, &self.sym_module, "module"), "interp module symbol intern failed");
    assert(interp_init_symbol(self, &self.sym_import, "import"), "interp module symbol intern failed");
    assert(interp_init_symbol(self, &self.sym_export, "export"), "interp module symbol intern failed");
    assert(interp_init_symbol(self, &self.sym_as, "as"), "interp module symbol intern failed");
    assert(interp_init_symbol(self, &self.sym_all, "all"), "interp module symbol intern failed");
    assert(interp_init_symbol(self, &self.sym_export_from, "export-from"), "interp module symbol intern failed");
    self.module_count = 0;
    self.module_capacity = MODULE_INITIAL_CAPACITY;
    usz module_bytes = 0;
    assert(interp_init_checked_mul_usz(Module.sizeof, self.module_capacity, &module_bytes), "interp module table size overflow");
    self.modules = (Module*)mem::malloc(module_bytes);
    assert(self.modules != null, "interp module table alloc failed");
    usz module_hash_capacity = 0;
    assert(interp_init_checked_mul_usz(self.module_capacity, (usz)2, &module_hash_capacity), "interp module hash capacity overflow");
    self.module_hash_capacity = module_hash_capacity;
    usz module_hash_bytes = 0;
    assert(interp_init_checked_mul_usz(usz.sizeof, self.module_hash_capacity, &module_hash_bytes), "interp module hash size overflow");
    self.module_hash_index = (usz*)mem::malloc(module_hash_bytes);
    assert(self.module_hash_index != null, "interp module hash alloc failed");
    for (usz i = 0; i < self.module_hash_capacity; i++) self.module_hash_index[i] = usz.max;
}

fn void interp_init_runtime_flags(Interp* self) {
    self.flags.jit_enabled = false;
    self.flags.console_to_stderr = false;
    self.console_capture_active = false;
    self.console_capture_buf = null;
    self.console_capture_len = 0;
    self.console_capture_cap = 0;
    self.input_state = null;
    self.sigint_requested = 0;
    self.interrupt_flag = null;
    self.signal_handles_head = null;
    self.jit_env = null;
    self.jit_tco_expr = null;
    self.jit_tco_env = null;
    self.tco_recycle_scope = null;
    self.tco_scope_defer_slot = 0;
    self.tco_scope_defer_active = false;
    self.releasing_scope = null;
    self.escape_env_mode = false;
    self.promotion_epoch_counter = 0;
    self.active_promotion_ctx = null;
    self.flags.jit_tco_bounce = false;

    self.eval_depth = 0;
}

fn void interp_init_macro_state(Interp* self) {
    self.macro_count = 0;
    self.macro_capacity = MACRO_INITIAL_CAPACITY;
    usz macro_bytes = 0;
    assert(interp_init_checked_mul_usz(MacroDef.sizeof, self.macro_capacity, &macro_bytes), "interp macro table size overflow");
    self.macro_table = (MacroDef*)mem::malloc(macro_bytes);
    assert(self.macro_table != null, "interp macro table alloc failed");
    self.gensym_counter = 0;
    usz macro_hash_capacity = 0;
    assert(interp_init_checked_mul_usz(self.macro_capacity, (usz)2, &macro_hash_capacity), "interp macro hash capacity overflow");
    self.macro_hash_capacity = macro_hash_capacity;
    usz macro_hash_bytes = 0;
    assert(interp_init_checked_mul_usz(usz.sizeof, self.macro_hash_capacity, &macro_hash_bytes), "interp macro hash size overflow");
    self.macro_hash_index = (usz*)mem::malloc(macro_hash_bytes);
    assert(self.macro_hash_index != null, "interp macro hash alloc failed");
    for (usz i = 0; i < self.macro_hash_capacity; i++) self.macro_hash_index[i] = usz.max;
}
