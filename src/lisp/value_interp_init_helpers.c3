module lisp;

fn void interp_init_core_symbols(Interp* self) {
    self.sym_lambda = self.symbols.intern("lambda");
    self.sym_if = self.symbols.intern("if");
    self.sym_let = self.symbols.intern("let");
    self.sym_define = self.symbols.intern("define");
    self.sym_quote = self.symbols.intern("quote");
    self.sym_checkpoint = self.symbols.intern("checkpoint");
    self.sym_capture = self.symbols.intern("capture");
    self.sym_perform = self.symbols.intern("perform");
    self.sym_signal = self.symbols.intern("signal");
    self.sym_handle = self.symbols.intern("handle");
    self.sym_resolve = self.symbols.intern("resolve");
    self.sym_with_continuation = self.symbols.intern("with-continuation");
    self.sym__k = self.symbols.intern("__k");
    self.sym_match = self.symbols.intern("match");
    self.sym_and = self.symbols.intern("and");
    self.sym_or = self.symbols.intern("or");
    self.sym_true = self.symbols.intern("true");
    self.sym_false = self.symbols.intern("false");
    self.sym_block = self.symbols.intern("block");
    self.sym_set = self.symbols.intern("set!");
    self.sym_quasiquote = self.symbols.intern("quasiquote");
    self.sym_unquote = self.symbols.intern("unquote");
    self.sym_unquote_splicing = self.symbols.intern("unquote-splicing");
    self.sym_defmacro = self.symbols.intern("defmacro");
    self.sym_dotdot = self.symbols.intern("..");
    self.sym_macro = self.symbols.intern("macro");
}

fn void interp_init_type_symbols(Interp* self) {
    self.sym_type = self.symbols.intern("type");
    self.sym_struct = self.symbols.intern("struct");
    self.sym_abstract = self.symbols.intern("abstract");
    self.sym_union = self.symbols.intern("union");
    self.sym_alias = self.symbols.intern("alias");
    self.sym_effect = self.symbols.intern("effect");

    self.sym_ffi = self.symbols.intern("ffi");
    self.sym_lib = self.symbols.intern("lib");
    self.sym_Pointer = self.symbols.intern("Pointer");
    self.sym_Ptr = self.symbols.intern("Ptr");
    self.sym_Void = self.symbols.intern("Void");

    self.sym_Integer = self.symbols.intern("Integer");
    self.sym_Int = self.symbols.intern("Int");
    self.sym_Double = self.symbols.intern("Double");
    self.sym_String = self.symbols.intern("String");
    self.sym_Symbol = self.symbols.intern("Symbol");
    self.sym_List = self.symbols.intern("List");
    self.sym_Boolean = self.symbols.intern("Boolean");
    self.sym_Bool = self.symbols.intern("Bool");
    self.sym_Nil = self.symbols.intern("Nil");
    self.sym_Closure = self.symbols.intern("Closure");
    self.sym_Array = self.symbols.intern("Array");
    self.sym_Dictionary = self.symbols.intern("Dictionary");
    self.sym_Dict = self.symbols.intern("Dict");
    self.sym_Set = self.symbols.intern("Set");
    self.sym_Any = self.symbols.intern("Any");
    self.sym_Value = self.symbols.intern("Value");
    self.sym_Number = self.symbols.intern("Number");
    self.sym_Collection = self.symbols.intern("Collection");
    self.sym_Iterator = self.symbols.intern("Iterator");
    self.sym_TimePoint = self.symbols.intern("TimePoint");
    self.sym_Coroutine = self.symbols.intern("Coroutine");
}

fn void interp_init_misc_symbols(Interp* self) {
    self.sym_raise = self.symbols.intern("raise");
    self.sym_car = self.symbols.intern("car");
    self.sym_cdr = self.symbols.intern("cdr");
    self.sym_placeholder = self.symbols.intern("__placeholder");
    self.sym_pipe = self.symbols.intern("|>");
    self.sym_question = self.symbols.intern("?");
}

fn void interp_init_module_state(Interp* self) {
    self.sym_module = self.symbols.intern("module");
    self.sym_import = self.symbols.intern("import");
    self.sym_export = self.symbols.intern("export");
    self.sym_as = self.symbols.intern("as");
    self.sym_all = self.symbols.intern("all");
    self.sym_export_from = self.symbols.intern("export-from");
    self.module_count = 0;
    self.module_capacity = MODULE_INITIAL_CAPACITY;
    self.modules = (Module*)mem::malloc(Module.sizeof * self.module_capacity);
    assert(self.modules != null, "interp module table alloc failed");
    self.module_hash_capacity = self.module_capacity * 2;
    self.module_hash_index = (usz*)mem::malloc(usz.sizeof * self.module_hash_capacity);
    assert(self.module_hash_index != null, "interp module hash alloc failed");
    for (usz i = 0; i < self.module_hash_capacity; i++) self.module_hash_index[i] = usz.max;
}

fn void interp_init_runtime_flags(Interp* self) {
    self.flags.jit_enabled = false;
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
    self.macro_table = (MacroDef*)mem::malloc(MacroDef.sizeof * self.macro_capacity);
    assert(self.macro_table != null, "interp macro table alloc failed");
    self.gensym_counter = 0;
    self.macro_hash_capacity = self.macro_capacity * 2;
    self.macro_hash_index = (usz*)mem::malloc(usz.sizeof * self.macro_hash_capacity);
    assert(self.macro_hash_index != null, "interp macro hash alloc failed");
    for (usz i = 0; i < self.macro_hash_capacity; i++) self.macro_hash_index[i] = usz.max;
}
