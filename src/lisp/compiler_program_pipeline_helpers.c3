module lisp;

import std::collections::list;
import main;

struct CompilerMacroStateSnapshot {
    MacroDef* macro_table;
    usz* macro_hash_index;
    usz macro_count;
    usz macro_capacity;
    usz macro_hash_capacity;
    bool active;
}

fn usz compiler_source_line_offset() @inline {
    usz lines = 1;
    foreach (c : STDLIB_PRELUDE) {
        if (c == '\n') lines++;
    }
    return lines;
}

fn bool Compiler.parse_program_exprs(Compiler* self, char[] full, List{Expr*}* exprs) {
    Lexer lex;
    lex.init(full);
    defer lex.destroy();

    if (lex.current.type == T_ERROR && lex.current.text == null && lex.current.text_cap == 0) {
        self.set_compile_error_formatted(
            "compiler syntax error",
            1,
            1,
            "out of memory while initializing lexer token buffer"
        );
        return false;
    }

    Parser p;
    p.init(&lex, self.interp);

    while (!lex.at_end() && !p.has_error) {
        if (!p.ensure_program_expr_limit(exprs.len())) break;
        Expr* e = p.parse_expr();
        if (e != null && !p.has_error) {
            exprs.push(e);
        }
    }

    if (p.has_error) {
        exprs.free();
        usz source_line = p.error_line;
        usz line_offset = compiler_source_line_offset();
        if (source_line > line_offset) {
            source_line -= line_offset;
        } else {
            source_line = 1;
        }
        self.set_compile_error_formatted("compiler syntax error", source_line, p.error_col, p.error_msg[:p.error_msg_len]);
        return false;
    }
    return true;
}

fn bool compiler_clone_macro_def(MacroDef* out, MacroDef* src) {
    *out = {};
    out.name = src.name;

    if (src.clause_count > 0) {
        usz clause_bytes = 0;
        if (!compiler_checked_mul_usz(MacroClause.sizeof, src.clause_count, &clause_bytes)) return false;
        out.clauses = (MacroClause*)mem::malloc(clause_bytes);
        if (out.clauses == null) return false;
        for (usz i = 0; i < src.clause_count; i++) out.clauses[i] = src.clauses[i];
        out.clause_count = src.clause_count;
    }

    if (src.captured_count > 0) {
        usz captured_bytes = 0;
        if (!compiler_checked_mul_usz(CapturedBinding.sizeof, src.captured_count, &captured_bytes)) {
            macro_def_destroy(out);
            return false;
        }
        out.captured_bindings = (CapturedBinding*)mem::malloc(captured_bytes);
        if (out.captured_bindings == null) {
            macro_def_destroy(out);
            return false;
        }
        for (usz i = 0; i < src.captured_count; i++) out.captured_bindings[i] = src.captured_bindings[i];
        out.captured_count = src.captured_count;
    }
    return true;
}

fn void compiler_rebuild_macro_hash(Interp* interp) {
    if (interp == null || interp.macro_hash_index == null || interp.macro_hash_capacity == 0) return;
    for (usz i = 0; i < interp.macro_hash_capacity; i++) interp.macro_hash_index[i] = usz.max;
    for (usz i = 0; i < interp.macro_count; i++) {
        usz hash_slot = (usz)interp.macro_table[i].name % interp.macro_hash_capacity;
        for (usz probe = 0; probe < interp.macro_hash_capacity; probe++) {
            usz slot = (hash_slot + probe) % interp.macro_hash_capacity;
            if (interp.macro_hash_index[slot] == usz.max) {
                interp.macro_hash_index[slot] = i;
                break;
            }
        }
    }
}

fn void Compiler.leave_macro_expansion_sandbox(Compiler* self, CompilerMacroStateSnapshot* saved) {
    if (saved == null || !saved.active || self == null || self.interp == null) return;

    Interp* interp = self.interp;
    if (interp.macro_table != null) {
        for (usz i = 0; i < interp.macro_count; i++) macro_def_destroy(&interp.macro_table[i]);
        mem::free(interp.macro_table);
    }
    if (interp.macro_hash_index != null) mem::free(interp.macro_hash_index);

    interp.macro_table = saved.macro_table;
    interp.macro_hash_index = saved.macro_hash_index;
    interp.macro_count = saved.macro_count;
    interp.macro_capacity = saved.macro_capacity;
    interp.macro_hash_capacity = saved.macro_hash_capacity;
    saved.active = false;
}

fn bool Compiler.enter_macro_expansion_sandbox(Compiler* self, CompilerMacroStateSnapshot* saved) {
    if (self == null || self.interp == null || saved == null) return false;
    *saved = {};

    Interp* interp = self.interp;
    saved.macro_table = interp.macro_table;
    saved.macro_hash_index = interp.macro_hash_index;
    saved.macro_count = interp.macro_count;
    saved.macro_capacity = interp.macro_capacity;
    saved.macro_hash_capacity = interp.macro_hash_capacity;
    saved.active = true;

    interp.macro_table = null;
    interp.macro_hash_index = null;
    interp.macro_count = 0;
    interp.macro_capacity = 0;
    interp.macro_hash_capacity = 0;

    usz needed = saved.macro_count > 0 ? saved.macro_count : MACRO_INITIAL_CAPACITY;
    if (!interp.ensure_macro_capacity(needed)) {
        self.leave_macro_expansion_sandbox(saved);
        self.set_compile_error("compiler: out of memory while isolating macro expansion state");
        return false;
    }

    for (usz i = 0; i < saved.macro_count; i++) {
        if (!compiler_clone_macro_def(&interp.macro_table[i], &saved.macro_table[i])) {
            interp.macro_count = i;
            self.leave_macro_expansion_sandbox(saved);
            self.set_compile_error("compiler: out of memory while copying macro expansion state");
            return false;
        }
    }
    interp.macro_count = saved.macro_count;
    compiler_rebuild_macro_hash(interp);
    return true;
}

fn bool Compiler.expand_program_macros_for_analysis(Compiler* self, List{Expr*}* exprs) {
    CompilerMacroStateSnapshot saved;
    if (!self.enter_macro_expansion_sandbox(&saved)) return false;
    defer self.leave_macro_expansion_sandbox(&saved);

    expand_macros_in_sequence(exprs.entries, exprs.len(), self.interp);
    return !self.has_error;
}

fn void Compiler.prepare_program_analysis(Compiler* self, List{Expr*}* exprs) {
    if (!self.expand_program_macros_for_analysis(exprs)) return;
    self.collect_top_level_defines(exprs);
    foreach (expr : *exprs) {
        self.prescan_mutable_captures(expr);
    }
    foreach (expr : *exprs) {
        self.scan_lambdas(expr);
    }
}
