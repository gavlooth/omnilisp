module lisp;

import std::io;
import main;

fn usz Compiler.compile_defabstract_direct(Compiler* self, Expr* expr) {
    usz id = self.next_result();
    self.emit_temp_decl(id);
    self.emit(" = aot::define_abstract(");
    self.emit_string_literal(self.interp.symbols.get_name(expr.defabstract.name));
    self.emit(", ");
    self.emit_bool_literal(expr.defabstract.has_parent);
    self.emit(", ");
    self.emit_string_literal(expr.defabstract.has_parent ? self.interp.symbols.get_name(expr.defabstract.parent) : "");
    self.emit(");\n");
    return id;
}
