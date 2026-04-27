module lisp;

import std::io;
import main;

fn bool compiler_aot_type_literal_tag_supported(ValueTag tag) @inline {
    switch (tag) {
        case INT:
        case FLOAT32:
        case SYMBOL:
        case STRING:
        case NIL:
            return true;
        default:
            return false;
    }
}

fn void Compiler.emit_aot_type_annotation_spec_init(Compiler* self, char[] target, TypeAnnotation* ann) {
    self.emit_target_assignment_prefix(target, "has_annotation");
    self.emit_bool_literal(ann.has_annotation);
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "is_compound");
    self.emit_bool_literal(ann.is_compound);
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "is_dict");
    self.emit_bool_literal(ann.is_dict);
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "base_type");
    self.emit_string_literal((uint)ann.base_type != 0 ? self.interp.symbols.get_name(ann.base_type) : "");
    self.emit(";\n");

    if (ann.param_count > 0 && ann.params != null) {
        usz params_id = self.next_result();
        char[64] params_name_buf;
        char[] params_name = self.generated_name_id(params_name_buf[..], "_aot_ann_params_", (long)params_id);
        self.emit_indent();
        self.emit("char[][");
        self.emit_usz(ann.param_count);
        self.emit("] ");
        self.emit(params_name);
        self.emit(";\n");
        for (usz i = 0; i < ann.param_count; i++) {
            self.emit_indent();
            self.emit(params_name);
            self.emit("[");
            self.emit_usz(i);
            self.emit("] = ");
            self.emit_string_literal(self.interp.symbols.get_name(ann.params[i]));
            self.emit(";\n");
        }

        self.emit_target_assignment_prefix(target, "params");
        self.emit("&");
        self.emit(params_name);
        self.emit("[0];\n");
    } else {
        self.emit_target_assignment_prefix(target, "params");
        self.emit("null;\n");
    }

    self.emit_target_assignment_prefix(target, "param_count");
    self.emit_usz(ann.param_count);
    self.emit(";\n");

    if (ann.meta_count > 0 && ann.meta != null) {
        usz meta_id = self.next_result();
        char[64] meta_name_buf;
        char[] meta_name = self.generated_name_id(meta_name_buf[..], "_aot_ann_meta_", (long)meta_id);
        self.emit_indent();
        self.emit("lisp::aot::AotMetaSpec[");
        self.emit_usz(ann.meta_count);
        self.emit("] ");
        self.emit(meta_name);
        self.emit(";\n");
        for (usz i = 0; i < ann.meta_count; i++) {
            char[96] entry_buf;
            char[] entry = self.generated_name_index(entry_buf[..], meta_name, (long)i);
            self.emit_target_assignment_prefix(entry, "key");
            self.emit_string_literal(self.interp.symbols.get_name(ann.meta[i].key));
            self.emit(";\n");
            self.emit_target_assignment_prefix(entry, "sym_value");
            self.emit_string_literal(!ann.meta[i].is_int && (uint)ann.meta[i].sym_value != 0 ? self.interp.symbols.get_name(ann.meta[i].sym_value) : "");
            self.emit(";\n");
            self.emit_target_assignment_prefix(entry, "int_value");
            self.emit_int(ann.meta[i].int_value);
            self.emit(";\n");
            self.emit_target_assignment_prefix(entry, "is_int");
            self.emit_bool_literal(ann.meta[i].is_int);
            self.emit(";\n");
        }

        self.emit_target_assignment_prefix(target, "meta");
        self.emit("&");
        self.emit(meta_name);
        self.emit("[0];\n");
    } else {
        self.emit_target_assignment_prefix(target, "meta");
        self.emit("null;\n");
    }

    self.emit_target_assignment_prefix(target, "meta_count");
    self.emit_usz(ann.meta_count);
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "has_val_literal");
    self.emit_bool_literal(ann.has_val_literal);
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "val_tag");
    if (ann.has_val_literal && !compiler_aot_type_literal_tag_supported(ann.val_literal.tag)) {
        self.set_compile_error("compiler: unsupported type literal tag in AOT metadata");
    }
    self.emit_value_tag_literal(ann.val_literal.tag);
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "val_int");
    self.emit_int(ann.val_literal.int_value);
    self.emit(";\n");

    self.emit_target_assignment_prefix(target, "val_sym");
    if ((uint)ann.val_literal.sym_value != 0) {
        self.emit_string_literal(self.interp.symbols.get_name(ann.val_literal.sym_value));
    } else {
        self.emit_string_literal("");
    }
    self.emit(";\n");
}
