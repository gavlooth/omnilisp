module lisp::aot;

import std::core::mem;
import lisp;

struct AotMetaSpec {
    char[] key;
    char[] sym_value;
    long int_value;
    bool is_int;
}

struct AotTypeAnnotationSpec {
    bool has_annotation;
    bool is_compound;
    bool is_dict;
    char[] base_type;
    char[]* params;
    usz param_count;
    AotMetaSpec* meta;
    usz meta_count;
    bool has_val_literal;
    lisp::ValueTag val_tag;
    long val_int;
    char[] val_sym;
}

struct AotTypeFieldSpec {
    char[] name;
    AotTypeAnnotationSpec annotation;
}

struct AotUnionVariantSpec {
    char[] name;
    char[]* fields;
    usz field_count;
}

fn void aot_type_annotation_init_empty(lisp::TypeAnnotation* ann) {
    ann.has_annotation = false;
    ann.is_compound = false;
    ann.is_dict = false;
    ann.base_type = (lisp::SymbolId)0;
    ann.params = null;
    ann.param_count = 0;
    ann.meta = null;
    ann.meta_count = 0;
    ann.val_literal.tag = lisp::ValueTag.NIL;
    ann.val_literal.int_value = 0;
    ann.val_literal.sym_value = (lisp::SymbolId)0;
    ann.has_val_literal = false;
}

fn void aot_type_annotation_destroy(lisp::TypeAnnotation* ann) {
    if (ann == null) return;
    if (ann.params != null) mem::free(ann.params);
    if (ann.meta != null) mem::free(ann.meta);
    aot_type_annotation_init_empty(ann);
}

fn bool aot_type_annotation_init_from_spec(lisp::TypeAnnotation* ann, AotTypeAnnotationSpec* spec) {
    aot_type_annotation_init_empty(ann);
    if (spec == null || !spec.has_annotation) return true;

    ann.has_annotation = true;
    ann.is_compound = spec.is_compound;
    ann.is_dict = spec.is_dict;
    ann.base_type = aot_symbol_from_text(spec.base_type);

    if (spec.param_count > usz.max / lisp::SymbolId.sizeof) return false;
    ann.param_count = spec.param_count;
    if (spec.param_count > 0) {
        ann.params = (lisp::SymbolId*)mem::malloc(lisp::SymbolId.sizeof * spec.param_count);
        if (ann.params == null) {
            aot_type_annotation_destroy(ann);
            return false;
        }
        for (usz i = 0; i < spec.param_count; i++) {
            ann.params[i] = aot_symbol_from_text(spec.params[i]);
        }
    }

    if (spec.meta_count > usz.max / lisp::MetaEntry.sizeof) return false;
    ann.meta_count = spec.meta_count;
    if (spec.meta_count > 0) {
        ann.meta = (lisp::MetaEntry*)mem::malloc(lisp::MetaEntry.sizeof * spec.meta_count);
        if (ann.meta == null) {
            aot_type_annotation_destroy(ann);
            return false;
        }
        for (usz i = 0; i < spec.meta_count; i++) {
            ann.meta[i].key = aot_symbol_from_text(spec.meta[i].key);
            ann.meta[i].is_int = spec.meta[i].is_int;
            ann.meta[i].int_value = spec.meta[i].int_value;
            ann.meta[i].sym_value = spec.meta[i].is_int ? (lisp::SymbolId)0 : aot_symbol_from_text(spec.meta[i].sym_value);
        }
    }

    ann.has_val_literal = spec.has_val_literal;
    if (spec.has_val_literal) {
        ann.val_literal.tag = spec.val_tag;
        ann.val_literal.int_value = spec.val_int;
        ann.val_literal.sym_value = aot_symbol_from_text(spec.val_sym);
    }
    return true;
}

fn void aot_method_signature_init_empty(lisp::MethodSignature* sig) {
    sig.param_types = null;
    sig.param_type_vars = null;
    sig.val_literals = null;
    sig.has_val_literal = null;
    sig.param_count = 0;
    sig.constraints = null;
    sig.constraint_count = 0;
}

fn void aot_method_signature_destroy(lisp::MethodSignature* sig) {
    if (sig == null) return;
    if (sig.param_types != null) mem::free(sig.param_types);
    if (sig.param_type_vars != null) mem::free(sig.param_type_vars);
    if (sig.val_literals != null) mem::free(sig.val_literals);
    if (sig.has_val_literal != null) mem::free(sig.has_val_literal);
    if (sig.constraints != null) mem::free(sig.constraints);
    aot_method_signature_init_empty(sig);
}

fn bool aot_method_signature_alloc(lisp::MethodSignature* sig, usz param_count, usz constraint_count) {
    if (sig == null) return false;
    aot_method_signature_init_empty(sig);

    if (param_count > usz.max / lisp::TypeId.sizeof) return false;
    if (param_count > usz.max / lisp::SymbolId.sizeof) return false;
    if (param_count > usz.max / lisp::ValueLiteralKey.sizeof) return false;
    if (param_count > usz.max / bool.sizeof) return false;
    if (constraint_count > usz.max / lisp::MethodConstraint.sizeof) return false;

    sig.param_count = param_count;
    sig.constraint_count = constraint_count;
    if (param_count > 0) {
        sig.param_types = (lisp::TypeId*)mem::malloc(lisp::TypeId.sizeof * param_count);
        sig.param_type_vars = (lisp::SymbolId*)mem::malloc(lisp::SymbolId.sizeof * param_count);
        sig.val_literals = (lisp::ValueLiteralKey*)mem::malloc(lisp::ValueLiteralKey.sizeof * param_count);
        sig.has_val_literal = (bool*)mem::malloc(bool.sizeof * param_count);
        if (sig.param_types == null || sig.param_type_vars == null || sig.val_literals == null || sig.has_val_literal == null) {
            aot_method_signature_destroy(sig);
            return false;
        }
        for (usz i = 0; i < param_count; i++) {
            sig.param_types[i] = lisp::INVALID_TYPE_ID;
            sig.param_type_vars[i] = (lisp::SymbolId)0;
            sig.val_literals[i].tag = lisp::ValueTag.NIL;
            sig.val_literals[i].int_value = 0;
            sig.val_literals[i].sym_value = (lisp::SymbolId)0;
            sig.has_val_literal[i] = false;
        }
    }
    if (constraint_count > 0) {
        sig.constraints = (lisp::MethodConstraint*)mem::malloc(lisp::MethodConstraint.sizeof * constraint_count);
        if (sig.constraints == null) {
            aot_method_signature_destroy(sig);
            return false;
        }
    }
    return true;
}

fn bool aot_build_method_signature_from_specs(lisp::MethodSignature* sig, AotTypeAnnotationSpec* specs, usz param_count) {
    if (specs == null && param_count > 0) return false;

    usz constraint_count = 0;
    for (usz i = 0; i < param_count; i++) {
        if (!specs[i].has_annotation || !specs[i].is_dict) continue;
        for (usz m = 0; m < specs[i].meta_count; m++) {
            if (constraint_count == usz.max) return false;
            if (!specs[i].meta[m].is_int) constraint_count++;
        }
    }
    if (!aot_method_signature_alloc(sig, param_count, constraint_count)) return false;

    lisp::SymbolId sym_lambda_type = g_aot_interp.symbols.intern("Lambda");
    usz next_constraint = 0;
    for (usz i = 0; i < param_count; i++) {
        AotTypeAnnotationSpec* ann = &specs[i];
        if (!ann.has_annotation) continue;

        if (ann.has_val_literal) {
            sig.has_val_literal[i] = true;
            sig.val_literals[i].tag = ann.val_tag;
            sig.val_literals[i].int_value = ann.val_int;
            sig.val_literals[i].sym_value = aot_symbol_from_text(ann.val_sym);
            continue;
        }

        if (ann.is_dict) {
            bool saw_type_var = false;
            for (usz m = 0; m < ann.meta_count; m++) {
                if (ann.meta[m].is_int) continue;
                if (next_constraint >= constraint_count) {
                    aot_method_signature_destroy(sig);
                    return false;
                }
                lisp::SymbolId param_sym = aot_symbol_from_text(ann.meta[m].key);
                if (!saw_type_var) {
                    sig.param_type_vars[i] = param_sym;
                    saw_type_var = true;
                } else if ((uint)sig.param_type_vars[i] != (uint)param_sym) {
                    sig.param_type_vars[i] = (lisp::SymbolId)0;
                }
                sig.constraints[next_constraint].param_sym = param_sym;
                sig.constraints[next_constraint].bound_type = g_aot_interp.types.lookup(aot_symbol_from_text(ann.meta[m].sym_value), &g_aot_interp.symbols);
                next_constraint++;
            }
            continue;
        }

        lisp::SymbolId base_type = aot_symbol_from_text(ann.base_type);
        lisp::TypeId resolved = g_aot_interp.types.lookup(base_type, &g_aot_interp.symbols);
        if ((uint)base_type == (uint)sym_lambda_type) {
            sig.param_types[i] = g_aot_interp.tid_Closure;
            sig.param_type_vars[i] = (lisp::SymbolId)0;
        } else {
            sig.param_types[i] = resolved;
            sig.param_type_vars[i] = resolved == lisp::INVALID_TYPE_ID ? base_type : (lisp::SymbolId)0;
        }
    }
    return true;
}
