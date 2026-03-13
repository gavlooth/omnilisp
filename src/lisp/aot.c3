module lisp::aot;

import std::io;
import lisp;
import main;

// =============================================================================
// AOT Wrapper Module
// =============================================================================
// This module provides a clean API for AOT-compiled code to call the JIT
// infrastructure. Generated C3 code calls aot::* functions instead of
// runtime::* functions.

tlocal lisp::Interp* g_aot_interp = null;

fn void aot_init() {
    main::thread_registry_init();
    g_aot_interp = (lisp::Interp*)mem::malloc(lisp::Interp.sizeof);
    g_aot_interp.init();
    lisp::register_primitives(g_aot_interp);
    lisp::register_stdlib(g_aot_interp);
    g_aot_interp.flags.jit_enabled = true;
}

fn void aot_shutdown() {
    if (g_aot_interp != null) {
        g_aot_interp.destroy();
        mem::free(g_aot_interp);
        g_aot_interp = null;
    }
    main::thread_registry_shutdown();
}

fn lisp::Interp* aot_interp() @inline {
    return g_aot_interp;
}

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

fn lisp::Value* aot_eval_result_value(lisp::EvalResult r) @inline {
    if (r.error.has_error) return lisp::make_error(g_aot_interp, r.error.message[:256]);
    return r.value != null ? r.value : lisp::make_nil(g_aot_interp);
}

fn lisp::SymbolId aot_symbol_from_text(char[] text) @inline {
    if (text.len == 0) return (lisp::SymbolId)0;
    return g_aot_interp.symbols.intern(text);
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
    ann.param_count = spec.param_count;
    if (spec.param_count > 0) {
        ann.params = (lisp::SymbolId*)mem::malloc(lisp::SymbolId.sizeof * spec.param_count);
        if (ann.params == null) return false;
        for (usz i = 0; i < spec.param_count; i++) {
            ann.params[i] = aot_symbol_from_text(spec.params[i]);
        }
    }

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
    aot_method_signature_init_empty(sig);
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
    usz constraint_count = 0;
    for (usz i = 0; i < param_count; i++) {
        if (!specs[i].has_annotation || !specs[i].is_dict) continue;
        for (usz m = 0; m < specs[i].meta_count; m++) {
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
                lisp::SymbolId param_sym = aot_symbol_from_text(ann.meta[m].key);
                if (!saw_type_var) {
                    sig.param_type_vars[i] = param_sym;
                    saw_type_var = true;
                } else if ((uint)sig.param_type_vars[i] != (uint)param_sym) {
                    sig.param_type_vars[i] = (lisp::SymbolId)0;
                }
                sig.constraints[next_constraint].param_sym = param_sym;
                sig.constraints[next_constraint].bound_type =
                    g_aot_interp.types.lookup(aot_symbol_from_text(ann.meta[m].sym_value), &g_aot_interp.symbols);
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

fn lisp::Value* define_type(
    char[] name,
    bool has_parent,
    char[] parent_name,
    char[]* type_params,
    usz type_param_count,
    AotTypeFieldSpec* fields,
    usz field_count
) {
    lisp::TypeField* rt_fields = field_count > 0
        ? (lisp::TypeField*)mem::malloc(lisp::TypeField.sizeof * field_count)
        : null;
    defer if (rt_fields != null) {
        for (usz i = 0; i < field_count; i++) aot_type_annotation_destroy(&rt_fields[i].type_ann);
        mem::free(rt_fields);
    };

    lisp::SymbolId* rt_type_params = type_param_count > 0
        ? (lisp::SymbolId*)mem::malloc(lisp::SymbolId.sizeof * type_param_count)
        : null;
    defer if (rt_type_params != null) mem::free(rt_type_params);

    if ((field_count > 0 && rt_fields == null) || (type_param_count > 0 && rt_type_params == null)) {
        return lisp::make_error(g_aot_interp, "aot define-type: out of memory");
    }

    for (usz i = 0; i < field_count; i++) {
        aot_type_annotation_init_empty(&rt_fields[i].type_ann);
    }
    for (usz i = 0; i < type_param_count; i++) {
        rt_type_params[i] = aot_symbol_from_text(type_params[i]);
    }
    for (usz i = 0; i < field_count; i++) {
        rt_fields[i].name = aot_symbol_from_text(fields[i].name);
        if (!aot_type_annotation_init_from_spec(&rt_fields[i].type_ann, &fields[i].annotation)) {
            return lisp::make_error(g_aot_interp, "aot define-type: out of memory");
        }
    }

    lisp::ExprDefType dt;
    dt.name = aot_symbol_from_text(name);
    dt.parent = has_parent ? aot_symbol_from_text(parent_name) : (lisp::SymbolId)0;
    dt.has_parent = has_parent;
    dt.fields = rt_fields;
    dt.field_count = field_count;
    dt.type_params = rt_type_params;
    dt.type_param_count = type_param_count;

    lisp::Expr expr;
    expr.tag = lisp::ExprTag.E_DEFTYPE;
    expr.deftype = &dt;
    return aot_eval_result_value(lisp::eval_deftype(&expr, g_aot_interp.global_env, g_aot_interp));
}

fn lisp::Value* define_abstract(char[] name, bool has_parent, char[] parent_name) {
    lisp::ExprDefAbstract da;
    da.name = aot_symbol_from_text(name);
    da.parent = has_parent ? aot_symbol_from_text(parent_name) : (lisp::SymbolId)0;
    da.has_parent = has_parent;

    lisp::Expr expr;
    expr.tag = lisp::ExprTag.E_DEFABSTRACT;
    expr.defabstract = da;
    return aot_eval_result_value(lisp::eval_defabstract(&expr, g_aot_interp.global_env, g_aot_interp));
}

fn lisp::Value* define_union(
    char[] name,
    char[]* type_params,
    usz type_param_count,
    AotUnionVariantSpec* variants,
    usz variant_count
) {
    lisp::SymbolId* rt_type_params = type_param_count > 0
        ? (lisp::SymbolId*)mem::malloc(lisp::SymbolId.sizeof * type_param_count)
        : null;
    defer if (rt_type_params != null) mem::free(rt_type_params);

    lisp::UnionVariant* rt_variants = variant_count > 0
        ? (lisp::UnionVariant*)mem::malloc(lisp::UnionVariant.sizeof * variant_count)
        : null;
    defer if (rt_variants != null) {
        for (usz i = 0; i < variant_count; i++) {
            if (rt_variants[i].fields != null) mem::free(rt_variants[i].fields);
        }
        mem::free(rt_variants);
    };

    if ((type_param_count > 0 && rt_type_params == null) || (variant_count > 0 && rt_variants == null)) {
        return lisp::make_error(g_aot_interp, "aot define-union: out of memory");
    }

    for (usz i = 0; i < variant_count; i++) {
        rt_variants[i].fields = null;
        rt_variants[i].field_count = 0;
    }
    for (usz i = 0; i < type_param_count; i++) {
        rt_type_params[i] = aot_symbol_from_text(type_params[i]);
    }
    for (usz i = 0; i < variant_count; i++) {
        rt_variants[i].name = aot_symbol_from_text(variants[i].name);
        rt_variants[i].field_count = variants[i].field_count;
        rt_variants[i].fields = variants[i].field_count > 0
            ? (lisp::SymbolId*)mem::malloc(lisp::SymbolId.sizeof * variants[i].field_count)
            : null;
        if (variants[i].field_count > 0 && rt_variants[i].fields == null) {
            return lisp::make_error(g_aot_interp, "aot define-union: out of memory");
        }
        for (usz fi = 0; fi < variants[i].field_count; fi++) {
            rt_variants[i].fields[fi] = aot_symbol_from_text(variants[i].fields[fi]);
        }
    }

    lisp::ExprDefUnion du;
    du.name = aot_symbol_from_text(name);
    du.type_params = rt_type_params;
    du.type_param_count = type_param_count;
    du.variants = rt_variants;
    du.variant_count = variant_count;

    lisp::Expr expr;
    expr.tag = lisp::ExprTag.E_DEFUNION;
    expr.defunion = &du;
    return aot_eval_result_value(lisp::eval_defunion(&expr, g_aot_interp.global_env, g_aot_interp));
}

fn lisp::Value* define_alias(char[] name, AotTypeAnnotationSpec* target_spec) {
    lisp::TypeAnnotation target;
    if (!aot_type_annotation_init_from_spec(&target, target_spec)) {
        return lisp::make_error(g_aot_interp, "aot define-alias: out of memory");
    }
    defer aot_type_annotation_destroy(&target);

    lisp::ExprDefAlias da;
    da.name = aot_symbol_from_text(name);
    da.target = target;

    lisp::Expr expr;
    expr.tag = lisp::ExprTag.E_DEFALIAS;
    expr.defalias = da;
    return aot_eval_result_value(lisp::eval_defalias(&expr, g_aot_interp.global_env, g_aot_interp));
}

fn lisp::Value* define_effect(char[] name, bool has_arg_type, AotTypeAnnotationSpec* arg_type_spec) {
    lisp::TypeAnnotation arg_type;
    if (!aot_type_annotation_init_from_spec(&arg_type, arg_type_spec)) {
        return lisp::make_error(g_aot_interp, "aot define-effect: out of memory");
    }
    defer aot_type_annotation_destroy(&arg_type);

    lisp::ExprDefEffect de;
    de.name = aot_symbol_from_text(name);
    de.arg_type = arg_type;
    de.has_arg_type = has_arg_type;

    lisp::Expr expr;
    expr.tag = lisp::ExprTag.E_DEFEFFECT;
    expr.defeffect = de;
    return aot_eval_result_value(lisp::eval_defeffect(&expr, g_aot_interp.global_env, g_aot_interp));
}

fn lisp::Value* define_typed_method(char[] name, lisp::Value* value, AotTypeAnnotationSpec* param_specs, usz param_count) {
    lisp::MethodSignature sig;
    if (!aot_build_method_signature_from_specs(&sig, param_specs, param_count)) {
        return lisp::make_error(g_aot_interp, "aot define-typed-method: out of memory");
    }
    defer aot_method_signature_destroy(&sig);

    lisp::Value* stored_val = lisp::boundary_promote_to_root_site(
        value,
        g_aot_interp,
        lisp::COPY_SITE_JIT_EVAL_DEFINE
    );
    if (stored_val != null && stored_val.tag == lisp::ValueTag.ERROR) return stored_val;

    lisp::SymbolId name_sym = aot_symbol_from_text(name);
    return lisp::jit_eval_define_typed_callable(g_aot_interp, name_sym, stored_val, &sig);
}

// =============================================================================
// Value Constructors
// =============================================================================

fn lisp::Value* make_nil() @inline { return lisp::make_nil(g_aot_interp); }
fn lisp::Value* make_void() @inline { return lisp::make_void(g_aot_interp); }
fn lisp::Value* make_int(long n) @inline { return lisp::make_int(g_aot_interp, n); }
fn lisp::Value* make_double(double d) @inline { return lisp::make_double(g_aot_interp, d); }
fn lisp::Value* make_string(char[] s) @inline { return lisp::make_string(g_aot_interp, s); }
fn lisp::Value* make_symbol(char[] s) @inline {
    lisp::SymbolId sid = g_aot_interp.symbols.intern(s);
    return lisp::make_symbol(g_aot_interp, sid);
}
fn lisp::Value* make_time_point(int kind, int year, int month, int day, int hour, int minute, int second, int microsecond, int tz_offset) @inline {
    return lisp::make_time_point(g_aot_interp, (lisp::TimePointKind)kind, year, month, day, hour, minute, second, microsecond, tz_offset);
}
fn lisp::Value* make_true() @inline { return lisp::make_symbol(g_aot_interp, g_aot_interp.sym_true); }
fn lisp::Value* make_false() @inline { return lisp::make_nil(g_aot_interp); }

// =============================================================================
// List Operations
// =============================================================================

fn lisp::Value* cons(lisp::Value* car, lisp::Value* cdr) @inline { return lisp::make_cons(g_aot_interp, car, cdr); }
fn lisp::Value* car(lisp::Value* v) @inline {
    if (v == null || v.tag != lisp::ValueTag.CONS) return lisp::make_nil(g_aot_interp);
    return v.cons_val.car;
}
fn lisp::Value* cdr(lisp::Value* v) @inline {
    if (v == null || v.tag != lisp::ValueTag.CONS) return lisp::make_nil(g_aot_interp);
    return v.cons_val.cdr;
}

// =============================================================================
// Function Invocation
// =============================================================================

// --- Tail-call trampoline state ---
tlocal lisp::Value* g_tail_func = null;
tlocal lisp::Value* g_tail_arg = null;
tlocal long g_tail_argc = 0;
tlocal bool g_tail_is_multi = false;
tlocal bool g_tail_pending = false;

struct TailStateSnapshot {
    lisp::Value* func;
    lisp::Value* arg;
    long argc;
    bool is_multi;
    bool pending;
}

fn void tail_state_capture(TailStateSnapshot* s) @inline {
    s.func = g_tail_func;
    s.arg = g_tail_arg;
    s.argc = g_tail_argc;
    s.is_multi = g_tail_is_multi;
    s.pending = g_tail_pending;
}

fn void tail_state_restore(TailStateSnapshot* s) @inline {
    g_tail_func = s.func;
    g_tail_arg = s.arg;
    g_tail_argc = s.argc;
    g_tail_is_multi = s.is_multi;
    g_tail_pending = s.pending;
}

/**
 * invoke_once — Single call without trampoline loop.
 * Used internally by invoke's trampoline.
 */
fn lisp::Value* invoke_once(lisp::Value* func, lisp::Value* arg) @inline {
    // Fast path: AOT closure → call directly, skipping JIT apply indirection
    if (func != null && func.tag == lisp::ValueTag.PRIMITIVE && func.prim_val != null) {
        lisp::PrimitiveFn pfn = func.prim_val.func;
        if (pfn == &aot_closure_apply) {
            AotClosureData* cd = (AotClosureData*)func.prim_val.user_data;
            if (cd != null) return cd.invoke(cd.data, arg);
        }
        if (pfn == &aot_variadic_apply) {
            AotClosureData* cd = (AotClosureData*)func.prim_val.user_data;
            if (cd != null) {
                // Variadic closures expect a proper list — wrap single arg
                lisp::Value* arg_list = lisp::make_cons(g_aot_interp, arg, lisp::make_nil(g_aot_interp));
                return cd.invoke_var(cd.data, arg_list);
            }
        }
    }
    // Fall through to JIT path for non-AOT values (real primitives, JIT closures, continuations)
    return lisp::jit_apply_value(func, arg, g_aot_interp);
}

/**
 * apply_multi_once — Single multi-arg call without trampoline loop.
 * Used internally by apply_multi's trampoline.
 */
fn lisp::Value* apply_multi_once(lisp::Value* func, lisp::Value* arg_list, long argc) @inline {
    // Fast path: AOT closure → call directly, skipping JIT apply_multi indirection
    if (func != null && func.tag == lisp::ValueTag.PRIMITIVE && func.prim_val != null) {
        lisp::PrimitiveFn pfn = func.prim_val.func;
        if (pfn == &aot_variadic_apply) {
            AotClosureData* cd = (AotClosureData*)func.prim_val.user_data;
            if (cd != null) return cd.invoke_var(cd.data, arg_list);
        }
        if (pfn == &aot_closure_apply) {
            // Non-variadic AOT closure called with multi-arg: take first arg
            AotClosureData* cd = (AotClosureData*)func.prim_val.user_data;
            if (cd != null) {
                lisp::Value* first_arg = (arg_list != null && arg_list.tag == lisp::ValueTag.CONS)
                    ? arg_list.cons_val.car : lisp::make_nil(g_aot_interp);
                return cd.invoke(cd.data, first_arg);
            }
        }
    }
    // Fall through to JIT path for non-AOT values
    return lisp::jit_apply_multi_args(g_aot_interp, func, arg_list, (usz)argc);
}

/**
 * invoke — Call a function with trampoline for tail-call optimization.
 * After the initial call, if g_tail_pending is set (by invoke_tail/apply_multi_tail),
 * re-dispatch the stored tail call instead of recursing.
 */
fn lisp::Value* invoke(lisp::Value* func, lisp::Value* arg) {
    TailStateSnapshot saved;
    tail_state_capture(&saved);
    g_tail_pending = false;
    g_tail_func = null;
    g_tail_arg = null;
    g_tail_argc = 0;
    g_tail_is_multi = false;

    lisp::Value* result = invoke_once(func, arg);
    while (g_tail_pending) {
        g_tail_pending = false;
        if (g_tail_is_multi) {
            result = apply_multi_once(g_tail_func, g_tail_arg, g_tail_argc);
        } else {
            result = invoke_once(g_tail_func, g_tail_arg);
        }
    }
    tail_state_restore(&saved);
    return result;
}

/**
 * apply_multi — Call a function with multiple args, with trampoline for TCO.
 */
fn lisp::Value* apply_multi(lisp::Value* func, lisp::Value* arg_list, long argc) {
    TailStateSnapshot saved;
    tail_state_capture(&saved);
    g_tail_pending = false;
    g_tail_func = null;
    g_tail_arg = null;
    g_tail_argc = 0;
    g_tail_is_multi = false;

    lisp::Value* result = apply_multi_once(func, arg_list, argc);
    while (g_tail_pending) {
        g_tail_pending = false;
        if (g_tail_is_multi) {
            result = apply_multi_once(g_tail_func, g_tail_arg, g_tail_argc);
        } else {
            result = invoke_once(g_tail_func, g_tail_arg);
        }
    }
    tail_state_restore(&saved);
    return result;
}

/**
 * invoke_tail — Register a tail call (single-arg) and return immediately.
 * The caller's invoke/apply_multi trampoline will dispatch this.
 */
fn lisp::Value* invoke_tail(lisp::Value* func, lisp::Value* arg) @inline {
    g_tail_func = func;
    g_tail_arg = arg;
    g_tail_is_multi = false;
    g_tail_pending = true;
    return lisp::make_nil(g_aot_interp);
}

/**
 * apply_multi_tail — Register a tail call (multi-arg) and return immediately.
 * The caller's invoke/apply_multi trampoline will dispatch this.
 */
fn lisp::Value* apply_multi_tail(lisp::Value* func, lisp::Value* arg_list, long argc) @inline {
    g_tail_func = func;
    g_tail_arg = arg_list;
    g_tail_argc = argc;
    g_tail_is_multi = true;
    g_tail_pending = true;
    return lisp::make_nil(g_aot_interp);
}

fn bool is_truthy(lisp::Value* v) @inline {
    if (v == null) return false;
    if (v.tag == lisp::ValueTag.NIL) return false;
    if (v.tag == lisp::ValueTag.SYMBOL && v.sym_val == g_aot_interp.sym_false) return false;
    return true;
}

fn bool values_equal(lisp::Value* a, lisp::Value* b) {
    return lisp::values_equal(a, b);
}

fn bool is_list(lisp::Value* v) {
    return lisp::is_list(v);
}

fn usz list_length(lisp::Value* v) {
    return lisp::list_length(v);
}

fn lisp::Value* list_nth(lisp::Value* v, long n) {
    return lisp::get_list_nth(v, (usz)n);
}

fn lisp::Value* list_rest(lisp::Value* v, long n) {
    return lisp::get_list_rest(v, (usz)n, g_aot_interp);
}

fn lisp::Value* lookup_prim(char[] name) {
    lisp::SymbolId sid = g_aot_interp.symbols.intern(name);
    lisp::Value* v = g_aot_interp.global_env.lookup(sid);
    if (v != null) return v;
    return lisp::make_nil(g_aot_interp);
}

// Legacy/debug bridge only. Normal compiler lowering must not emit this path.
// Kept for explicit manual debugging of serialized forms.
fn lisp::Value* eval_serialized_expr(char[] source) @deprecated("Legacy debug-only bridge; compiler should use direct lowering helpers") {
    lisp::EvalResult r = lisp::run(source, g_aot_interp);
    if (r.error.has_error) {
        return lisp::make_error(g_aot_interp, r.error.message[:256]);
    }
    return r.value != null ? r.value : lisp::make_nil(g_aot_interp);
}

// =============================================================================
// AOT Closure Creation
// =============================================================================

alias AotClosureFn = fn lisp::Value*(void* data, lisp::Value* arg);
alias AotVariadicFn = fn lisp::Value*(void* data, lisp::Value* arg_list);

struct AotClosureData {
    void*         data;
    AotClosureFn  invoke;
    AotVariadicFn invoke_var;
    bool          is_variadic;
}

fn lisp::Value* aot_closure_apply(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    TailStateSnapshot saved;
    tail_state_capture(&saved);
    g_tail_pending = false;
    g_tail_func = null;
    g_tail_arg = null;
    g_tail_argc = 0;
    g_tail_is_multi = false;

    AotClosureData* cd = (AotClosureData*)interp.prim_user_data;
    lisp::Value* arg = args.len > 0 ? args[0] : lisp::make_nil(interp);
    lisp::Value* result = cd.invoke(cd.data, arg);
    while (g_tail_pending) {
        g_tail_pending = false;
        if (g_tail_is_multi) {
            result = apply_multi_once(g_tail_func, g_tail_arg, g_tail_argc);
        } else {
            result = invoke_once(g_tail_func, g_tail_arg);
        }
    }
    tail_state_restore(&saved);
    return result;
}

fn lisp::Value* aot_variadic_apply(lisp::Value*[] args, lisp::Env* env, lisp::Interp* interp) {
    TailStateSnapshot saved;
    tail_state_capture(&saved);
    g_tail_pending = false;
    g_tail_func = null;
    g_tail_arg = null;
    g_tail_argc = 0;
    g_tail_is_multi = false;

    AotClosureData* cd = (AotClosureData*)interp.prim_user_data;
    lisp::Value* list = lisp::make_nil(interp);
    for (isz i = (isz)args.len - 1; i >= 0; i--) list = lisp::make_cons(interp, args[(usz)i], list);
    lisp::Value* result = cd.invoke_var(cd.data, list);
    while (g_tail_pending) {
        g_tail_pending = false;
        if (g_tail_is_multi) {
            result = apply_multi_once(g_tail_func, g_tail_arg, g_tail_argc);
        } else {
            result = invoke_once(g_tail_func, g_tail_arg);
        }
    }
    tail_state_restore(&saved);
    return result;
}

fn lisp::Value* make_closure(void* data, AotClosureFn invoke) {
    AotClosureData* cd = (AotClosureData*)mem::malloc(AotClosureData.sizeof);
    cd.data = data; cd.invoke = invoke; cd.invoke_var = null; cd.is_variadic = false;
    lisp::Value* v = lisp::make_primitive(g_aot_interp, "aot-closure", &aot_closure_apply, 1);
    v.prim_val.user_data = (void*)cd;
    return v;
}

fn lisp::Value* make_variadic_closure(void* data, AotVariadicFn invoke) {
    AotClosureData* cd = (AotClosureData*)mem::malloc(AotClosureData.sizeof);
    cd.data = data; cd.invoke = null; cd.invoke_var = invoke; cd.is_variadic = true;
    lisp::Value* v = lisp::make_primitive(g_aot_interp, "aot-closure-var", &aot_variadic_apply, -1);
    v.prim_val.user_data = (void*)cd;
    return v;
}

// =============================================================================
// Variable Table Operations
// =============================================================================

fn void define_var(char[] name, lisp::Value* val) {
    lisp::SymbolId sid = g_aot_interp.symbols.intern(name);
    lisp::env_define_with_barrier(g_aot_interp.global_env, sid, val, g_aot_interp);
}

fn lisp::Value* lookup_var(char[] name) {
    lisp::SymbolId sid = g_aot_interp.symbols.intern(name);
    lisp::Value* v = g_aot_interp.global_env.lookup(sid);
    return v != null ? v : lisp::make_nil(g_aot_interp);
}

fn void set_var(char[] name, lisp::Value* val) {
    lisp::SymbolId sid = g_aot_interp.symbols.intern(name);
    // AOT set_var on global env — silently ignore if unbound (define_var should be called first)
    lisp::env_set_with_barrier(g_aot_interp.global_env, sid, val, g_aot_interp)!!;
}

fn lisp::Value* explain_dispatch(char[] source, lisp::Value* callee, lisp::Value* arg_list, long argc) {
    if (argc < 0) return lisp::make_error(g_aot_interp, "aot explain-dispatch: negative arg count");
    usz arg_count = (usz)argc;
    lisp::Value** storage = arg_count > 0
        ? (lisp::Value**)mem::malloc(lisp::Value*.sizeof * arg_count)
        : null;
    defer if (storage != null) mem::free(storage);

    if (arg_count > 0 && storage == null) {
        return lisp::make_error(g_aot_interp, "aot explain-dispatch: out of memory");
    }

    lisp::Value* curr = arg_list;
    for (usz i = 0; i < arg_count; i++) {
        if (curr == null || curr.tag != lisp::ValueTag.CONS) {
            return lisp::make_error(g_aot_interp, "aot explain-dispatch: arg list too short");
        }
        storage[i] = curr.cons_val.car;
        curr = curr.cons_val.cdr;
    }

    lisp::Value*[] args = arg_count > 0 ? storage[:arg_count] : {};
    return lisp::explain_dispatch_result_from_compiled_call(g_aot_interp, source, callee, args);
}

fn lisp::Value* explain_effect_signal(char[] source, char[] effect_name, lisp::Value* arg) {
    return lisp::explain_effect_result_from_compiled_signal(
        g_aot_interp,
        source,
        aot_symbol_from_text(effect_name),
        arg
    );
}

fn lisp::Value* explain_effect_resolve(char[] source, lisp::Value* continuation) {
    return lisp::explain_effect_result_from_compiled_resolve(g_aot_interp, source, continuation);
}

// =============================================================================
// Output
// =============================================================================

fn void print_value(lisp::Value* v) {
    lisp::print_value(v, &g_aot_interp.symbols);
    io::print("\n");
}

// =============================================================================
// Dict/Array Operations
// =============================================================================

fn lisp::Value* dict_from_args(lisp::Value* arg_list) {
    lisp::Value* dict = lisp::make_hashmap(g_aot_interp, 16);
    lisp::HashMap* map = dict.hashmap_val;
    lisp::Value* curr = arg_list;
    while (curr != null && curr.tag == lisp::ValueTag.CONS) {
        lisp::Value* head = curr.cons_val.car;

        // Pair-list form: ((k . v) ...)
        if (head != null && head.tag == lisp::ValueTag.CONS) {
            lisp::Value* key = head.cons_val.car;
            lisp::Value* val_pair = head.cons_val.cdr;
            lisp::Value* val = (val_pair != null && val_pair.tag == lisp::ValueTag.CONS)
                ? val_pair.cons_val.car
                : lisp::make_nil(g_aot_interp);
            lisp::hashmap_set(map, key, val, g_aot_interp);
            curr = curr.cons_val.cdr;
            continue;
        }

        // Flat arg form: (k1 v1 k2 v2 ...)
        lisp::Value* key = head;
        curr = curr.cons_val.cdr;
        lisp::Value* val = lisp::make_nil(g_aot_interp);
        if (curr != null && curr.tag == lisp::ValueTag.CONS) {
            val = curr.cons_val.car;
            curr = curr.cons_val.cdr;
        }
        lisp::hashmap_set(map, key, val, g_aot_interp);
    }
    return dict;
}

fn lisp::Value* index(lisp::Value* collection, lisp::Value* idx) {
    lisp::Value* ref_fn = g_aot_interp.global_env.lookup(g_aot_interp.symbols.intern("ref"));
    if (ref_fn == null) return lisp::raise_error(g_aot_interp, "ref: primitive not found");
    lisp::Value* partial = lisp::jit_apply_value(ref_fn, collection, g_aot_interp);
    if (partial != null && partial.tag == lisp::ValueTag.ERROR) return partial;
    return lisp::jit_apply_value(partial, idx, g_aot_interp);
}

// =============================================================================
// Effects (Bridge to JIT)
// =============================================================================

/**
 * AOT handle wrapper.
 */
fn lisp::Value* compiled_handle(lisp::Value** tags, lisp::Value** handlers, usz count, lisp::Value* body) {
    return lisp::jit_handle_value(g_aot_interp, tags, handlers, count, body);
}
/**
 * AOT signal wrapper.
 */
fn lisp::Value* compiled_signal(lisp::Value* tag, lisp::Value* arg) {
    return lisp::jit_signal_value(g_aot_interp, tag, arg);
}

/**
 * AOT reset wrapper.
 */
fn lisp::Value* compiled_reset(lisp::Value* body) {
    return lisp::jit_reset_value(g_aot_interp, body);
}

/**
 * AOT shift wrapper.
 */
fn lisp::Value* compiled_shift(lisp::Value* body) {
    return lisp::jit_shift_value(g_aot_interp, body);
}

/**
 * AOT resolve wrapper.
 */
fn lisp::Value* compiled_resolve(lisp::Value* k_val, lisp::Value* val) {
    return lisp::jit_resolve_value(g_aot_interp, k_val, val);
}
