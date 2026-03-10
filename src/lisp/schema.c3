module lisp;

import std::io;
import pika;
import main;

// ============================================================
// Schema Validation — data-driven contracts via multiple dispatch
//
// Schemas are plain Omni data (lists + symbols). validate/explain
// walk the schema and check values. Dispatch on the schema tag symbol.
//
// Schema types:
//   'string, 'int, 'double, 'bool, 'any, 'nil
//   '(and s1 s2), '(or s1 s2), '(not s), '(maybe s)
//   '(> n), '(< n), '(>= n), '(<= n), '(= v)
//   '(re pattern)
//   '(map (name schema) ...), '(vector-of schema), '(tuple s1 s2 ...)
//   '(enum v1 v2 ...)
// ============================================================

struct SchemaTagSet {
    SymbolId s_and;
    SymbolId s_or;
    SymbolId s_not;
    SymbolId s_maybe;
    SymbolId s_gt;
    SymbolId s_lt;
    SymbolId s_ge;
    SymbolId s_le;
    SymbolId s_eq;
    SymbolId s_re;
    SymbolId s_map;
    SymbolId s_vector_of;
    SymbolId s_tuple;
    SymbolId s_enum;
}

fn SchemaTagSet schema_make_tag_set(Interp* interp) {
    SchemaTagSet tags;
    tags.s_and = interp.symbols.intern("and");
    tags.s_or = interp.symbols.intern("or");
    tags.s_not = interp.symbols.intern("not");
    tags.s_maybe = interp.symbols.intern("maybe");
    tags.s_gt = interp.symbols.intern(">");
    tags.s_lt = interp.symbols.intern("<");
    tags.s_ge = interp.symbols.intern(">=");
    tags.s_le = interp.symbols.intern("<=");
    tags.s_eq = interp.symbols.intern("=");
    tags.s_re = interp.symbols.intern("re");
    tags.s_map = interp.symbols.intern("map");
    tags.s_vector_of = interp.symbols.intern("vector-of");
    tags.s_tuple = interp.symbols.intern("tuple");
    tags.s_enum = interp.symbols.intern("enum");
    return tags;
}

fn bool schema_string_equals(Value* a, Value* b) {
    if (a == null || b == null || a.tag != STRING || b.tag != STRING) return false;
    if (a.str_len != b.str_len) return false;
    for (usz i = 0; i < a.str_len; i++) {
        if (a.str_chars[i] != b.str_chars[i]) return false;
    }
    return true;
}

fn bool schema_option_equals(Value* opt, Value* val) {
    if (opt == null || val == null) return opt == val;
    if (opt.tag == INT && val.tag == INT) return opt.int_val == val.int_val;
    if (opt.tag == STRING && val.tag == STRING) return schema_string_equals(opt, val);
    if (opt.tag == SYMBOL && val.tag == SYMBOL) return opt.sym_val == val.sym_val;
    return false;
}

fn bool schema_check_symbol_schema(Value* schema, Value* val, Interp* interp) {
    char[] name = interp.symbols.get_name(schema.sym_val);
    if (name.len == 3 && name[0] == 'a' && name[1] == 'n' && name[2] == 'y') return true;
    if (name.len == 3 && name[0] == 'n' && name[1] == 'i' && name[2] == 'l') return val == null || val.tag == NIL;
    if (name.len == 3 && name[0] == 'i' && name[1] == 'n' && name[2] == 't') return val != null && val.tag == INT;
    if (name.len == 6 && name[0] == 's' && name[1] == 't' && name[2] == 'r') return val != null && val.tag == STRING;
    if (name.len == 6 && name[0] == 'd' && name[1] == 'o' && name[2] == 'u') return val != null && val.tag == DOUBLE;
    if (name.len == 4 && name[0] == 'b' && name[1] == 'o' && name[2] == 'o' && name[3] == 'l') return val != null && val.tag == SYMBOL;

    SymbolId s_string = interp.symbols.intern("string");
    SymbolId s_int = interp.symbols.intern("int");
    SymbolId s_double = interp.symbols.intern("double");
    SymbolId s_bool = interp.symbols.intern("bool");
    SymbolId s_any = interp.symbols.intern("any");
    SymbolId s_nil = interp.symbols.intern("nil");
    if (schema.sym_val == s_string) return val != null && val.tag == STRING;
    if (schema.sym_val == s_int) return val != null && val.tag == INT;
    if (schema.sym_val == s_double) return val != null && val.tag == DOUBLE;
    if (schema.sym_val == s_any) return true;
    if (schema.sym_val == s_nil) return val == null || val.tag == NIL;
    if (schema.sym_val == s_bool) return val != null && val.tag == SYMBOL;
    return false;
}

fn bool schema_check_and_clause(Value* schema, Value* val, Interp* interp) {
    Value* rest = cdr(schema);
    while (is_cons(rest)) {
        if (!schema_check(car(rest), val, interp)) return false;
        rest = cdr(rest);
    }
    return true;
}

fn bool schema_check_or_clause(Value* schema, Value* val, Interp* interp) {
    Value* rest = cdr(schema);
    while (is_cons(rest)) {
        if (schema_check(car(rest), val, interp)) return true;
        rest = cdr(rest);
    }
    return false;
}

fn bool schema_check_numeric_compare(SymbolId tag_sym, Value* schema, Value* val, SchemaTagSet* tags) {
    if (val == null || (val.tag != INT && val.tag != DOUBLE)) return false;
    Value* bound = car(cdr(schema));
    if (bound == null || (bound.tag != INT && bound.tag != DOUBLE)) return false;
    double v = val.tag == INT ? (double)val.int_val : val.double_val;
    double b = bound.tag == INT ? (double)bound.int_val : bound.double_val;
    if (tag_sym == tags.s_gt) return v > b;
    if (tag_sym == tags.s_lt) return v < b;
    if (tag_sym == tags.s_ge) return v >= b;
    return v <= b;
}

fn bool schema_check_equal_clause(Value* schema, Value* val) {
    Value* expected = car(cdr(schema));
    if (expected == null || val == null) return expected == val;
    if (expected.tag == INT && val.tag == INT) return expected.int_val == val.int_val;
    if (expected.tag == STRING && val.tag == STRING) return schema_string_equals(expected, val);
    return false;
}

fn bool schema_check_regex_clause(Value* schema, Value* val, Interp* interp) {
    if (val == null || val.tag != STRING) return false;
    Value* pat = car(cdr(schema));
    if (pat == null || pat.tag != STRING) return false;
    Value*[2] re_args;
    re_args[0] = pat;
    re_args[1] = val;
    Value* result = pika::prim_re_fullmatch(re_args[..], null, interp);
    return result != null && result.tag != NIL;
}

fn bool schema_check_map_clause(Value* schema, Value* val, Interp* interp) {
    if (val == null || val.tag != HASHMAP) return false;
    Value* entries = cdr(schema);
    while (is_cons(entries)) {
        Value* entry = car(entries);
        if (is_cons(entry)) {
            Value* key_sym = car(entry);
            Value* val_schema = car(cdr(entry));
            Value* dict_val = hashmap_get(val.hashmap_val, key_sym);
            if (dict_val == null) return false;
            if (val_schema != null && !schema_check(val_schema, dict_val, interp)) return false;
        }
        entries = cdr(entries);
    }
    return true;
}

fn bool schema_check_vector_of_clause(Value* schema, Value* val, Interp* interp) {
    if (val == null || val.tag != ARRAY) return false;
    Value* elem_schema = car(cdr(schema));
    for (usz i = 0; i < val.array_val.length; i++) {
        if (!schema_check(elem_schema, val.array_val.items[i], interp)) return false;
    }
    return true;
}

fn bool schema_check_tuple_clause(Value* schema, Value* val, Interp* interp) {
    if (val == null || val.tag != ARRAY) return false;
    Value* schemas = cdr(schema);
    usz idx = 0;
    while (is_cons(schemas)) {
        if (idx >= val.array_val.length) return false;
        if (!schema_check(car(schemas), val.array_val.items[idx], interp)) return false;
        schemas = cdr(schemas);
        idx++;
    }
    return idx == val.array_val.length;
}

fn bool schema_check_enum_clause(Value* schema, Value* val) {
    Value* options = cdr(schema);
    while (is_cons(options)) {
        if (schema_option_equals(car(options), val)) return true;
        options = cdr(options);
    }
    return false;
}

// Internal: check if a value satisfies a schema. Returns true/false.
fn bool schema_check(Value* schema, Value* val, Interp* interp) {
    if (schema == null) return true;

    if (schema.tag == SYMBOL) {
        return schema_check_symbol_schema(schema, val, interp);
    }

    if (is_cons(schema)) {
        Value* tag = car(schema);
        if (tag == null || tag.tag != SYMBOL) return false;

        SymbolId tag_sym = tag.sym_val;
        SchemaTagSet tags = schema_make_tag_set(interp);

        if (tag_sym == tags.s_and) return schema_check_and_clause(schema, val, interp);
        if (tag_sym == tags.s_or) return schema_check_or_clause(schema, val, interp);
        if (tag_sym == tags.s_not) return !schema_check(car(cdr(schema)), val, interp);
        if (tag_sym == tags.s_maybe) return (val == null || val.tag == NIL)
            ? true
            : schema_check(car(cdr(schema)), val, interp);
        if (tag_sym == tags.s_gt || tag_sym == tags.s_lt ||
            tag_sym == tags.s_ge || tag_sym == tags.s_le) {
            return schema_check_numeric_compare(tag_sym, schema, val, &tags);
        }
        if (tag_sym == tags.s_eq) return schema_check_equal_clause(schema, val);
        if (tag_sym == tags.s_re) return schema_check_regex_clause(schema, val, interp);
        if (tag_sym == tags.s_map) return schema_check_map_clause(schema, val, interp);
        if (tag_sym == tags.s_vector_of) return schema_check_vector_of_clause(schema, val, interp);
        if (tag_sym == tags.s_tuple) return schema_check_tuple_clause(schema, val, interp);
        if (tag_sym == tags.s_enum) return schema_check_enum_clause(schema, val);
    }

    return false;
}

// ============================================================
// (validate schema-name value) → true or nil
// ============================================================

fn Value* prim_validate(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) return raise_error(interp, "validate: expected (validate schema value)");

    Value* schema = args[0];
    // If schema is a symbol, try looking up as a named schema first
    if (schema.tag == SYMBOL) {
        Value* named = interp.global_env.lookup(schema.sym_val);
        if (named != null && named.tag != NIL) {
            schema = named;  // Found named schema
        }
        // Otherwise use the symbol itself as the schema (bare type: 'int, 'string, etc.)
    }

    Value* val = args[1];

    if (schema_check(schema, val, interp)) {
        return interp.global_env.lookup(interp.symbols.intern("true"));
    }
    return make_nil(interp);
}

// ============================================================
// explain selector surface:
//   (explain 'dispatch <form>)
//   (explain 'effect <form>)
//
// L1.1 lock:
// - selector must be quoted symbol
// - accepted selectors are 'dispatch and 'effect
// - target form is parser-wrapped as a thunk to avoid eager evaluation
// ============================================================

fn Value* schema_resolve_named_symbol(Value* schema, Interp* interp) @inline {
    if (schema == null || schema.tag != SYMBOL) return schema;
    Value* named = interp.global_env.lookup(schema.sym_val);
    if (named != null && named.tag != NIL) {
        return named;
    }
    return schema;
}

fn Value* explain_bool_value(Interp* interp, bool value) @inline {
    return value ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn void explain_dict_set(Value* dict, Interp* interp, char[] key, Value* value) @inline {
    hashmap_set(dict.hashmap_val, make_symbol(interp, interp.symbols.intern(key)), value, interp);
}

fn void explain_dict_set_symbol(Value* dict, Interp* interp, char[] key, SymbolId value) @inline {
    hashmap_set(dict.hashmap_val, make_symbol(interp, interp.symbols.intern(key)), make_symbol(interp, value), interp);
}

fn Value* explain_reverse_list(Value* list, Interp* interp) {
    Value* out = make_nil(interp);
    Value* cur = list;
    while (is_cons(cur)) {
        out = make_cons(interp, cur.cons_val.car, out);
        cur = cur.cons_val.cdr;
    }
    return out;
}

fn SymbolId explain_dispatch_failure_symbol(Interp* interp, DispatchMatchFailureReason reason) {
    switch (reason) {
        case DISPATCH_MATCH_FAIL_NONE:
            return interp.symbols.intern("none");
        case DISPATCH_MATCH_FAIL_ARITY:
            return interp.symbols.intern("arity-mismatch");
        case DISPATCH_MATCH_FAIL_VALUE_LITERAL:
            return interp.symbols.intern("value-literal-mismatch");
        case DISPATCH_MATCH_FAIL_TYPE:
            return interp.symbols.intern("type-mismatch");
        case DISPATCH_MATCH_FAIL_CONSTRAINT:
            return interp.symbols.intern("constraint-mismatch");
    }
}

fn Value* explain_literal_value(Interp* interp, ValueLiteralKey* literal) {
    if (literal == null) return make_nil(interp);
    switch (literal.tag) {
        case NIL:
            if ((uint)literal.sym_value == (uint)interp.sym_false) {
                return make_symbol(interp, interp.sym_false);
            }
            return make_nil(interp);
        case INT:
            return make_int(interp, literal.int_value);
        case SYMBOL:
            return make_symbol(interp, literal.sym_value);
        case STRING: {
            char[] text = interp.symbols.get_name(literal.sym_value);
            return make_string(interp, text);
        }
        default:
            return make_nil(interp);
    }
}

fn SymbolId explain_type_symbol(Interp* interp, TypeId type_id) {
    if (type_id == INVALID_TYPE_ID) return interp.sym_Any;
    TypeInfo* ti = interp.types.get(type_id);
    if (ti != null) return ti.name;
    return interp.sym_Any;
}

fn Value* explain_primitive_name_value(Interp* interp, Primitive* prim) {
    if (prim == null) return make_nil(interp);
    usz len = 0;
    while (len < prim.name.len && prim.name[len] != 0) len++;
    if (len == 0) return make_nil(interp);
    return make_string(interp, prim.name[:len]);
}

fn Value* explain_method_source(Interp* interp, Value* impl) {
    Value* source = make_hashmap(interp, 8);
    if (impl == null) {
        explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("unknown"));
        explain_dict_set(source, interp, "line", make_nil(interp));
        explain_dict_set(source, interp, "column", make_nil(interp));
        return source;
    }

    switch (impl.tag) {
        case CLOSURE: {
            explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("closure"));
            Expr* body = impl.closure_val != null ? impl.closure_val.body : null;
            if (body != null) {
                explain_dict_set(source, interp, "line", make_int(interp, (long)body.loc_line));
                explain_dict_set(source, interp, "column", make_int(interp, (long)body.loc_column));
            } else {
                explain_dict_set(source, interp, "line", make_nil(interp));
                explain_dict_set(source, interp, "column", make_nil(interp));
            }
            return source;
        }
        case PRIMITIVE:
            explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("primitive"));
            explain_dict_set(source, interp, "line", make_nil(interp));
            explain_dict_set(source, interp, "column", make_nil(interp));
            explain_dict_set(source, interp, "name", explain_primitive_name_value(interp, impl.prim_val));
            return source;
        default:
            explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("runtime"));
            explain_dict_set(source, interp, "line", make_nil(interp));
            explain_dict_set(source, interp, "column", make_nil(interp));
            explain_dict_set_symbol(source, interp, "tag", value_type_name(impl, interp));
            return source;
    }
}

fn Value* explain_signature_params(Interp* interp, MethodSignature* sig) {
    if (sig == null || sig.param_count == 0) return make_nil(interp);
    Value* rev = make_nil(interp);
    for (usz i = 0; i < sig.param_count; i++) {
        Value* param = make_hashmap(interp, 8);
        explain_dict_set(param, interp, "index", make_int(interp, (long)i));
        SymbolId type_var = (sig.param_type_vars != null) ? sig.param_type_vars[i] : (SymbolId)0;
        if ((uint)type_var != 0) {
            explain_dict_set(param, interp, "type-var", make_symbol(interp, type_var));
        } else {
            explain_dict_set(param, interp, "type-var", make_nil(interp));
        }
        if (sig.has_val_literal != null && sig.has_val_literal[i]) {
            explain_dict_set_symbol(param, interp, "kind", interp.symbols.intern("value"));
            ValueLiteralKey* literal = (sig.val_literals != null) ? &sig.val_literals[i] : null;
            explain_dict_set(param, interp, "literal", explain_literal_value(interp, literal));
        } else if (sig.param_types != null && sig.param_types[i] != INVALID_TYPE_ID) {
            explain_dict_set_symbol(param, interp, "kind", interp.symbols.intern("type"));
            explain_dict_set(param, interp, "type", make_symbol(interp, explain_type_symbol(interp, sig.param_types[i])));
        } else {
            explain_dict_set_symbol(param, interp, "kind", interp.symbols.intern("any"));
        }
        rev = make_cons(interp, param, rev);
    }
    return explain_reverse_list(rev, interp);
}

fn Value* explain_constraints(Interp* interp, MethodSignature* sig) {
    if (sig == null || sig.constraint_count == 0 || sig.constraints == null) return make_nil(interp);
    Value* rev = make_nil(interp);
    for (usz i = 0; i < sig.constraint_count; i++) {
        Value* c = make_hashmap(interp, 4);
        explain_dict_set(c, interp, "param", make_symbol(interp, sig.constraints[i].param_sym));
        explain_dict_set(c, interp, "bound", make_symbol(interp, explain_type_symbol(interp, sig.constraints[i].bound_type)));
        rev = make_cons(interp, c, rev);
    }
    return explain_reverse_list(rev, interp);
}

fn Value* explain_score_components(Interp* interp, DispatchMatchBreakdown* breakdown) {
    Value* score = make_hashmap(interp, 12);
    explain_dict_set(score, interp, "value", make_int(interp, breakdown.score_value_literal));
    explain_dict_set(score, interp, "exact", make_int(interp, breakdown.score_exact));
    explain_dict_set(score, interp, "widen", make_int(interp, breakdown.score_widen));
    explain_dict_set(score, interp, "subtype", make_int(interp, breakdown.score_subtype));
    explain_dict_set(score, interp, "any", make_int(interp, breakdown.score_any));
    explain_dict_set(score, interp, "total", make_int(interp, breakdown.score_total));
    explain_dict_set(score, interp, "constraints-ok",
        explain_bool_value(interp, breakdown.failure_reason != DISPATCH_MATCH_FAIL_CONSTRAINT));
    return score;
}

fn Value* explain_arg_types(Interp* interp, TypeId[] arg_types) {
    Value* rev = make_nil(interp);
    for (usz i = 0; i < arg_types.len; i++) {
        rev = make_cons(interp, make_symbol(interp, explain_type_symbol(interp, arg_types[i])), rev);
    }
    return explain_reverse_list(rev, interp);
}

fn SymbolId explain_effect_miss_reason_symbol(Interp* interp, bool dispatchable, bool matched, bool strict_mode) {
    if (dispatchable) return interp.symbols.intern("none");
    if (matched) return interp.symbols.intern("handler-inactive");
    if (strict_mode) return interp.symbols.intern("strict-boundary");
    return interp.symbols.intern("tag-miss");
}

fn SymbolId explain_effect_clause_tag(EffectHandler* handler, usz clause_index) {
    if (handler.clauses != null) {
        return handler.clauses[clause_index].effect_tag;
    }
    if (handler.tags != null) {
        return handler.tags[clause_index];
    }
    return 0;
}

fn Expr* explain_effect_clause_body(EffectHandler* handler, usz clause_index) {
    if (handler.clauses != null) {
        return handler.clauses[clause_index].handler_body;
    }
    if (handler.closures != null &&
        handler.closures[clause_index] != null &&
        handler.closures[clause_index].tag == CLOSURE) {
        return handler.closures[clause_index].closure_val.body;
    }
    return null;
}

fn SymbolId explain_effect_clause_k_name(EffectHandler* handler, usz clause_index, Interp* interp) {
    if (handler.clauses != null) {
        return handler.clauses[clause_index].k_name;
    }
    return interp.sym__k;
}

fn SymbolId explain_effect_clause_arg_name(EffectHandler* handler, usz clause_index) {
    if (handler.clauses != null) {
        return handler.clauses[clause_index].arg_name;
    }
    return 0;
}

fn isz explain_effect_find_clause_index(EffectHandler* handler, SymbolId effect_tag) {
    if (handler == null) return -1;
    for (usz j = 0; j < handler.clause_count; j++) {
        SymbolId clause_tag = explain_effect_clause_tag(handler, j);
        if ((uint)clause_tag == (uint)effect_tag) return (isz)j;
    }
    return -1;
}

fn Value* explain_effect_handler_tags(Interp* interp, EffectHandler* handler) {
    if (handler == null || handler.clause_count == 0) return make_nil(interp);
    Value* rev = make_nil(interp);
    for (usz j = 0; j < handler.clause_count; j++) {
        SymbolId tag = explain_effect_clause_tag(handler, j);
        if ((uint)tag == 0) continue;
        rev = make_cons(interp, make_symbol(interp, tag), rev);
    }
    return explain_reverse_list(rev, interp);
}

fn bool explain_expr_contains_resolve(Expr* expr) {
    if (expr == null) return false;
    if (expr.tag == E_RESOLVE) return true;
    switch (expr.tag) {
        case E_IF:
            return explain_expr_contains_resolve(expr.if_expr.test) ||
                explain_expr_contains_resolve(expr.if_expr.then_branch) ||
                explain_expr_contains_resolve(expr.if_expr.else_branch);
        case E_LET:
            return explain_expr_contains_resolve(expr.let_expr.init) ||
                explain_expr_contains_resolve(expr.let_expr.body);
        case E_LAMBDA:
            // Resolve inside nested lambdas is not guaranteed on this signal path.
            return false;
        case E_BLOCK:
            for (usz i = 0; i < expr.block.expr_count; i++) {
                if (explain_expr_contains_resolve(expr.block.exprs[i])) return true;
            }
            return false;
        case E_CALL:
            if (explain_expr_contains_resolve(expr.call.func)) return true;
            for (usz i = 0; i < expr.call.arg_count; i++) {
                if (explain_expr_contains_resolve(expr.call.args[i])) return true;
            }
            return false;
        case E_APP:
            return explain_expr_contains_resolve(expr.app.func) ||
                explain_expr_contains_resolve(expr.app.arg);
        case E_MATCH:
            if (explain_expr_contains_resolve(expr.match.scrutinee)) return true;
            for (usz i = 0; i < expr.match.clause_count; i++) {
                if (explain_expr_contains_resolve(expr.match.clauses[i].result)) return true;
            }
            return false;
        case E_AND:
            return explain_expr_contains_resolve(expr.and_expr.left) ||
                explain_expr_contains_resolve(expr.and_expr.right);
        case E_OR:
            return explain_expr_contains_resolve(expr.or_expr.left) ||
                explain_expr_contains_resolve(expr.or_expr.right);
        case E_SET:
            return explain_expr_contains_resolve(expr.set_expr.value);
        case E_DEFINE:
            return explain_expr_contains_resolve(expr.define.value);
        case E_PERFORM:
            return explain_expr_contains_resolve(expr.perform.arg);
        case E_HANDLE:
            // Handle clause executes in its own runtime loop.
            return false;
        case E_RESET:
            return explain_expr_contains_resolve(expr.reset.body);
        case E_SHIFT:
            return explain_expr_contains_resolve(expr.shift.body);
        default:
            return false;
    }
}

fn SymbolId explain_effect_handler_outcome_symbol(Expr* clause_body, Interp* interp) {
    if (clause_body == null) return interp.symbols.intern("unknown");
    if (explain_expr_contains_resolve(clause_body)) return interp.sym_resolve;
    return interp.symbols.intern("abort");
}

fn Value* explain_effect_clause_source(Interp* interp, Expr* clause_body) {
    Value* source = make_hashmap(interp, 6);
    if (clause_body == null) {
        explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("unknown"));
        explain_dict_set(source, interp, "line", make_nil(interp));
        explain_dict_set(source, interp, "column", make_nil(interp));
        return source;
    }
    explain_dict_set_symbol(source, interp, "kind", interp.symbols.intern("clause"));
    explain_dict_set(source, interp, "line", make_int(interp, (long)clause_body.loc_line));
    explain_dict_set(source, interp, "column", make_int(interp, (long)clause_body.loc_column));
    return source;
}

fn Value* explain_effect_fast_path_primitive(Interp* interp, SymbolId effect_tag, usz* table_index_out) {
    *table_index_out = usz.max;
    for (usz i = 0; i < interp.fast_path_count; i++) {
        if ((uint)interp.fast_path_table[i].tag != (uint)effect_tag) continue;
        *table_index_out = i;
        return interp.fast_path_table[i].prim;
    }
    return null;
}

fn Value* explain_effect_continuation_lookup(Env* target_env, Interp* interp) {
    Value* k_val = null;
    if (target_env != null) {
        k_val = target_env.lookup(interp.sym__k);
    }
    if (k_val == null || k_val.tag == NIL) {
        k_val = interp.global_env.lookup(interp.sym__k);
    }
    return k_val;
}

fn void explain_init_common_output(
    Interp* interp,
    Value* out,
    SymbolId kind,
    Value* input,
    Value* decision,
    Value* trace
) {
    explain_dict_set(out, interp, "kind", make_symbol(interp, kind));
    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("pending"));
    explain_dict_set(out, interp, "input", input);
    explain_dict_set(out, interp, "decision", decision);
    explain_dict_set(out, interp, "candidates", make_nil(interp));
    explain_dict_set(out, interp, "trace", trace);
    explain_dict_set(out, interp, "debug_message", make_nil(interp));
}

fn void explain_init_dispatch_decision_shape(Interp* interp, Value* decision) {
    explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("pending"));
    explain_dict_set(decision, interp, "winner-index", make_nil(interp));
    explain_dict_set(decision, interp, "best-score", make_nil(interp));
    explain_dict_set(decision, interp, "tie-count", make_nil(interp));
    explain_dict_set(decision, interp, "fallback-source", make_nil(interp));
    explain_dict_set(decision, interp, "outcome", make_nil(interp));
    explain_dict_set(decision, interp, "debug_message", make_nil(interp));
}

fn void explain_init_dispatch_trace_shape(Interp* interp, Value* trace) {
    explain_dict_set(trace, interp, "arg-types", make_nil(interp));
    explain_dict_set(trace, interp, "arg-count", make_nil(interp));
    explain_dict_set(trace, interp, "target-type", make_nil(interp));
    explain_dict_set(trace, interp, "method-name", make_nil(interp));
    explain_dict_set(trace, interp, "method-count", make_nil(interp));
    explain_dict_set(trace, interp, "debug_message", make_nil(interp));
}

fn void explain_init_effect_decision_shape(Interp* interp, Value* decision) {
    explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("pending"));
    explain_dict_set(decision, interp, "outcome", make_nil(interp));
    explain_dict_set(decision, interp, "handler-index", make_nil(interp));
    explain_dict_set(decision, interp, "search-order", make_nil(interp));
    explain_dict_set(decision, interp, "clause-index", make_nil(interp));
    explain_dict_set(decision, interp, "match-tag", make_nil(interp));
    explain_dict_set(decision, interp, "strict-handler-index", make_nil(interp));
    explain_dict_set(decision, interp, "fast-path-index", make_nil(interp));
    explain_dict_set(decision, interp, "fast-path-primitive", make_nil(interp));
    explain_dict_set(decision, interp, "arg-name", make_nil(interp));
    explain_dict_set(decision, interp, "continuation-name", make_nil(interp));
    explain_dict_set(decision, interp, "debug_message", make_nil(interp));
}

fn void explain_init_effect_trace_shape(Interp* interp, Value* trace) {
    explain_dict_set(trace, interp, "effect-tag", make_nil(interp));
    explain_dict_set(trace, interp, "arg-type", make_nil(interp));
    explain_dict_set(trace, interp, "handler-count", make_nil(interp));
    explain_dict_set(trace, interp, "searched-handlers", make_nil(interp));
    explain_dict_set(trace, interp, "stack-context-active", make_nil(interp));
    explain_dict_set(trace, interp, "has-continuation-binding", make_nil(interp));
    explain_dict_set(trace, interp, "effect-continuation", make_nil(interp));
    explain_dict_set(trace, interp, "debug_message", make_nil(interp));
}

fn bool explain_eval_call_target(
    Expr* target,
    Env* target_env,
    Interp* interp,
    Value** callee_out,
    Value*[]* args_out,
    Value*** args_storage_out
) {
    *callee_out = jit_eval(target.call.func, target_env, interp);
    if (*callee_out != null && (*callee_out).tag == ERROR) return false;

    usz arg_count = target.call.arg_count;
    *args_out = {};
    *args_storage_out = null;
    if (arg_count > 0) {
        Value** storage = (Value**)mem::malloc(Value*.sizeof * arg_count);
        if (storage == null) {
            *callee_out = raise_error(interp, "explain dispatch: out of memory");
            return false;
        }
        *args_storage_out = storage;
        *args_out = storage[:arg_count];
        for (usz i = 0; i < arg_count; i++) {
            (*args_out)[i] = jit_eval(target.call.args[i], target_env, interp);
            if ((*args_out)[i] != null && (*args_out)[i].tag == ERROR) {
                *callee_out = (*args_out)[i];
                return false;
            }
        }
    }
    return true;
}

fn Value* explain_dispatch_result_from_thunk(Interp* interp, Value* thunk_value) {
    if (thunk_value == null || thunk_value.tag != CLOSURE || thunk_value.closure_val == null) {
        return raise_error(interp, "explain dispatch: expected thunked form");
    }
    Closure* thunk = thunk_value.closure_val;
    if (thunk.param_count != 0 || thunk.has_rest) {
        return raise_error(interp, "explain dispatch: expected zero-arg thunk");
    }

    Expr* target = thunk.body;
    Env* target_env = thunk.env != null ? thunk.env : interp.global_env;
    Value* input_form = expr_to_value(target, interp);

    Value* out = make_hashmap(interp, 16);
    Value* decision = make_hashmap(interp, 16);
    Value* trace = make_hashmap(interp, 16);
    explain_init_common_output(interp, out, interp.symbols.intern("dispatch"), input_form, decision, trace);
    explain_init_dispatch_decision_shape(interp, decision);
    explain_init_dispatch_trace_shape(interp, trace);

    if (target == null || target.tag != E_CALL) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("unsupported-form"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("dispatch-target-not-call"));
        return out;
    }

    Value* callee = null;
    Value*[] call_args = {};
    Value** call_args_storage = null;
    bool evaluated = explain_eval_call_target(target, target_env, interp, &callee, &call_args, &call_args_storage);
    defer if (call_args_storage != null) mem::free(call_args_storage);
    if (!evaluated) return callee;

    TypeId[] arg_types = {};
    TypeId* arg_types_storage = null;
    if (call_args.len > 0) {
        arg_types_storage = (TypeId*)mem::malloc(TypeId.sizeof * call_args.len);
        if (arg_types_storage == null) {
            return raise_error(interp, "explain dispatch: out of memory");
        }
        arg_types = arg_types_storage[:call_args.len];
        for (usz i = 0; i < call_args.len; i++) {
            arg_types[i] = infer_value_type(call_args[i], interp);
        }
    }
    defer if (arg_types_storage != null) mem::free(arg_types_storage);

    explain_dict_set(trace, interp, "arg-types", explain_arg_types(interp, arg_types));
    explain_dict_set(trace, interp, "arg-count", make_int(interp, (long)call_args.len));
    explain_dict_set(trace, interp, "target-type", make_symbol(interp, value_type_name(callee, interp)));

    if (callee == null || callee.tag != METHOD_TABLE || callee.method_table_val == null) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("unsupported-form"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("dispatch-target-not-method-table"));
        return out;
    }

    MethodTable* mt = callee.method_table_val;
    explain_dict_set(trace, interp, "method-name", make_symbol(interp, mt.name));
    explain_dict_set(trace, interp, "method-count", make_int(interp, (long)mt.entry_count));

    long best_score = -1;
    isz winner_index = -1;
    usz tie_count = 0;
    Value* rev_candidates = make_nil(interp);

    for (usz i = 0; i < mt.entry_count; i++) {
        MethodEntry* entry = &mt.entries[i];
        DispatchMatchBreakdown breakdown = dispatch_match_breakdown(entry, call_args, arg_types, interp);

        Value* candidate = make_hashmap(interp, 24);
        explain_dict_set(candidate, interp, "index", make_int(interp, (long)i));
        explain_dict_set(candidate, interp, "name", make_symbol(interp, mt.name));
        explain_dict_set(candidate, interp, "signature", explain_signature_params(interp, &entry.sig));
        explain_dict_set(candidate, interp, "constraints", explain_constraints(interp, &entry.sig));
        explain_dict_set(candidate, interp, "source", explain_method_source(interp, entry.implementation));
        explain_dict_set(candidate, interp, "applicable", explain_bool_value(interp, breakdown.matched));
        explain_dict_set(candidate, interp, "score-components", explain_score_components(interp, &breakdown));
        explain_dict_set_symbol(candidate, interp, "failure",
            explain_dispatch_failure_symbol(interp, breakdown.failure_reason));
        explain_dict_set(candidate, interp, "debug_message", make_nil(interp));
        if (breakdown.failure_arg_index != usz.max) {
            explain_dict_set(candidate, interp, "failure-arg-index", make_int(interp, (long)breakdown.failure_arg_index));
        } else {
            explain_dict_set(candidate, interp, "failure-arg-index", make_nil(interp));
        }

        if (breakdown.matched) {
            if (breakdown.score_total > best_score) {
                best_score = breakdown.score_total;
                winner_index = (isz)i;
                tie_count = 1;
            } else if (breakdown.score_total == best_score) {
                tie_count++;
            }
        }

        rev_candidates = make_cons(interp, candidate, rev_candidates);
    }

    Value* candidates = explain_reverse_list(rev_candidates, interp);
    explain_dict_set(out, interp, "candidates", candidates);

    if (winner_index >= 0 && tie_count == 1) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("method-match"));
        explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("match"));
        explain_dict_set(decision, interp, "winner-index", make_int(interp, (long)winner_index));
        explain_dict_set(decision, interp, "best-score", make_int(interp, best_score));
        explain_dict_set(decision, interp, "tie-count", make_int(interp, (long)tie_count));
    } else if (winner_index >= 0 && tie_count > 1) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ambiguous"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("ambiguous-equal-specificity"));
        explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"));
        explain_dict_set(decision, interp, "winner-index", make_nil(interp));
        explain_dict_set(decision, interp, "best-score", make_int(interp, best_score));
        explain_dict_set(decision, interp, "tie-count", make_int(interp, (long)tie_count));
    } else if (mt.fallback != null) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("fallback"));
        explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("fallback"));
        explain_dict_set(decision, interp, "winner-index", make_nil(interp));
        explain_dict_set(decision, interp, "best-score", make_nil(interp));
        explain_dict_set(decision, interp, "tie-count", make_int(interp, 0));
        explain_dict_set(decision, interp, "fallback-source", explain_method_source(interp, mt.fallback));
    } else {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("no-match"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("no-matching-method"));
        explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"));
        explain_dict_set(decision, interp, "winner-index", make_nil(interp));
        explain_dict_set(decision, interp, "best-score", make_nil(interp));
        explain_dict_set(decision, interp, "tie-count", make_int(interp, 0));
    }

    return out;
}

fn Value* explain_effect_result_from_signal(Interp* interp, Expr* target, Env* target_env, Value* out, Value* decision, Value* trace) {
    Value* arg_value = jit_eval(target.perform.arg, target_env, interp);
    if (arg_value != null && arg_value.tag == ERROR) return arg_value;

    SymbolId effect_tag = target.perform.tag;
    TypeId arg_type = infer_value_type(arg_value, interp);
    bool has_stack_ctx = main::g_current_stack_ctx != null;

    explain_dict_set(trace, interp, "effect-tag", make_symbol(interp, effect_tag));
    explain_dict_set(trace, interp, "arg-type", make_symbol(interp, explain_type_symbol(interp, arg_type)));
    explain_dict_set(trace, interp, "handler-count", make_int(interp, (long)interp.handler_count));
    explain_dict_set(trace, interp, "stack-context-active", explain_bool_value(interp, has_stack_ctx));

    Value* rev_candidates = make_nil(interp);
    usz searched_handlers = 0;

    bool matched = false;
    usz matched_handler_index = usz.max;
    usz matched_search_order = usz.max;
    usz matched_clause_index = usz.max;
    Expr* matched_clause_body = null;
    SymbolId matched_clause_arg = 0;
    SymbolId matched_clause_k = 0;

    bool strict_stop = false;
    usz strict_handler_index = usz.max;
    usz search_order = 0;

    for (isz i = (isz)interp.handler_count - 1; i >= 0; i--) {
        EffectHandler* handler = &interp.handler_stack[(usz)i];
        isz clause_idx = explain_effect_find_clause_index(handler, effect_tag);
        bool clause_match = clause_idx >= 0;
        usz clause_index = clause_match ? (usz)clause_idx : usz.max;
        bool dispatchable = clause_match && has_stack_ctx && handler.effect_state != null;

        Value* candidate = make_hashmap(interp, 20);
        explain_dict_set(candidate, interp, "index", make_int(interp, (long)search_order));
        explain_dict_set(candidate, interp, "handler-index", make_int(interp, (long)i));
        explain_dict_set(candidate, interp, "strict", explain_bool_value(interp, handler.strict_mode));
        explain_dict_set(candidate, interp, "clause-count", make_int(interp, (long)handler.clause_count));
        explain_dict_set(candidate, interp, "tags", explain_effect_handler_tags(interp, handler));
        explain_dict_set(candidate, interp, "matched", explain_bool_value(interp, clause_match));
        explain_dict_set(candidate, interp, "dispatchable", explain_bool_value(interp, dispatchable));
        explain_dict_set_symbol(candidate, interp, "miss-reason",
            explain_effect_miss_reason_symbol(interp, dispatchable, clause_match, handler.strict_mode));
        explain_dict_set(candidate, interp, "debug_message", make_nil(interp));
        if (clause_match) {
            Expr* clause_body = explain_effect_clause_body(handler, clause_index);
            explain_dict_set(candidate, interp, "match-clause-index", make_int(interp, (long)clause_index));
            explain_dict_set(candidate, interp, "match-tag", make_symbol(interp, explain_effect_clause_tag(handler, clause_index)));
            explain_dict_set(candidate, interp, "source", explain_effect_clause_source(interp, clause_body));
            explain_dict_set_symbol(candidate, interp, "outcome",
                explain_effect_handler_outcome_symbol(clause_body, interp));

            SymbolId arg_name = explain_effect_clause_arg_name(handler, clause_index);
            SymbolId k_name = explain_effect_clause_k_name(handler, clause_index, interp);
            if ((uint)arg_name != 0) {
                explain_dict_set(candidate, interp, "arg-name", make_symbol(interp, arg_name));
            } else {
                explain_dict_set(candidate, interp, "arg-name", make_nil(interp));
            }
            if ((uint)k_name != 0) {
                explain_dict_set(candidate, interp, "continuation-name", make_symbol(interp, k_name));
            } else {
                explain_dict_set(candidate, interp, "continuation-name", make_nil(interp));
            }
        } else {
            explain_dict_set(candidate, interp, "match-clause-index", make_nil(interp));
            explain_dict_set(candidate, interp, "match-tag", make_nil(interp));
            explain_dict_set(candidate, interp, "source", make_nil(interp));
            explain_dict_set(candidate, interp, "outcome", make_nil(interp));
            explain_dict_set(candidate, interp, "arg-name", make_nil(interp));
            explain_dict_set(candidate, interp, "continuation-name", make_nil(interp));
        }

        rev_candidates = make_cons(interp, candidate, rev_candidates);
        searched_handlers++;

        if (dispatchable) {
            matched = true;
            matched_handler_index = (usz)i;
            matched_search_order = search_order;
            matched_clause_index = clause_index;
            matched_clause_body = explain_effect_clause_body(handler, clause_index);
            matched_clause_arg = explain_effect_clause_arg_name(handler, clause_index);
            matched_clause_k = explain_effect_clause_k_name(handler, clause_index, interp);
            break;
        }

        if (handler.strict_mode) {
            strict_stop = true;
            strict_handler_index = (usz)i;
            break;
        }

        search_order++;
    }

    Value* candidates = explain_reverse_list(rev_candidates, interp);
    explain_dict_set(out, interp, "candidates", candidates);
    explain_dict_set(trace, interp, "searched-handlers", make_int(interp, (long)searched_handlers));

    if (matched) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("handler-match"));
        explain_dict_set(decision, interp, "handler-index", make_int(interp, (long)matched_handler_index));
        explain_dict_set(decision, interp, "search-order", make_int(interp, (long)matched_search_order));
        explain_dict_set(decision, interp, "clause-index", make_int(interp, (long)matched_clause_index));
        explain_dict_set(decision, interp, "match-tag", make_symbol(interp, effect_tag));
        explain_dict_set_symbol(decision, interp, "outcome",
            explain_effect_handler_outcome_symbol(matched_clause_body, interp));
        if ((uint)matched_clause_arg != 0) {
            explain_dict_set(decision, interp, "arg-name", make_symbol(interp, matched_clause_arg));
        } else {
            explain_dict_set(decision, interp, "arg-name", make_nil(interp));
        }
        if ((uint)matched_clause_k != 0) {
            explain_dict_set(decision, interp, "continuation-name", make_symbol(interp, matched_clause_k));
        } else {
            explain_dict_set(decision, interp, "continuation-name", make_nil(interp));
        }
        return out;
    }

    if (strict_stop) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("unhandled"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("strict-unhandled-effect"));
        explain_dict_set(decision, interp, "strict-handler-index", make_int(interp, (long)strict_handler_index));
        explain_dict_set(decision, interp, "match-tag", make_symbol(interp, effect_tag));
        explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"));
        return out;
    }

    usz fast_path_index = usz.max;
    Value* fast_path_prim = explain_effect_fast_path_primitive(interp, effect_tag, &fast_path_index);
    if (fast_path_prim != null) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("fast-path"));
        explain_dict_set(decision, interp, "fast-path-index", make_int(interp, (long)fast_path_index));
        if (fast_path_prim.tag == PRIMITIVE) {
            explain_dict_set(decision, interp, "fast-path-primitive", explain_primitive_name_value(interp, fast_path_prim.prim_val));
        } else {
            explain_dict_set(decision, interp, "fast-path-primitive", make_nil(interp));
        }
        explain_dict_set(decision, interp, "match-tag", make_symbol(interp, effect_tag));
        explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"));
        return out;
    }

    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("unhandled"));
    explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("unhandled-effect"));
    explain_dict_set(decision, interp, "match-tag", make_symbol(interp, effect_tag));
    explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"));
    return out;
}

fn Value* explain_effect_result_from_resolve(Interp* interp, Env* target_env, Value* out, Value* decision, Value* trace) {
    Value* k_val = explain_effect_continuation_lookup(target_env, interp);
    explain_dict_set(trace, interp, "has-continuation-binding", explain_bool_value(interp, k_val != null));

    if (k_val == null || k_val.tag != CONTINUATION) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("invalid"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("resolve-invalid-continuation"));
        explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"));
        return out;
    }

    Continuation* k = k_val.cont_val;
    bool effect_cont = k.is_coroutine_based && k.handle_state != null;
    explain_dict_set(trace, interp, "effect-continuation", explain_bool_value(interp, effect_cont));

    if (effect_cont) {
        if (k.ctx == null || k.ctx.status == main::StackCtxStatus.CTX_DEAD || k.ctx.status == main::StackCtxStatus.CTX_COMPLETED) {
            explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("invalid"));
            explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("resolve-dead-continuation"));
            explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"));
            return out;
        }
        if (k.ctx.status != main::StackCtxStatus.CTX_SUSPENDED) {
            explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("invalid"));
            explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("resolve-continuation-not-suspended"));
            explain_dict_set_symbol(decision, interp, "outcome", interp.symbols.intern("abort"));
            return out;
        }
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("resolve-effect-continuation"));
        explain_dict_set_symbol(decision, interp, "outcome", interp.sym_resolve);
        return out;
    }

    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
    explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("resolve-generic-continuation"));
    explain_dict_set_symbol(decision, interp, "outcome", interp.sym_resolve);
    return out;
}

fn Value* explain_effect_result_from_thunk(Interp* interp, Value* thunk_value) {
    if (thunk_value == null || thunk_value.tag != CLOSURE || thunk_value.closure_val == null) {
        return raise_error(interp, "explain effect: expected thunked form");
    }
    Closure* thunk = thunk_value.closure_val;
    if (thunk.param_count != 0 || thunk.has_rest) {
        return raise_error(interp, "explain effect: expected zero-arg thunk");
    }

    Expr* target = thunk.body;
    Env* target_env = thunk.env != null ? thunk.env : interp.global_env;
    Value* input_form = expr_to_value(target, interp);

    Value* out = make_hashmap(interp, 16);
    Value* decision = make_hashmap(interp, 16);
    Value* trace = make_hashmap(interp, 16);
    explain_init_common_output(interp, out, interp.sym_effect, input_form, decision, trace);
    explain_init_effect_decision_shape(interp, decision);
    explain_init_effect_trace_shape(interp, trace);

    if (target == null) {
        explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("unsupported-form"));
        explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("effect-target-null"));
        return out;
    }

    if (target.tag == E_PERFORM) {
        Value* result = explain_effect_result_from_signal(interp, target, target_env, out, decision, trace);
        if (result != null && result.tag == ERROR) return result;
        return out;
    }

    if (target.tag == E_RESOLVE) {
        return explain_effect_result_from_resolve(interp, target_env, out, decision, trace);
    }

    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("unsupported-form"));
    explain_dict_set_symbol(decision, interp, "reason", interp.symbols.intern("effect-target-not-signal-or-resolve"));
    return out;
}

fn Value* prim_explain(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) {
        return raise_error(interp, "explain: expected (explain 'dispatch <form>) or (explain 'effect <form>)");
    }

    Value* selector = args[0];
    if (selector == null || selector.tag != SYMBOL) {
        return raise_error(interp, "explain: selector must be symbol ('dispatch or 'effect)");
    }

    SymbolId dispatch_sym = interp.symbols.intern("dispatch");
    SymbolId effect_sym = interp.sym_effect;
    if ((uint)selector.sym_val != (uint)dispatch_sym && (uint)selector.sym_val != (uint)effect_sym) {
        return raise_error(interp, "explain: unknown selector; expected 'dispatch or 'effect");
    }

    if ((uint)selector.sym_val == (uint)dispatch_sym) {
        return explain_dispatch_result_from_thunk(interp, args[1]);
    }

    return explain_effect_result_from_thunk(interp, args[1]);
}

// ============================================================
// Legacy schema explanation kept under explicit name:
// (schema-explain schema-name value) → list of (path message) or nil if valid
// ============================================================

fn Value* prim_schema_explain(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2) {
        return raise_error_with_payload_names(
            interp,
            "schema",
            "schema/schema-explain-expected-schema-value",
            "schema-explain: expected (schema-explain schema value)"
        );
    }

    Value* schema = schema_resolve_named_symbol(args[0], interp);

    Value* val = args[1];

    if (schema_check(schema, val, interp)) {
        return make_nil(interp);  // valid — no errors
    }

    // Simple explanation: return schema that failed
    Value* msg = make_string(interp, "validation failed");
    return make_cons(interp, msg, make_nil(interp));
}
