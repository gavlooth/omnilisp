module lisp;

import pika;

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
        return make_nil(interp);
    }

    Value* msg = make_string(interp, "validation failed");
    return make_cons(interp, msg, make_nil(interp));
}
