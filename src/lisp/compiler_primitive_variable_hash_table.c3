module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 1b: PRIMITIVE VARIABLE HASH TABLE
// =============================================================================

const usz PRIM_HASH_SIZE = 256;  // Power of 2, > 2*87
struct PrimHashEntry {
    SymbolId key;
    char[]   emit;
}

// Module-level hash table for primitive variable → C3 code mapping
PrimHashEntry[PRIM_HASH_SIZE] g_prim_hash;
bool g_prim_hash_initialized = false;

fn void prim_hash_insert(SymbolId key, char[] emit) {
    usz idx = ((usz)key) & (PRIM_HASH_SIZE - 1);
    for (usz i = 0; i < PRIM_HASH_SIZE; i++) {
        usz slot = (idx + i) & (PRIM_HASH_SIZE - 1);
        if ((uint)g_prim_hash[slot].key == 0xFFFFFFFF) {
            g_prim_hash[slot].key = key;
            g_prim_hash[slot].emit = emit;
            return;
        }
    }
}

fn char[] prim_hash_lookup(SymbolId key) {
    usz idx = ((usz)key) & (PRIM_HASH_SIZE - 1);
    for (usz i = 0; i < PRIM_HASH_SIZE; i++) {
        usz slot = (idx + i) & (PRIM_HASH_SIZE - 1);
        if ((uint)g_prim_hash[slot].key == 0xFFFFFFFF) return "";
        if ((uint)g_prim_hash[slot].key == (uint)key) return g_prim_hash[slot].emit;
    }
    return "";
}

fn void init_prim_hash_arithmetic_and_comparison(SymbolTable* st) {
    // All primitives looked up by name at runtime via aot::lookup_prim
    // Arithmetic
    prim_hash_insert(st.intern("+"), "aot::lookup_prim(\"+\")");
    prim_hash_insert(st.intern("-"), "aot::lookup_prim(\"-\")");
    prim_hash_insert(st.intern("*"), "aot::lookup_prim(\"*\")");
    prim_hash_insert(st.intern("/"), "aot::lookup_prim(\"/\")");
    prim_hash_insert(st.intern("%"), "aot::lookup_prim(\"%\")");

    // Comparison
    prim_hash_insert(st.intern("="), "aot::lookup_prim(\"=\")");
    prim_hash_insert(st.intern("<"), "aot::lookup_prim(\"<\")");
    prim_hash_insert(st.intern(">"), "aot::lookup_prim(\">\")");
    prim_hash_insert(st.intern("<="), "aot::lookup_prim(\"<=\")");
    prim_hash_insert(st.intern(">="), "aot::lookup_prim(\">=\")");
}

fn void init_prim_hash_core_and_strings(SymbolTable* st) {
    // List ops
    prim_hash_insert(st.intern("cons"), "aot::lookup_prim(\"cons\")");
    prim_hash_insert(st.intern("car"), "aot::lookup_prim(\"car\")");
    prim_hash_insert(st.intern("cdr"), "aot::lookup_prim(\"cdr\")");
    prim_hash_insert(st.intern("null?"), "aot::lookup_prim(\"null?\")");
    prim_hash_insert(st.intern("pair?"), "aot::lookup_prim(\"pair?\")");
    prim_hash_insert(st.intern("list"), "aot::lookup_prim(\"list\")");
    prim_hash_insert(st.intern("length"), "aot::lookup_prim(\"length\")");
    prim_hash_insert(st.intern("not"), "aot::lookup_prim(\"not\")");

    // I/O
    prim_hash_insert(st.intern("print"), "aot::lookup_prim(\"print\")");
    prim_hash_insert(st.intern("println"), "aot::lookup_prim(\"println\")");
    prim_hash_insert(st.intern("newline"), "aot::lookup_prim(\"newline\")");
    prim_hash_insert(st.intern("display"), "aot::lookup_prim(\"display\")");

    // String ops
    prim_hash_insert(st.intern("string-append"), "aot::lookup_prim(\"string-append\")");
    prim_hash_insert(st.intern("string-join"), "aot::lookup_prim(\"string-join\")");
    prim_hash_insert(st.intern("substring"), "aot::lookup_prim(\"substring\")");
    prim_hash_insert(st.intern("string-split"), "aot::lookup_prim(\"string-split\")");
    prim_hash_insert(st.intern("string-length"), "aot::lookup_prim(\"string-length\")");
    prim_hash_insert(st.intern("string->list"), "aot::lookup_prim(\"string->list\")");
    prim_hash_insert(st.intern("list->string"), "aot::lookup_prim(\"list->string\")");
    prim_hash_insert(st.intern("string-upcase"), "aot::lookup_prim(\"string-upcase\")");
    prim_hash_insert(st.intern("string-downcase"), "aot::lookup_prim(\"string-downcase\")");
    prim_hash_insert(st.intern("string-casefold"), "aot::lookup_prim(\"string-casefold\")");
    prim_hash_insert(st.intern("string-titlecase"), "aot::lookup_prim(\"string-titlecase\")");
    prim_hash_insert(st.intern("string-trim"), "aot::lookup_prim(\"string-trim\")");
    prim_hash_insert(st.intern("string->number"), "aot::lookup_prim(\"string->number\")");
    prim_hash_insert(st.intern("number->string"), "aot::lookup_prim(\"number->string\")");

    // Type predicates
    prim_hash_insert(st.intern("string?"), "aot::lookup_prim(\"string?\")");
    prim_hash_insert(st.intern("int?"), "aot::lookup_prim(\"int?\")");
    prim_hash_insert(st.intern("symbol?"), "aot::lookup_prim(\"symbol?\")");
    prim_hash_insert(st.intern("closure?"), "aot::lookup_prim(\"closure?\")");
    prim_hash_insert(st.intern("continuation?"), "aot::lookup_prim(\"continuation?\")");
    prim_hash_insert(st.intern("double?"), "aot::lookup_prim(\"double?\")");
    prim_hash_insert(st.intern("list?"), "aot::lookup_prim(\"list?\")");
    prim_hash_insert(st.intern("boolean?"), "aot::lookup_prim(\"boolean?\")");
    prim_hash_insert(st.intern("number?"), "aot::lookup_prim(\"number?\")");
    prim_hash_insert(st.intern("equal?"), "aot::lookup_prim(\"equal?\")");
    prim_hash_insert(st.intern("type-of"), "aot::lookup_prim(\"type-of\")");
}

fn void init_prim_hash_files_and_misc(SymbolTable* st) {
    // File I/O
    prim_hash_insert(st.intern("read-file"), "aot::lookup_prim(\"read-file\")");
    prim_hash_insert(st.intern("write-file"), "aot::lookup_prim(\"write-file\")");
    prim_hash_insert(st.intern("file-exists?"), "aot::lookup_prim(\"file-exists?\")");
    prim_hash_insert(st.intern("read-lines"), "aot::lookup_prim(\"read-lines\")");
    prim_hash_insert(st.intern("fs-open"), "aot::lookup_prim(\"fs-open\")");
    prim_hash_insert(st.intern("fs-read"), "aot::lookup_prim(\"fs-read\")");
    prim_hash_insert(st.intern("fs-write"), "aot::lookup_prim(\"fs-write\")");
    prim_hash_insert(st.intern("fs-close"), "aot::lookup_prim(\"fs-close\")");
    prim_hash_insert(st.intern("fs-stat"), "aot::lookup_prim(\"fs-stat\")");
    prim_hash_insert(st.intern("fs-readdir"), "aot::lookup_prim(\"fs-readdir\")");
    prim_hash_insert(st.intern("fs-rename"), "aot::lookup_prim(\"fs-rename\")");
    prim_hash_insert(st.intern("fs-unlink"), "aot::lookup_prim(\"fs-unlink\")");

    // Misc
    prim_hash_insert(st.intern("gensym"), "aot::lookup_prim(\"gensym\")");
    prim_hash_insert(st.intern("load"), "aot::lookup_prim(\"load\")");
    prim_hash_insert(st.intern("apply"), "aot::lookup_prim(\"apply\")");
}

fn void init_prim_hash_collections_math_bitwise(SymbolTable* st) {
    // Dict/collection ops
    prim_hash_insert(st.intern("dict"), "aot::lookup_prim(\"dict\")");
    prim_hash_insert(st.intern("dict-set!"), "aot::lookup_prim(\"dict-set!\")");
    prim_hash_insert(st.intern("dict?"), "aot::lookup_prim(\"dict?\")");
    prim_hash_insert(st.intern("ref"), "aot::lookup_prim(\"ref\")");
    prim_hash_insert(st.intern("keys"), "aot::lookup_prim(\"keys\")");
    prim_hash_insert(st.intern("values"), "aot::lookup_prim(\"values\")");
    prim_hash_insert(st.intern("has?"), "aot::lookup_prim(\"has?\")");
    prim_hash_insert(st.intern("remove!"), "aot::lookup_prim(\"remove!\")");
    prim_hash_insert(st.intern("push!"), "aot::lookup_prim(\"push!\")");
    prim_hash_insert(st.intern("json-get"), "aot::lookup_prim(\"json-get\")");
    prim_hash_insert(st.intern("json-parse"), "aot::lookup_prim(\"json-parse\")");
    prim_hash_insert(st.intern("json-emit"), "aot::lookup_prim(\"json-emit\")");
    prim_hash_insert(st.intern("json-emit-pretty"), "aot::lookup_prim(\"json-emit-pretty\")");
    prim_hash_insert(st.intern("toml-parse"), "aot::lookup_prim(\"toml-parse\")");
    prim_hash_insert(st.intern("csv-parse"), "aot::lookup_prim(\"csv-parse\")");
    prim_hash_insert(st.intern("csv-emit"), "aot::lookup_prim(\"csv-emit\")");
    prim_hash_insert(st.intern("time-point"), "aot::lookup_prim(\"time-point\")");
    prim_hash_insert(st.intern("time-point?"), "aot::lookup_prim(\"time-point?\")");

    // Math
    prim_hash_insert(st.intern("abs"), "aot::lookup_prim(\"abs\")");
    prim_hash_insert(st.intern("min"), "aot::lookup_prim(\"min\")");
    prim_hash_insert(st.intern("max"), "aot::lookup_prim(\"max\")");
    prim_hash_insert(st.intern("floor"), "aot::lookup_prim(\"floor\")");
    prim_hash_insert(st.intern("ceiling"), "aot::lookup_prim(\"ceiling\")");
    prim_hash_insert(st.intern("round"), "aot::lookup_prim(\"round\")");
    prim_hash_insert(st.intern("truncate"), "aot::lookup_prim(\"truncate\")");
    prim_hash_insert(st.intern("sqrt"), "aot::lookup_prim(\"sqrt\")");
    prim_hash_insert(st.intern("even?"), "aot::lookup_prim(\"even?\")");
    prim_hash_insert(st.intern("odd?"), "aot::lookup_prim(\"odd?\")");
    prim_hash_insert(st.intern("zero?"), "aot::lookup_prim(\"zero?\")");
    prim_hash_insert(st.intern("positive?"), "aot::lookup_prim(\"positive?\")");
    prim_hash_insert(st.intern("negative?"), "aot::lookup_prim(\"negative?\")");
    prim_hash_insert(st.intern("gcd"), "aot::lookup_prim(\"gcd\")");

    // Bitwise
    prim_hash_insert(st.intern("bitwise-and"), "aot::lookup_prim(\"bitwise-and\")");
    prim_hash_insert(st.intern("bitwise-or"), "aot::lookup_prim(\"bitwise-or\")");
    prim_hash_insert(st.intern("bitwise-xor"), "aot::lookup_prim(\"bitwise-xor\")");
    prim_hash_insert(st.intern("bitwise-not"), "aot::lookup_prim(\"bitwise-not\")");
    prim_hash_insert(st.intern("lshift"), "aot::lookup_prim(\"lshift\")");
    prim_hash_insert(st.intern("rshift"), "aot::lookup_prim(\"rshift\")");

    // Literals
    prim_hash_insert(st.intern("true"), "aot::make_true()");
    prim_hash_insert(st.intern("false"), "aot::make_false()");
    prim_hash_insert(st.intern("nil"), "aot::make_nil()");
}

fn void init_prim_hash(Interp* interp) {
    if (g_prim_hash_initialized) return;
    g_prim_hash_initialized = true;

    // Clear
    for (usz i = 0; i < PRIM_HASH_SIZE; i++) {
        g_prim_hash[i].key = (SymbolId)0xFFFFFFFF;
    }

    SymbolTable* st = &interp.symbols;
    init_prim_hash_arithmetic_and_comparison(st);
    init_prim_hash_core_and_strings(st);
    init_prim_hash_files_and_misc(st);
    init_prim_hash_collections_math_bitwise(st);
}
