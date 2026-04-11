module lisp;

import std::io;
import std::collections::list;
import main;

// =============================================================================
// SECTION 1b: PRIMITIVE VARIABLE HASH TABLE (domain population)
// =============================================================================
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
    prim_hash_insert(st.intern("List"), "aot::lookup_prim(\"List\")");
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
    prim_hash_insert(st.intern("string-upcase"), "aot::lookup_prim(\"string-upcase\")");
    prim_hash_insert(st.intern("string-downcase"), "aot::lookup_prim(\"string-downcase\")");
    prim_hash_insert(st.intern("string-casefold"), "aot::lookup_prim(\"string-casefold\")");
    prim_hash_insert(st.intern("string-titlecase"), "aot::lookup_prim(\"string-titlecase\")");
    prim_hash_insert(st.intern("string-trim"), "aot::lookup_prim(\"string-trim\")");
    prim_hash_insert(st.intern("string->number"), "aot::lookup_prim(\"string->number\")");
    prim_hash_insert(st.intern("string-contains?"), "aot::lookup_prim(\"string-contains?\")");

    // Type predicates
    prim_hash_insert(st.intern("string?"), "aot::lookup_prim(\"string?\")");
    prim_hash_insert(st.intern("integer?"), "aot::lookup_prim(\"integer?\")");
    prim_hash_insert(st.intern("int?"), "aot::lookup_prim(\"int?\")");
    prim_hash_insert(st.intern("symbol?"), "aot::lookup_prim(\"symbol?\")");
    prim_hash_insert(st.intern("closure?"), "aot::lookup_prim(\"closure?\")");
    prim_hash_insert(st.intern("continuation?"), "aot::lookup_prim(\"continuation?\")");
    prim_hash_insert(st.intern("double?"), "aot::lookup_prim(\"double?\")");
    prim_hash_insert(st.intern("list?"), "aot::lookup_prim(\"list?\")");
    prim_hash_insert(st.intern("boolean?"), "aot::lookup_prim(\"boolean?\")");
    prim_hash_insert(st.intern("bool?"), "aot::lookup_prim(\"bool?\")");
    prim_hash_insert(st.intern("dictionary?"), "aot::lookup_prim(\"dictionary?\")");
    prim_hash_insert(st.intern("number?"), "aot::lookup_prim(\"number?\")");
    prim_hash_insert(st.intern("equal?"), "aot::lookup_prim(\"equal?\")");
    prim_hash_insert(st.intern("type-of"), "aot::lookup_prim(\"type-of\")");

    // Callable type symbols / coercion constructors
    prim_hash_insert(st.intern("Integer"), "aot::lookup_prim(\"Integer\")");
    prim_hash_insert(st.intern("Double"), "aot::lookup_prim(\"Double\")");
    prim_hash_insert(st.intern("String"), "aot::lookup_prim(\"String\")");
    prim_hash_insert(st.intern("Symbol"), "aot::lookup_prim(\"Symbol\")");
    prim_hash_insert(st.intern("Boolean"), "aot::lookup_prim(\"Boolean\")");
    prim_hash_insert(st.intern("Nil"), "aot::lookup_prim(\"Nil\")");
    prim_hash_insert(st.intern("Void"), "aot::lookup_prim(\"Void\")");
    prim_hash_insert(st.intern("Closure"), "aot::lookup_prim(\"Closure\")");
    prim_hash_insert(st.intern("Iterator"), "aot::lookup_prim(\"Iterator\")");
}

fn void init_prim_hash_files_and_misc(SymbolTable* st) {
    // File I/O
    prim_hash_insert(st.intern("read-line"), "aot::lookup_prim(\"read-line\")");
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
    prim_hash_insert(st.intern("explain"), "aot::lookup_prim(\"explain\")");
    prim_hash_insert(st.intern("schema-explain"), "aot::lookup_prim(\"schema-explain\")");
}
