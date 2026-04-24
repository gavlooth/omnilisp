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
    prim_hash_insert(st.intern("string-byte-length"), "aot::lookup_prim(\"string-byte-length\")");
    prim_hash_insert(st.intern("string-index-of"), "aot::lookup_prim(\"string-index-of\")");
    prim_hash_insert(st.intern("string-replace"), "aot::lookup_prim(\"string-replace\")");
    prim_hash_insert(st.intern("char-at"), "aot::lookup_prim(\"char-at\")");
    prim_hash_insert(st.intern("string-repeat"), "aot::lookup_prim(\"string-repeat\")");
    prim_hash_insert(st.intern("string-upcase"), "aot::lookup_prim(\"string-upcase\")");
    prim_hash_insert(st.intern("string-downcase"), "aot::lookup_prim(\"string-downcase\")");
    prim_hash_insert(st.intern("string-casefold"), "aot::lookup_prim(\"string-casefold\")");
    prim_hash_insert(st.intern("string-titlecase"), "aot::lookup_prim(\"string-titlecase\")");
    prim_hash_insert(st.intern("string-trim"), "aot::lookup_prim(\"string-trim\")");
    prim_hash_insert(st.intern("parse-number"), "aot::lookup_prim(\"parse-number\")");
    prim_hash_insert(st.intern("string-contains?"), "aot::lookup_prim(\"string-contains?\")");
    prim_hash_insert(st.intern("procedure?"), "aot::lookup_prim(\"procedure?\")");
    prim_hash_insert(st.intern("format"), "aot::lookup_prim(\"format\")");
    prim_hash_insert(st.intern("cl-format"), "aot::lookup_prim(\"cl-format\")");
    prim_hash_insert(st.intern("string-normalize"), "aot::lookup_prim(\"string-normalize\")");
    prim_hash_insert(st.intern("string-graphemes"), "aot::lookup_prim(\"string-graphemes\")");
    prim_hash_insert(st.intern("string-codepoints"), "aot::lookup_prim(\"string-codepoints\")");
    prim_hash_insert(st.intern("char-category"), "aot::lookup_prim(\"char-category\")");
    prim_hash_insert(st.intern("char-width"), "aot::lookup_prim(\"char-width\")");
    prim_hash_insert(st.intern("char-property"), "aot::lookup_prim(\"char-property\")");

    // Type predicates
    prim_hash_insert(st.intern("string?"), "aot::lookup_prim(\"string?\")");
    prim_hash_insert(st.intern("integer?"), "aot::lookup_prim(\"integer?\")");
    prim_hash_insert(st.intern("int?"), "aot::lookup_prim(\"int?\")");
    prim_hash_insert(st.intern("symbol?"), "aot::lookup_prim(\"symbol?\")");
    prim_hash_insert(st.intern("closure?"), "aot::lookup_prim(\"closure?\")");
    prim_hash_insert(st.intern("continuation?"), "aot::lookup_prim(\"continuation?\")");
    prim_hash_insert(st.intern("float64?"), "aot::lookup_prim(\"float64?\")");
    prim_hash_insert(st.intern("list?"), "aot::lookup_prim(\"list?\")");
    prim_hash_insert(st.intern("boolean?"), "aot::lookup_prim(\"boolean?\")");
    prim_hash_insert(st.intern("bool?"), "aot::lookup_prim(\"bool?\")");
    prim_hash_insert(st.intern("dictionary?"), "aot::lookup_prim(\"dictionary?\")");
    prim_hash_insert(st.intern("number?"), "aot::lookup_prim(\"number?\")");
    prim_hash_insert(st.intern("equal?"), "aot::lookup_prim(\"equal?\")");
    prim_hash_insert(st.intern("type-of"), "aot::lookup_prim(\"type-of\")");

    // Callable type symbols / coercion constructors
    prim_hash_insert(st.intern("Integer"), "aot::lookup_prim(\"Integer\")");
    prim_hash_insert(st.intern("BigInteger"), "aot::lookup_prim(\"BigInteger\")");
    prim_hash_insert(st.intern("BigFloat"), "aot::lookup_prim(\"BigFloat\")");
    prim_hash_insert(st.intern("BigComplex"), "aot::lookup_prim(\"BigComplex\")");
    prim_hash_insert(st.intern("Complex128"), "aot::lookup_prim(\"Complex128\")");
    prim_hash_insert(st.intern("Complex64"), "aot::lookup_prim(\"Complex64\")");
    prim_hash_insert(st.intern("Float"), "aot::lookup_prim(\"Float\")");
    prim_hash_insert(st.intern("Float32"), "aot::lookup_prim(\"Float32\")");
    prim_hash_insert(st.intern("Float64"), "aot::lookup_prim(\"Float64\")");
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
    prim_hash_insert(st.intern("macroexpand"), "aot::lookup_prim(\"macroexpand\")");
    prim_hash_insert(st.intern("eval"), "aot::lookup_prim(\"eval\")");
    prim_hash_insert(st.intern("bound?"), "aot::lookup_prim(\"bound?\")");
    prim_hash_insert(st.intern("error"), "aot::lookup_prim(\"error\")");
    prim_hash_insert(st.intern("error?"), "aot::lookup_prim(\"error?\")");
    prim_hash_insert(st.intern("error-message"), "aot::lookup_prim(\"error-message\")");
    prim_hash_insert(st.intern("read-string"), "aot::lookup_prim(\"read-string\")");
    prim_hash_insert(st.intern("unsafe-free!"), "aot::lookup_prim(\"unsafe-free!\")");
    prim_hash_insert(st.intern("shell"), "aot::lookup_prim(\"shell\")");
    prim_hash_insert(st.intern("random"), "aot::lookup_prim(\"random\")");
    prim_hash_insert(st.intern("random-int"), "aot::lookup_prim(\"random-int\")");
    prim_hash_insert(st.intern("getenv"), "aot::lookup_prim(\"getenv\")");
    prim_hash_insert(st.intern("time"), "aot::lookup_prim(\"time\")");
    prim_hash_insert(st.intern("time-ms"), "aot::lookup_prim(\"time-ms\")");
    prim_hash_insert(st.intern("exit"), "aot::lookup_prim(\"exit\")");
    prim_hash_insert(st.intern("sleep"), "aot::lookup_prim(\"sleep\")");
    prim_hash_insert(st.intern("gzip"), "aot::lookup_prim(\"gzip\")");
    prim_hash_insert(st.intern("gunzip"), "aot::lookup_prim(\"gunzip\")");
    prim_hash_insert(st.intern("deflate"), "aot::lookup_prim(\"deflate\")");
    prim_hash_insert(st.intern("inflate"), "aot::lookup_prim(\"inflate\")");
    prim_hash_insert(st.intern("zlib-compress"), "aot::lookup_prim(\"zlib-compress\")");
    prim_hash_insert(st.intern("zlib-decompress"), "aot::lookup_prim(\"zlib-decompress\")");
    prim_hash_insert(st.intern("adler32"), "aot::lookup_prim(\"adler32\")");
    prim_hash_insert(st.intern("crc32"), "aot::lookup_prim(\"crc32\")");
    prim_hash_insert(st.intern("validate"), "aot::lookup_prim(\"validate\")");
    prim_hash_insert(st.intern("spawn"), "aot::lookup_prim(\"spawn\")");
    prim_hash_insert(st.intern("await"), "aot::lookup_prim(\"await\")");
    prim_hash_insert(st.intern("run-fibers"), "aot::lookup_prim(\"run-fibers\")");
    prim_hash_insert(st.intern("fiber-cancel"), "aot::lookup_prim(\"fiber-cancel\")");
    prim_hash_insert(st.intern("atomic"), "aot::lookup_prim(\"atomic\")");
    prim_hash_insert(st.intern("atomic-add!"), "aot::lookup_prim(\"atomic-add!\")");
    prim_hash_insert(st.intern("atomic-read"), "aot::lookup_prim(\"atomic-read\")");
    prim_hash_insert(st.intern("atomic-cas!"), "aot::lookup_prim(\"atomic-cas!\")");
    prim_hash_insert(st.intern("explain"), "aot::lookup_prim(\"explain\")");
    prim_hash_insert(st.intern("schema-explain"), "aot::lookup_prim(\"schema-explain\")");
}

fn void init_prim_hash_runtime_parity_families(SymbolTable* st) {
    prim_hash_insert(st.intern("__raw-print"), "aot::lookup_prim(\"__raw-print\")");
    prim_hash_insert(st.intern("__raw-println"), "aot::lookup_prim(\"__raw-println\")");
    prim_hash_insert(st.intern("__raw-newline"), "aot::lookup_prim(\"__raw-newline\")");
    prim_hash_insert(st.intern("__raw-display"), "aot::lookup_prim(\"__raw-display\")");
    prim_hash_insert(st.intern("__raw-read-line"), "aot::lookup_prim(\"__raw-read-line\")");
    prim_hash_insert(st.intern("__raw-read-file"), "aot::lookup_prim(\"__raw-read-file\")");
    prim_hash_insert(st.intern("__raw-write-file"), "aot::lookup_prim(\"__raw-write-file\")");
    prim_hash_insert(st.intern("__raw-file-exists?"), "aot::lookup_prim(\"__raw-file-exists?\")");
    prim_hash_insert(st.intern("__raw-read-lines"), "aot::lookup_prim(\"__raw-read-lines\")");
    prim_hash_insert(st.intern("__raw-fs-open"), "aot::lookup_prim(\"__raw-fs-open\")");
    prim_hash_insert(st.intern("__raw-fs-read"), "aot::lookup_prim(\"__raw-fs-read\")");
    prim_hash_insert(st.intern("__raw-fs-write"), "aot::lookup_prim(\"__raw-fs-write\")");
    prim_hash_insert(st.intern("__raw-fs-close"), "aot::lookup_prim(\"__raw-fs-close\")");
    prim_hash_insert(st.intern("__raw-fs-stat"), "aot::lookup_prim(\"__raw-fs-stat\")");
    prim_hash_insert(st.intern("__raw-fs-readdir"), "aot::lookup_prim(\"__raw-fs-readdir\")");
    prim_hash_insert(st.intern("__raw-fs-rename"), "aot::lookup_prim(\"__raw-fs-rename\")");
    prim_hash_insert(st.intern("__raw-fs-unlink"), "aot::lookup_prim(\"__raw-fs-unlink\")");

    prim_hash_insert(st.intern("__iterator-from-list"), "aot::lookup_prim(\"__iterator-from-list\")");
    prim_hash_insert(st.intern("__iterator-from-array"), "aot::lookup_prim(\"__iterator-from-array\")");
    prim_hash_insert(st.intern("__iterator-from-dict"), "aot::lookup_prim(\"__iterator-from-dict\")");
    prim_hash_insert(st.intern("__iterator-from-tensor"), "aot::lookup_prim(\"__iterator-from-tensor\")");
    prim_hash_insert(st.intern("__iterator-map"), "aot::lookup_prim(\"__iterator-map\")");
    prim_hash_insert(st.intern("__iterator-filter"), "aot::lookup_prim(\"__iterator-filter\")");
    prim_hash_insert(st.intern("__iterator-take"), "aot::lookup_prim(\"__iterator-take\")");
    prim_hash_insert(st.intern("__iterator-zip"), "aot::lookup_prim(\"__iterator-zip\")");
    prim_hash_insert(st.intern("__iterator-foldl"), "aot::lookup_prim(\"__iterator-foldl\")");
    prim_hash_insert(st.intern("__iterator-range-from"), "aot::lookup_prim(\"__iterator-range-from\")");
    prim_hash_insert(st.intern("__iterator-repeat"), "aot::lookup_prim(\"__iterator-repeat\")");
    prim_hash_insert(st.intern("__iterator-cycle"), "aot::lookup_prim(\"__iterator-cycle\")");
    prim_hash_insert(st.intern("__reverse-list"), "aot::lookup_prim(\"__reverse-list\")");

    prim_hash_insert(st.intern("foreign-describe"), "aot::lookup_prim(\"foreign-describe\")");
    prim_hash_insert(st.intern("foreign-release"), "aot::lookup_prim(\"foreign-release\")");
    prim_hash_insert(st.intern("ffi-async-call"), "aot::lookup_prim(\"ffi-async-call\")");
    prim_hash_insert(st.intern("ffi-callback"), "aot::lookup_prim(\"ffi-callback\")");
    prim_hash_insert(st.intern("runtime-memory-stats"), "aot::lookup_prim(\"runtime-memory-stats\")");

    prim_hash_insert(st.intern("__raw-tcp-connect"), "aot::lookup_prim(\"__raw-tcp-connect\")");
    prim_hash_insert(st.intern("__raw-tcp-listen"), "aot::lookup_prim(\"__raw-tcp-listen\")");
    prim_hash_insert(st.intern("__raw-tcp-accept"), "aot::lookup_prim(\"__raw-tcp-accept\")");
    prim_hash_insert(st.intern("__raw-tcp-read"), "aot::lookup_prim(\"__raw-tcp-read\")");
    prim_hash_insert(st.intern("__raw-tcp-write"), "aot::lookup_prim(\"__raw-tcp-write\")");
    prim_hash_insert(st.intern("__raw-tcp-close"), "aot::lookup_prim(\"__raw-tcp-close\")");
    prim_hash_insert(st.intern("__raw-udp-socket"), "aot::lookup_prim(\"__raw-udp-socket\")");
    prim_hash_insert(st.intern("__raw-udp-bind"), "aot::lookup_prim(\"__raw-udp-bind\")");
    prim_hash_insert(st.intern("__raw-udp-send"), "aot::lookup_prim(\"__raw-udp-send\")");
    prim_hash_insert(st.intern("__raw-udp-recv"), "aot::lookup_prim(\"__raw-udp-recv\")");
    prim_hash_insert(st.intern("__raw-udp-close"), "aot::lookup_prim(\"__raw-udp-close\")");
    prim_hash_insert(st.intern("__raw-pipe-connect"), "aot::lookup_prim(\"__raw-pipe-connect\")");
    prim_hash_insert(st.intern("__raw-pipe-listen"), "aot::lookup_prim(\"__raw-pipe-listen\")");
    prim_hash_insert(st.intern("__raw-process-spawn"), "aot::lookup_prim(\"__raw-process-spawn\")");
    prim_hash_insert(st.intern("__raw-process-wait"), "aot::lookup_prim(\"__raw-process-wait\")");
    prim_hash_insert(st.intern("__raw-process-kill"), "aot::lookup_prim(\"__raw-process-kill\")");
    prim_hash_insert(st.intern("__raw-signal-handle"), "aot::lookup_prim(\"__raw-signal-handle\")");
    prim_hash_insert(st.intern("__raw-signal-unhandle"), "aot::lookup_prim(\"__raw-signal-unhandle\")");
    prim_hash_insert(st.intern("__raw-uv-timer-callback-handle"), "aot::lookup_prim(\"__raw-uv-timer-callback-handle\")");
    prim_hash_insert(st.intern("__raw-uv-timer-callback-invoke"), "aot::lookup_prim(\"__raw-uv-timer-callback-invoke\")");
    prim_hash_insert(st.intern("__raw-uv-timer-callback-unhandle"), "aot::lookup_prim(\"__raw-uv-timer-callback-unhandle\")");
    prim_hash_insert(st.intern("__raw-uv-timer-callback-dispatch-count"), "aot::lookup_prim(\"__raw-uv-timer-callback-dispatch-count\")");
    prim_hash_insert(st.intern("__raw-dns-resolve"), "aot::lookup_prim(\"__raw-dns-resolve\")");
    prim_hash_insert(st.intern("__raw-async-sleep"), "aot::lookup_prim(\"__raw-async-sleep\")");
    prim_hash_insert(st.intern("__raw-offload"), "aot::lookup_prim(\"__raw-offload\")");
    prim_hash_insert(st.intern("__raw-offload-batch"), "aot::lookup_prim(\"__raw-offload-batch\")");
    prim_hash_insert(st.intern("__raw-thread-spawn"), "aot::lookup_prim(\"__raw-thread-spawn\")");
    prim_hash_insert(st.intern("__raw-thread-spawn-batch"), "aot::lookup_prim(\"__raw-thread-spawn-batch\")");
    prim_hash_insert(st.intern("__raw-thread-join"), "aot::lookup_prim(\"__raw-thread-join\")");
    prim_hash_insert(st.intern("__raw-thread-join-timeout"), "aot::lookup_prim(\"__raw-thread-join-timeout\")");
    prim_hash_insert(st.intern("__raw-thread-cancel"), "aot::lookup_prim(\"__raw-thread-cancel\")");
    prim_hash_insert(st.intern("__raw-task-spawn"), "aot::lookup_prim(\"__raw-task-spawn\")");
    prim_hash_insert(st.intern("__raw-task-spawn-batch"), "aot::lookup_prim(\"__raw-task-spawn-batch\")");
    prim_hash_insert(st.intern("__raw-task-join"), "aot::lookup_prim(\"__raw-task-join\")");
    prim_hash_insert(st.intern("__raw-task-join-timeout"), "aot::lookup_prim(\"__raw-task-join-timeout\")");
    prim_hash_insert(st.intern("__raw-task-cancel"), "aot::lookup_prim(\"__raw-task-cancel\")");
    prim_hash_insert(st.intern("__raw-http-get"), "aot::lookup_prim(\"__raw-http-get\")");
    prim_hash_insert(st.intern("__raw-http-request"), "aot::lookup_prim(\"__raw-http-request\")");

    prim_hash_insert(st.intern("__raw-tls-connect"), "aot::lookup_prim(\"__raw-tls-connect\")");
    prim_hash_insert(st.intern("__raw-tls-trust-store-cause"), "aot::lookup_prim(\"__raw-tls-trust-store-cause\")");
    prim_hash_insert(st.intern("__raw-tls-server-wrap"), "aot::lookup_prim(\"__raw-tls-server-wrap\")");
    prim_hash_insert(st.intern("__raw-tls-read"), "aot::lookup_prim(\"__raw-tls-read\")");
    prim_hash_insert(st.intern("__raw-tls-write"), "aot::lookup_prim(\"__raw-tls-write\")");
    prim_hash_insert(st.intern("__raw-tls-close"), "aot::lookup_prim(\"__raw-tls-close\")");

    prim_hash_insert(st.intern("deduce"), "aot::lookup_prim(\"deduce\")");
    prim_hash_insert(st.intern("deduce/open"), "aot::lookup_prim(\"deduce/open\")");
    prim_hash_insert(st.intern("deduce/open-named"), "aot::lookup_prim(\"deduce/open-named\")");
    prim_hash_insert(st.intern("deduce/block"), "aot::lookup_prim(\"deduce/block\")");
    prim_hash_insert(st.intern("deduce/commit"), "aot::lookup_prim(\"deduce/commit\")");
    prim_hash_insert(st.intern("deduce/abort"), "aot::lookup_prim(\"deduce/abort\")");
    prim_hash_insert(st.intern("deduce/scan"), "aot::lookup_prim(\"deduce/scan\")");
    prim_hash_insert(st.intern("deduce/scan-range"), "aot::lookup_prim(\"deduce/scan-range\")");
    prim_hash_insert(st.intern("deduce/query"), "aot::lookup_prim(\"deduce/query\")");
    prim_hash_insert(st.intern("deduce/schema"), "aot::lookup_prim(\"deduce/schema\")");
    prim_hash_insert(st.intern("deduce/indexes"), "aot::lookup_prim(\"deduce/indexes\")");
    prim_hash_insert(st.intern("deduce/stats"), "aot::lookup_prim(\"deduce/stats\")");
    prim_hash_insert(st.intern("deduce/count"), "aot::lookup_prim(\"deduce/count\")");
    prim_hash_insert(st.intern("deduce/match"), "aot::lookup_prim(\"deduce/match\")");
    prim_hash_insert(st.intern("deduce/fact!"), "aot::lookup_prim(\"deduce/fact!\")");
    prim_hash_insert(st.intern("deduce/retract!"), "aot::lookup_prim(\"deduce/retract!\")");
    prim_hash_insert(st.intern("deduce/clear!"), "aot::lookup_prim(\"deduce/clear!\")");
    prim_hash_insert(st.intern("deduce/drop!"), "aot::lookup_prim(\"deduce/drop!\")");
    prim_hash_insert(st.intern("deduce/rule!"), "aot::lookup_prim(\"deduce/rule!\")");
    prim_hash_insert(st.intern("deduce/explain"), "aot::lookup_prim(\"deduce/explain\")");
    prim_hash_insert(st.intern("deduce/why-result"), "aot::lookup_prim(\"deduce/why-result\")");
    prim_hash_insert(st.intern("deduce/analyze"), "aot::lookup_prim(\"deduce/analyze\")");
    prim_hash_insert(st.intern("deduce/materialize!"), "aot::lookup_prim(\"deduce/materialize!\")");
    prim_hash_insert(st.intern("deduce/dematerialize!"), "aot::lookup_prim(\"deduce/dematerialize!\")");
    prim_hash_insert(st.intern("deduce/refresh!"), "aot::lookup_prim(\"deduce/refresh!\")");
    prim_hash_insert(st.intern("__define-relation"), "aot::lookup_prim(\"__define-relation\")");

    prim_hash_insert(st.intern("__ui-ftxui-run"), "aot::lookup_prim(\"__ui-ftxui-run\")");
    prim_hash_insert(st.intern("__ui-ftxui-dispatch"), "aot::lookup_prim(\"__ui-ftxui-dispatch\")");
    prim_hash_insert(st.intern("__ui-ftxui-loop"), "aot::lookup_prim(\"__ui-ftxui-loop\")");
    prim_hash_insert(st.intern("__ui-ftxui-session-open"), "aot::lookup_prim(\"__ui-ftxui-session-open\")");
    prim_hash_insert(st.intern("__ui-ftxui-session-update"), "aot::lookup_prim(\"__ui-ftxui-session-update\")");
    prim_hash_insert(st.intern("__ui-ftxui-session-render"), "aot::lookup_prim(\"__ui-ftxui-session-render\")");
    prim_hash_insert(st.intern("__ui-ftxui-session-read-event"), "aot::lookup_prim(\"__ui-ftxui-session-read-event\")");
    prim_hash_insert(st.intern("__ui-ftxui-session-invalidate"), "aot::lookup_prim(\"__ui-ftxui-session-invalidate\")");
    prim_hash_insert(st.intern("__ui-ftxui-session-post-event"), "aot::lookup_prim(\"__ui-ftxui-session-post-event\")");
    prim_hash_insert(st.intern("__ui-ftxui-session-close"), "aot::lookup_prim(\"__ui-ftxui-session-close\")");
}
