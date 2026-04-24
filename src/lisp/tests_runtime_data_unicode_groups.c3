module lisp;


import std::io;

fn void run_json_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("JSON Tests");

    // Parse strict-by-default
    test_error(interp, "json-parse rejects comments by default",
        "(json-parse \"{\\\"a\\\": 1 /* comment */ \\\"b\\\": 2}\")", pass, fail);
    test_error(interp, "json-parse rejects trailing comma by default",
        "(json-parse \"[1,2,3,]\")", pass, fail);
    test_error(interp, "json-parse rejects NaN/Inf by default",
        "(json-parse \"[NaN, Infinity]\")", pass, fail);

    // Parse options
    test_eq(interp, "json-parse allows comments",
        "(ref (json-parse \"{\\\"a\\\": 1 /* comment */ , \\\"b\\\": 2}\" '(allow-comments)) \"b\")", 2, pass, fail);
    test_eq(interp, "json-parse allows trailing commas",
        "(ref (json-parse \"[1,2,3,]\" '(allow-trailing-commas)) 2)", 3, pass, fail);
    test_tag(interp, "json-parse allows NaN/Inf",
        "(ref (json-parse \"[NaN, Infinity, -Infinity]\" '(allow-nan-inf)) 0)", DOUBLE, pass, fail);
    test_eq(interp, "json-parse allows comments and trailing commas together",
        "(ref (json-parse \"{\\\"a\\\": 1 /*comment*/ ,\\\"b\\\": 2,}\" '(allow-comments allow-trailing-commas)) \"b\")", 2, pass, fail);
    test_error(interp, "json-parse trailing commas still strict without allow-trailing-commas",
        "(json-parse \"[1,2,3,]\" '(allow-comments))", pass, fail);
    test_error(interp, "json-parse comments still strict without allow-comments",
        "(json-parse \"{\\\"a\\\": 1 /* comment */ , \\\"b\\\": 2}\" '(allow-trailing-commas))", pass, fail);
    test_error(interp, "json-parse unknown option symbol",
        "(json-parse \"42\" '(allow-foo))", pass, fail);

    // Parse basic types
    test_eq(interp, "json-parse integer",
        "(json-parse \"42\")", 42, pass, fail);
    test_str_val(interp, "json-parse string",
        "(json-parse \"\\\"hello\\\"\")", "hello", pass, fail);
    test_tag(interp, "json-parse null",
        "(json-parse \"null\")", NIL, pass, fail);

    // Parse array
    test_tag(interp, "json-parse array is array",
        "(json-parse \"[1,2,3]\")", ARRAY, pass, fail);
    test_eq(interp, "json-parse array length",
        "(length (json-parse \"[1,2,3]\"))", 3, pass, fail);

    // json-get pointer lookup
    test_eq(interp, "json-get object key",
        "(json-get (json-parse \"{\\\"a\\\": {\\\"b\\\": 42}}\") \"/a/b\")", 42, pass, fail);
    test_str_val(interp, "json-get array index",
        "(json-get (json-parse \"{\\\"users\\\": [\\\"alice\\\",\\\"bob\\\"]}\") \"/users/1\")", "bob", pass, fail);
    test_eq(interp, "json-get escaped object key",
        "(json-get (json-parse \"{\\\"a/b\\\": 7, \\\"a~0b\\\": 9}\") \"/a~1b\")", 7, pass, fail);
    test_tag(interp, "json-get root path",
        "(json-get (json-parse \"{\\\"a\\\": 1}\") \"\")", HASHMAP, pass, fail);

    test_nil(interp, "json-get missing path returns nil",
        "(json-get (json-parse \"{\\\"a\\\": 1}\") \"/b\")", pass, fail);
    test_error(interp, "json-get invalid escape",
        "(json-get (json-parse \"{\\\"a\\\": 1}\") \"/a~\")", pass, fail);
    test_error(interp, "json-get invalid path start",
        "(json-get (json-parse \"{\\\"a\\\": 1}\") \"a/b\")", pass, fail);

    // Parse object
    test_tag(interp, "json-parse object is dict",
        "(json-parse \"{\\\"a\\\": 1}\")", HASHMAP, pass, fail);

    // Emit basic types
    test_str_val(interp, "json-emit integer",
        "(json-emit 42)", "42", pass, fail);
    test_str_val(interp, "json-emit string",
        "(json-emit \"hello\")", "\"hello\"", pass, fail);
    test_str_val(interp, "json-emit supports precision",
        "(json-emit 1.23456 '((precision 2)))", "1.23", pass, fail);
    test_str_val(interp, "json-emit supports writer flag escape-slashes",
        "(json-emit \"a/b\" '((escape-slashes true)))", "\"a\\/b\"", pass, fail);
    test_str_val(interp, "json-emit supports writer flag newline-at-end",
        "(json-emit [1 2 3] '((newline-at-end true)))", "[1,2,3]\n", pass, fail);
    test_str_val(interp, "json-emit-pretty accepts option precision override",
        "(json-emit-pretty 1.2 '((precision 1)))", "1.2", pass, fail);
    test_str_val(interp, "json-emit nil",
        "(json-emit nil)", "null", pass, fail);

    // Round-trip: array
    test_str_val(interp, "json-emit array",
        "(json-emit [1 2 3])", "[1,2,3]", pass, fail);

    test_error_contains(interp, "json-emit validates unknown option",
        "(json-emit 1 '((bogus true)))", "json-emit: unknown option key", pass, fail);
    test_error_contains(interp, "json-emit validates precision range",
        "(json-emit 1 '((precision 99)))", "precision must be an integer between 0 and 15", pass, fail);
    test_error_contains(interp, "json-emit validates pretty-two-spaces option type",
        "(json-emit 1 '((pretty-two-spaces 1)))", "must be a boolean", pass, fail);
    test_error_contains(interp, "json-emit rejects improper option list tail",
        "(json-emit 1 (cons (list 'pretty true) 2))", "options list must be a proper list", pass, fail);
    test_error_contains(interp, "json-emit rejects unsupported root value type",
        "(json-emit (lambda (x) x))", "json-emit: unsupported value type", pass, fail);
    test_error_contains(interp, "json-emit rejects unsupported dictionary key type",
        "(json-emit (Dictionary 1 2))", "json-emit: unsupported dictionary key type", pass, fail);
    test_error_contains(interp, "json-emit rejects improper list tail",
        "(json-emit (cons 1 2))", "json-emit: expected proper list", pass, fail);

    // Round-trip: emit then parse
    test_eq(interp, "json round-trip integer",
        "(json-parse (json-emit 99))", 99, pass, fail);
}

fn void run_data_format_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("Data Format Tests");

    // TOML
    test_tag(interp, "toml-parse object is dict",
        "(toml-parse \"title = \\\"Toml\\\"\\n[owner]\\nname = \\\"Alice\\\"\\n\")", HASHMAP, pass, fail);
    test_str_val(interp, "toml-parse simple scalar",
        "(ref (toml-parse \"title = \\\"Toml\\\"\\n\") \"title\")", "Toml", pass, fail);
    test_str_val(interp, "toml-parse nested scalar",
        "(ref (ref (toml-parse \"[owner]\\nname = \\\"Alice\\\"\\n\\nage = 42\\n\") \"owner\") \"name\")", "Alice", pass, fail);
    test_eq(interp, "toml-parse numeric field",
        "(ref (toml-parse \"age = 42\\n\") \"age\")", 42, pass, fail);
    test_truthy(interp, "toml-parse boolean false preserves quoted false symbol",
        "(= (ref (toml-parse \"enabled = false\\n\") \"enabled\") 'false)",
        pass, fail);
    test_tag(interp, "toml-parse date",
        "(ref (toml-parse \"d = 1979-05-27\\n\") \"d\")", TIME_POINT, pass, fail);
    test_tag(interp, "toml-parse time",
        "(ref (toml-parse \"t = 07:32:00\\n\") \"t\")", TIME_POINT, pass, fail);
    test_tag(interp, "toml-parse datetime",
        "(ref (toml-parse \"dt = 1979-05-27T07:32:00Z\") \"dt\")", TIME_POINT, pass, fail);
    test_tag(interp, "toml-parse datetime with offset",
        "(ref (toml-parse \"dtz = 1979-05-27T07:32:00-07:00\\n\") \"dtz\")", TIME_POINT, pass, fail);
    test_truthy(interp, "toml-parse date equality",
        "(let (d1 (ref (toml-parse \"d = 1979-05-27\\n\") \"d\") d2 (ref (toml-parse \"d = 1979-05-27\\n\") \"d\")) (= d1 d2))",
        pass, fail);
    test_tag(interp, "TimePoint constructor date",
        "(TimePoint 'date 2026 3 7)", TIME_POINT, pass, fail);
    test_tag(interp, "time-point constructor date",
        "(TimePoint 'date 2026 3 7)", TIME_POINT, pass, fail);
    test_truthy(interp, "time-point predicate for date",
        "(time-point? (TimePoint 'date 2026 3 7))", pass, fail);
    test_tag(interp, "#time ISO-8601 reader tag",
        "#time \"2024-01-15T10:30:00Z\"", TIME_POINT, pass, fail);
    test_tag(interp, "time-point constructor time",
        "(TimePoint 'time 8 15 30 123456)", TIME_POINT, pass, fail);
    test_tag(interp, "time-point constructor datetime",
        "(TimePoint 'datetime 2026 3 7 8 15 30 123456)", TIME_POINT, pass, fail);
    test_tag(interp, "time-point constructor datetime with tz",
        "(TimePoint 'datetime-tz 2026 3 7 8 15 30 123456 -420)", TIME_POINT, pass, fail);
    test_tag(interp, "time-point constructor datetime with tz alias",
        "(TimePoint 'datetimetz 2026 3 7 8 15 30 123456 -420)", TIME_POINT, pass, fail);
    test_error_contains(interp, "time-point rejects unknown kind",
        "(TimePoint 'unknown 1 2 3)", "time-point:", pass, fail);
    test_error_contains(interp, "time-point rejects bad datetime-tz arity",
        "(TimePoint 'datetime-tz 2026 3 7 8 15 30)", "time-point: datetime-tz expects", pass, fail);
    test_error_contains(interp, "time-point rejects date field out of range",
        "(TimePoint 'date 2026 13 7)", "out of range", pass, fail);
    test_error_contains(interp, "time-point rejects datetime-tz offset out of range",
        "(TimePoint 'datetime-tz 2026 3 7 8 15 30 0 2000)", "out of range", pass, fail);
    test_tag(interp, "toml-parse supports options list",
        "(toml-parse \"title = \\\"Toml\\\"\n\" '((check-utf8 false)))", HASHMAP, pass, fail);
    test_error_contains(interp, "toml-parse validates unknown option key",
        "(toml-parse \"title = \\\"Toml\\\"\n\" '((bogus true)))", "toml-parse: unknown option key", pass, fail);
    test_error_contains(interp, "toml-parse validates option value",
        "(toml-parse \"title = \\\"Toml\\\"\n\" '((check-utf8 1)))", "toml-parse: check-utf8 must be a boolean", pass, fail);
    test_error_contains(interp, "toml-parse rejects improper options tail",
        "(toml-parse \"title = \\\"Toml\\\"\n\" (cons (list 'check-utf8 false) \"tail\"))", "toml-parse: options must be a proper list", pass, fail);
    test_error_contains(interp, "toml-parse rejects invalid input",
        "(toml-parse \"[broken = 1\")", "toml-parse", pass, fail);

    // CSV parse
    test_tag(interp, "csv-parse returns array",
        "(csv-parse \"a,b\\r\\n1,2\")", ARRAY, pass, fail);
    test_str_val(interp, "csv-parse basic values",
        "(ref (ref (csv-parse \"a,b\\r\\n1,2\") 0) 1)", "b", pass, fail);
    test_str_val(interp, "csv-parse quoted delimiter",
        "(ref (ref (csv-parse \"\\\"a,b\\\",2\") 0) 0)", "a,b", pass, fail);
    test_str_val(interp, "csv-parse preserves trailing empty field",
        "(ref (ref (csv-parse \"a,b,\") 0) 2)", "", pass, fail);
    test_error_contains(interp, "csv-parse default strict rejects LF-only rows",
        "(csv-parse \"a,b\\n1,2\")", "csv-parse: strict mode requires CRLF line endings", pass, fail);
    test_str_val(interp, "csv-parse supports strict mode toggle",
        "(ref (ref (csv-parse \"a,b\\rc,d\" '((strict false))) 1) 0)", "c", pass, fail);
    test_error_contains(interp, "csv-parse strict true matches default CRLF enforcement",
        "(csv-parse \"a,b\\r;c\" '((strict true)))", "csv-parse: strict mode requires CRLF line endings", pass, fail);
    test_error_contains(interp, "csv-parse rejects improper options tail",
        "(csv-parse \"a,b\" (cons (list 'delimiter \",\") \"tail\"))", "csv-parse: options list must be a proper list", pass, fail);
    test_str_val(interp, "csv-parse strict allows quote inside unquoted field",
        "(ref (ref (csv-parse \"a\\\"b,c\" '((strict false))) 0) 0)", "a\"b", pass, fail);
    test_eq(interp, "csv-parse omits trailing empty row",
        "(length (csv-parse \"a,b\\r\\n\"))", 1, pass, fail);
    test_str_val(interp, "csv-parse quoted multiline field",
        "(ref (ref (csv-parse \"\\\"a\\nb\\\",c\\r\\nd,e\") 0) 0)", "a\nb", pass, fail);
    test_error_contains(interp, "csv-parse rejects unterminated quote",
        "(csv-parse \"\\\"a,b\")", "csv-parse: unterminated quoted field", pass, fail);
    test_error_contains(interp, "csv-parse rejects invalid CR line terminator",
        "(csv-parse \"a,b\\r;c\")", "csv-parse: strict mode requires CRLF line endings", pass, fail);

    // CSV emit
    test_str_val(interp, "csv-emit basic",
        "(csv-emit [[\"a\" \"b,2\" \"x\" ]])", "a,\"b,2\",x", pass, fail);
    test_str_val(interp, "csv-emit with list rows",
        "(csv-emit '((\"a\" \"b\") (\"c\" \"d\")))", "a,b\r\nc,d", pass, fail);
    test_str_val(interp, "csv-emit with custom delimiter",
        "(csv-emit [[\"a\" \"b|c\"]] \"|\")", "a|\"b|c\"", pass, fail);
    test_str_val(interp, "csv-emit supports custom line ending",
        "(csv-emit [[\"a\" \"b\"] [\"c\" \"d\"]] '((line-ending \"\\r\\n\")))", "a,b\r\nc,d", pass, fail);
    test_str_val(interp, "csv-emit supports quote-style always",
        "(csv-emit [[\"a,b\" \"x\"]] '((quote-style 'always)))", "\"a,b\",\"x\"", pass, fail);
    test_str_val(interp, "csv-emit supports quote-style none",
        "(csv-emit [[\"a,b\" \"x\"]] '((quote-style 'none)))", "a,b,x", pass, fail);
    test_str_val(interp, "csv-emit supports nil-as",
        "(csv-emit [[nil \"x\"]] '((nil-as \"NULL\")))", "NULL,x", pass, fail);
    test_str_val(interp, "csv-emit supports custom quote-char",
        "(csv-emit [[\"a|b\" \"x\"]] '((quote-char \"|\") (quote-style 'always)))", "|a||b|,|x|", pass, fail);
    test_error_contains(interp, "csv-emit default strict validates quote-char != delimiter",
        "(csv-emit [[\"a\"]] '((quote-char \",\")))", "quote-char", pass, fail);
    test_str_val(interp, "csv-emit strict false allows quote-char == delimiter",
        "(csv-emit [[\"a\"]] '((quote-char \",\") (strict false)))", "a", pass, fail);
    test_error_contains(interp, "csv-emit validates quote-char != delimiter in strict mode",
        "(csv-emit [[\"a\"]] '((quote-char \",\") (strict true)))", "quote-char", pass, fail);
    test_error_contains(interp, "csv-emit rejects improper options tail",
        "(csv-emit [[\"a\"]] (cons (list 'delimiter \",\") \"tail\"))", "csv-emit: options list must be a proper list", pass, fail);
    test_error_contains(interp, "csv-emit validates row type",
        "(csv-emit [\"bad\"])", "csv-emit: expected each row", pass, fail);
    test_error_contains(interp, "csv-emit validates row cell types",
        "(csv-emit [[[]]])", "csv-emit: row cells must be", pass, fail);
    test_error_contains(interp, "csv-parse validates delimiter length",
        "(csv-parse \"a,b\" \";;\")", "delimiter must be a one-character string", pass, fail);

    // Thin dispatched format helpers (`parse` / `emit`)
    test_eq_interp(interp, "parse helper json route",
        "(ref (parse 'json \"{\\\"a\\\":1}\") \"a\")", 1, pass, fail);
    test_truthy_interp(interp, "parse helper json array route",
        "(= (ref (parse 'json \"[\\\"x\\\"]\") 0) \"x\")", pass, fail);
    test_truthy_interp(interp, "parse helper toml route",
        "(= (ref (parse 'toml \"title = \\\"Toml\\\"\\n\") \"title\") \"Toml\")", pass, fail);
    test_truthy_interp(interp, "parse helper toml array route",
        "(= (ref (ref (parse 'toml \"items = [\\\"x\\\"]\\n\") \"items\") 0) \"x\")", pass, fail);
    test_truthy_interp(interp, "parse helper csv route",
        "(= (ref (ref (parse 'csv \"a,b\\r\\n1,2\") 1) 1) \"2\")", pass, fail);
    test_truthy_interp(interp, "emit helper json route",
        "(= (emit 'json [1 2 3]) \"[1,2,3]\")", pass, fail);
    test_truthy_interp(interp, "emit helper json-pretty route",
        "(string-contains? (emit 'json-pretty [1 2]) \"\\n\")", pass, fail);
    test_truthy_interp(interp, "emit helper csv route",
        "(= (emit 'csv [[\"a,b\" \"x\"]] '((quote-style 'always))) \"\\\"a,b\\\",\\\"x\\\"\")", pass, fail);
    test_truthy_interp(interp, "parse helper unsupported format",
        "(= (handle (parse 'yaml \"a: 1\") (raise msg (ref msg 'message))) \"parse: unsupported format (supported: 'json, 'toml, 'csv)\")",
        pass, fail);
    test_truthy_interp(interp, "emit helper unsupported format",
        "(= (handle (emit 'toml 1) (raise msg (ref msg 'message))) \"emit: unsupported format (supported: 'json, 'json-pretty, 'csv)\")",
        pass, fail);
}

fn void run_json_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_json_tests, pass, fail);
}

fn void run_data_format_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_data_format_tests, pass, fail);
}

fn void run_compression_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("Compression Tests");

    // gzip round-trip
    test_str_val(interp, "gzip/gunzip round-trip",
        "(gunzip (gzip \"hello world\"))", "hello world", pass, fail);

    // deflate round-trip
    test_str_val(interp, "deflate/inflate round-trip",
        "(inflate (deflate \"test data\"))", "test data", pass, fail);

    // explicit compression levels
    test_str_val(interp, "gzip/gunzip round-trip level 1",
        "(gunzip (gzip \"hello world\" 1))", "hello world", pass, fail);
    test_str_val(interp, "deflate/inflate round-trip level 12",
        "(inflate (deflate \"test data\" 12))", "test data", pass, fail);
    test_error_contains(interp, "inflate rejects ignored extra args",
        "(inflate (deflate \"test data\") 32 64)", "expected 1-2 arguments", pass, fail);
    test_error_contains(interp, "inflate rejects non-integer original-size",
        "(inflate (deflate \"test data\") \"32\")", "original-size must be an integer", pass, fail);
    test_error_contains(interp, "inflate rejects non-positive original-size",
        "(inflate (deflate \"test data\") 0)", "original-size must be > 0", pass, fail);
    test_str_val(interp, "zlib round-trip",
        "(zlib-decompress (zlib-compress \"zlib payload\"))", "zlib payload", pass, fail);
    test_str_val(interp, "zlib round-trip level 9",
        "(zlib-decompress (zlib-compress \"zlib payload\" 9))", "zlib payload", pass, fail);

    // invalid compression level validation
    test_error_contains(interp, "gzip level rejects non-int",
        "(gzip \"hello\" 'bad)", "level must be an integer", pass, fail);
    test_error_contains(interp, "deflate level rejects out-of-range",
        "(deflate \"hello\" 99)", "level must be in range 0..12", pass, fail);
    test_error_contains(interp, "gzip rejects too many args",
        "(gzip \"hello\" 1 2)", "expected 1-2 arguments", pass, fail);
    test_str_val(interp, "gzip invalid arg payload domain",
        "(handle (gzip 42) (raise msg (String (ref msg 'domain))))",
        "type", pass, fail);
    test_str_val(interp, "gzip too-many-args payload code",
        "(handle (gzip \"hello\" 1 2) (raise msg (String (ref msg 'code))))",
        "type/arity", pass, fail);
    test_error_contains(interp, "zlib-decompress rejects corrupt payload",
        "(zlib-decompress \"not-a-zlib-stream\")", "decompression failed", pass, fail);
    test_error_contains(interp, "zlib-decompress rejects oversized original-size",
        "(zlib-decompress (zlib-compress \"zlib payload\") 4611686018427387904)", "original-size is too large", pass, fail);
    test_str_val(interp, "zlib-decompress corrupt payload code",
        "(handle (zlib-decompress \"not-a-zlib-stream\") (raise msg (String (ref msg 'code))))",
        "runtime/zlib-decompress-failed", pass, fail);
    test_error_contains(interp, "gunzip rejects corrupt payload",
        "(gunzip \"not-a-gzip-stream\")", "decompression failed", pass, fail);
    test_str_val(interp, "gunzip corrupt payload code",
        "(handle (gunzip \"not-a-gzip-stream\") (raise msg (String (ref msg 'code))))",
        "runtime/gunzip-failed", pass, fail);
    test_error_contains(interp, "inflate rejects corrupt payload",
        "(inflate \"not-a-deflate-stream\")", "decompression failed", pass, fail);

    // checksums
    test_eq(interp, "adler32 empty", "(adler32 \"\")", 1, pass, fail);
    test_eq(interp, "crc32 empty", "(crc32 \"\")", 0, pass, fail);
    test_eq(interp, "adler32 hello", "(adler32 \"hello\")", 103547413, pass, fail);
    test_eq(interp, "crc32 hello", "(crc32 \"hello\")", 907060870, pass, fail);
    test_error_contains(interp, "adler32 rejects non-string",
        "(adler32 42)", "expected string", pass, fail);
    test_error_contains(interp, "crc32 rejects non-string",
        "(crc32 42)", "expected string", pass, fail);

    // Empty string round-trip
    test_str_val(interp, "gzip/gunzip empty",
        "(gunzip (gzip \"\"))", "", pass, fail);
}

fn void run_compression_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_compression_tests, pass, fail);
}

fn void run_unicode_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("Unicode Tests");

    // Upcase: ASCII
    test_str_val(interp, "string-upcase ASCII",
        "(string-upcase \"hello\")", "HELLO", pass, fail);

    // Downcase: ASCII
    test_str_val(interp, "string-downcase ASCII",
        "(string-downcase \"HELLO\")", "hello", pass, fail);
    test_str_val(interp, "string-casefold ASCII",
        "(string-casefold \"HELLO\")", "hello", pass, fail);
    test_str_val(interp, "string-casefold unicode sharp s",
        "(string-casefold \"Straße\")", "strasse", pass, fail);
    test_str_val(interp, "string-titlecase ASCII",
        "(string-titlecase \"hello world\")", "Hello World", pass, fail);
    test_str_val(interp, "string-titlecase mixed",
        "(string-titlecase \"hELLO wORLD\")", "Hello World", pass, fail);
    test_eq(interp, "string-upcase long no truncation",
        "(string-length (string-upcase (string-repeat \"a\" 1500)))", 1500, pass, fail);
    test_eq(interp, "string-downcase long no truncation",
        "(string-length (string-downcase (string-repeat \"A\" 1500)))", 1500, pass, fail);
    test_eq(interp, "string-upcase long unicode no truncation",
        "(length (string-codepoints (string-upcase (string-repeat \"é\" 600))))", 600, pass, fail);
    test_eq(interp, "string-downcase long unicode no truncation",
        "(length (string-codepoints (string-downcase (string-repeat \"É\" 600))))", 600, pass, fail);

    // Codepoints: ASCII
    test_eq(interp, "string-codepoints ASCII count",
        "(length (string-codepoints \"abc\"))", 3, pass, fail);
    test_eq(interp, "string-codepoints first is 97",
        "(car (string-codepoints \"abc\"))", 97, pass, fail);

    // Char-category: letter
    test_str_val(interp, "char-category uppercase A",
        "(char-category 65)", "Lu", pass, fail);
    test_str_val(interp, "char-category lowercase a",
        "(char-category 97)", "Ll", pass, fail);
    test_str_val(interp, "char-category digit 0",
        "(char-category 48)", "Nd", pass, fail);
    test_eq(interp, "char-width ASCII A",
        "(char-width 65)", 1, pass, fail);
    test_error_contains(interp, "char-width rejects non-int",
        "(char-width \"A\")", "expected integer", pass, fail);
    test_error_contains(interp, "char-category rejects out-of-range codepoint",
        "(char-category 1114112)", "out of range", pass, fail);
    test_error_contains(interp, "char-property rejects negative codepoint",
        "(char-property -1 'category)", "out of range", pass, fail);
    test_str_val(interp, "char-property category",
        "(char-property 65 'category)", "Lu", pass, fail);
    test_eq(interp, "char-property width",
        "(char-property 65 'width)", 1, pass, fail);
    test_truthy(interp, "char-property letter? true",
        "(char-property 65 'letter?)", pass, fail);
    test_truthy(interp, "char-property digit? true",
        "(char-property 48 'digit?)", pass, fail);
    test_nil(interp, "char-property lower? false",
        "(char-property 65 'lower?)", pass, fail);
    test_error_contains(interp, "char-property unknown key",
        "(char-property 65 'unknown)", "unknown property", pass, fail);

    // Normalization: NFC (identity for ASCII)
    test_str_val(interp, "string-normalize NFC ASCII",
        "(string-normalize \"hello\" 'NFC)", "hello", pass, fail);

    // Graphemes: ASCII chars are each a grapheme
    test_eq(interp, "string-graphemes ASCII",
        "(length (string-graphemes \"abc\"))", 3, pass, fail);
}

fn void run_unicode_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_unicode_tests, pass, fail);
}
