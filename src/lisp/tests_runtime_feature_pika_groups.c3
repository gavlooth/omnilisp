module lisp;


import std::io;
import pika;

fn void run_pika_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("Pika Regex/Grammar Tests");

    // Regex: re-match
    test_str_val(interp, "re-match literal",
        "(re-match \"hello\" \"say hello world\")", "hello", pass, fail);
    test_tag(interp, "re-match no match returns nil",
        "(re-match \"xyz\" \"hello\")", NIL, pass, fail);
    test_str(interp, "re-match digits finds digit",
        "(re-match \"[0-9]+\" \"abc123def\")", pass, fail);
    test_str_val(interp, "re-match dot",
        "(re-match \"a.c\" \"abc\")", "abc", pass, fail);
    test_str_val(interp, "re-match star quantifier",
        "(re-match \"ab*c\" \"zzabbbczz\")", "abbbc", pass, fail);
    test_str_val(interp, "re-match plus quantifier",
        "(re-match \"ab+c\" \"zzabbbczz\")", "abbbc", pass, fail);
    test_str_val(interp, "re-match question quantifier",
        "(re-match \"ab?c\" \"zzaczz\")", "ac", pass, fail);
    test_str_val(interp, "re-match alternation",
        "(re-match \"ab|cd\" \"zzcdyy\")", "cd", pass, fail);

    // Regex: re-fullmatch
    test_str_val(interp, "re-fullmatch exact",
        "(re-fullmatch \"hello\" \"hello\")", "hello", pass, fail);
    test_str_val(interp, "re-fullmatch start+end anchors",
        "(re-fullmatch \"^hello$\" \"hello\")", "hello", pass, fail);
    test_tag(interp, "re-fullmatch rejects partial",
        "(re-fullmatch \"hello\" \"hello world\")", NIL, pass, fail);
    test_str_val(interp, "re-fullmatch zero-length-capable pattern matches empty input",
        "(re-fullmatch \"a*\" \"\")", "", pass, fail);
    test_tag(interp, "re-match start anchor rejects non-zero start",
        "(re-match \"^abc\" \"xabc\")", NIL, pass, fail);
    test_str_val(interp, "re-match end anchor matches suffix at end",
        "(re-match \"abc$\" \"xabc\")", "abc", pass, fail);
    test_str_val(interp, "re-match zero-length-capable pattern matches empty input",
        "(re-match \"a*\" \"\")", "", pass, fail);
    test_str_val(interp, "re-fullmatch bounded exact {n}",
        "(re-fullmatch \"ab{3}\" \"abbb\")", "abbb", pass, fail);
    test_str_val(interp, "re-fullmatch bounded range {n,m}",
        "(re-fullmatch \"ab{2,4}\" \"abbb\")", "abbb", pass, fail);
    test_str_val(interp, "re-fullmatch bounded unbounded {n,}",
        "(re-fullmatch \"ab{2,}\" \"abbbbb\")", "abbbbb", pass, fail);
    test_tag(interp, "re-fullmatch bounded range rejects overflow",
        "(re-fullmatch \"ab{2,4}\" \"abbbbb\")", NIL, pass, fail);
    test_str_val(interp, "re-fullmatch group plus",
        "(re-fullmatch \"(ab)+\" \"abab\")", "abab", pass, fail);
    test_str_val(interp, "re-fullmatch char-class chain >8",
        "(re-fullmatch \"[a][b][c][d][e][f][g][h][i]\" \"abcdefghi\")", "abcdefghi", pass, fail);
    test_tag(interp, "re-fullmatch char-class chain >32",
        "(let loop (i 0 p \"\" s \"\") (if (= i 40) (re-fullmatch p s) (loop (+ i 1) (string-append p \"[a]\") (string-append s \"a\"))))",
        STRING, pass, fail);
    test_tag(interp, "re-fullmatch char-class preserves entries past 64",
        "(let loop (i 0 cls \"[\") (if (= i 64) (re-fullmatch (string-append cls \"z]\") \"z\") (loop (+ i 1) (string-append cls \"a\"))))",
        STRING, pass, fail);
    test_str_val(interp, "re-fullmatch long compiled pattern",
        "(re-fullmatch \"(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|z)\" \"z\")", "z", pass, fail);
    test_tag(interp, "re-fullmatch long concatenation dynamic pattern",
        "(let loop (i 0 p \"\") (if (= i 180) (re-fullmatch p p) (loop (+ i 1) (string-append p \"a\"))))",
        STRING, pass, fail);
    test_tag(interp, "re-fullmatch long alternation dynamic pattern",
        "(let loop (i 0 p \"x\") (if (= i 80) (re-fullmatch p \"x\") (loop (+ i 1) (string-append p \"|x\"))))",
        STRING, pass, fail);
    test_tag(interp, "re-fullmatch large bounded quantifier shape",
        "(let loop (i 0 s \"\") (if (= i 200) (re-fullmatch \"a{128,256}\" s) (loop (+ i 1) (string-append s \"a\"))))",
        STRING, pass, fail);

    {
        EvalResult r = run(
            "(let loop (i 0 ok true) "
            "  (if (= i 40) ok "
            "      (let (p (string-append \"(\" (String i) \"|x)\") "
            "            m (re-fullmatch p \"x\")) "
            "        (loop (+ i 1) (and ok (and m (= m \"x\")))))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] re-fullmatch cache grows beyond 32 compiled patterns");
            (*pass)++;
        } else {
            io::printn("[FAIL] re-fullmatch cache grows beyond 32 compiled patterns");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(let loop (i 0 ok true) "
            "  (if (= i 180) ok "
            "      (let (p (string-append \"(\" (String i) \"|x)\") "
            "            m (re-fullmatch p \"x\")) "
            "        (loop (+ i 1) (and ok (and m (= m \"x\")))))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] re-fullmatch cache bounded eviction stays usable");
            (*pass)++;
        } else {
            io::printn("[FAIL] re-fullmatch cache bounded eviction stays usable");
            (*fail)++;
        }
    }

    // Regex: re-find-all
    test_eq(interp, "re-find-all count",
        "(length (re-find-all \"a\" \"abracadabra\"))", 5, pass, fail);
    test_str_val(interp, "re-find-all first",
        "(car (re-find-all \"[0-9]+\" \"a1 b22 c333\"))", "1", pass, fail);
    test_eq(interp, "re-find-all zero-length-capable pattern matches empty input",
        "(length (re-find-all \"a*\" \"\"))", 1, pass, fail);
    test_str_val(interp, "re-find-all empty-input zero-length match is empty string",
        "(car (re-find-all \"a*\" \"\"))", "", pass, fail);

    // Regex: re-split
    test_eq(interp, "re-split count",
        "(length (re-split \" \" \"hello world foo\"))", 3, pass, fail);
    test_str_val(interp, "re-split first",
        "(car (re-split \" \" \"hello world\"))", "hello", pass, fail);
    test_eq(interp, "re-split preserves leading trailing and consecutive empty fields",
        "(length (re-split \",\" \",a,,b,\"))", 5, pass, fail);
    test_str_val(interp, "re-split preserves leading empty field",
        "(ref (re-split \",\" \",a,,b,\") 0)", "", pass, fail);
    test_str_val(interp, "re-split preserves consecutive empty field",
        "(ref (re-split \",\" \",a,,b,\") 2)", "", pass, fail);
    test_str_val(interp, "re-split preserves trailing empty field",
        "(ref (re-split \",\" \",a,,b,\") 4)", "", pass, fail);
    test_str_val(interp, "re-split no match on empty input returns empty field",
        "(car (re-split \",\" \"\"))", "", pass, fail);

    // Regex: re-replace
    test_str_val(interp, "re-replace first",
        "(re-replace \"[0-9]+\" \"X\" \"a1b2c3\")", "aXb2c3", pass, fail);
    test_str(interp, "re-replace global returns string",
        "(re-replace \"a\" \"X\" \"abaca\" 'global)", pass, fail);
    test_str_val(interp, "re-replace global replaces every match",
        "(re-replace \"a\" \"X\" \"abaca\" 'global)", "XbXcX", pass, fail);
    test_eq(interp, "re-replace first preserves >1024 output",
        "(string-byte-length (re-replace \"a\" (string-repeat \"b\" 1500) \"a\"))", 1500, pass, fail);
    test_eq(interp, "re-replace global preserves >1024 output",
        "(string-byte-length (re-replace \"a\" \"abcdef\" (string-repeat \"a\" 300) 'global))", 1800, pass, fail);
    test_str_val(interp, "re-replace rejects unknown fourth symbol",
        "(handle (re-replace \"a\" \"X\" \"a\" 'all) (raise msg (String (ref msg 'code))))",
        "regex/re-replace-invalid-mode", pass, fail);
    test_str_val(interp, "re-replace rejects numeric fourth argument",
        "(handle (re-replace \"a\" \"X\" \"a\" 2) (raise msg (String (ref msg 'code))))",
        "regex/re-replace-invalid-mode", pass, fail);
    test_str_val(interp, "re-replace rejects extra arguments",
        "(handle (re-replace \"a\" \"X\" \"a\" 'global 'extra) (raise msg (String (ref msg 'code))))",
        "regex/re-replace-expected-pattern-replacement-input", pass, fail);

    // Regex: re-match-pos
    test_eq(interp, "re-match-pos start",
        "(car (re-match-pos \"hello\" \"say hello\"))", 4, pass, fail);
    test_eq(interp, "re-match-pos codepoint start after UTF-8 prefix",
        "(car (re-match-pos \"x\" \"éx\"))", 1, pass, fail);
    test_eq(interp, "re-match-pos codepoint end after UTF-8 prefix",
        "(car (cdr (re-match-pos \"xy\" \"éxy\")))", 3, pass, fail);
    test_eq(interp, "re-match-pos zero-length-capable empty input start",
        "(car (re-match-pos \"a*\" \"\"))", 0, pass, fail);
    test_eq(interp, "re-match-pos zero-length-capable empty input end",
        "(car (cdr (re-match-pos \"a*\" \"\")))", 0, pass, fail);

    // Regex: re-find-all-pos
    test_eq(interp, "re-find-all-pos count",
        "(length (re-find-all-pos \"a\" \"abaca\"))", 3, pass, fail);
    test_eq(interp, "re-find-all-pos codepoint second start after UTF-8 prefix",
        "(car (car (cdr (re-find-all-pos \"x\" \"éxax\"))))", 3, pass, fail);
    test_eq(interp, "re-find-all-pos codepoint second end after UTF-8 prefix",
        "(car (cdr (car (cdr (re-find-all-pos \"x\" \"éxax\")))))", 4, pass, fail);
    test_eq(interp, "re-find-all-pos zero-length-capable empty input count",
        "(length (re-find-all-pos \"a*\" \"\"))", 1, pass, fail);
    test_eq(interp, "re-find-all-pos zero-length-capable empty input start",
        "(car (car (re-find-all-pos \"a*\" \"\")))", 0, pass, fail);
    test_eq(interp, "re-find-all-pos zero-length-capable empty input end",
        "(car (cdr (car (re-find-all-pos \"a*\" \"\"))))", 0, pass, fail);

    {
        int saved_force = pika::g_regex_force_result_string_alloc_fail_nth;
        pika::g_regex_force_result_string_alloc_fail_nth = 1;
        test_truthy_interp(interp, "re-match result string allocation raises structured code",
            "(= (handle (re-match \"a\" \"a\") (raise msg (ref msg 'code))) 'regex/re-match-out-of-memory)",
            pass, fail);
        pika::g_regex_force_result_string_alloc_fail_nth = saved_force;
    }
    {
        int saved_force = pika::g_regex_force_result_string_alloc_fail_nth;
        pika::g_regex_force_result_string_alloc_fail_nth = 1;
        test_truthy_interp(interp, "re-fullmatch result string allocation raises structured code",
            "(= (handle (re-fullmatch \"a\" \"a\") (raise msg (ref msg 'code))) 'regex/re-fullmatch-out-of-memory)",
            pass, fail);
        pika::g_regex_force_result_string_alloc_fail_nth = saved_force;
    }
    {
        int saved_force = pika::g_regex_force_result_string_alloc_fail_nth;
        pika::g_regex_force_result_string_alloc_fail_nth = 1;
        test_truthy_interp(interp, "re-find-all result string allocation raises structured code",
            "(= (handle (re-find-all \"a\" \"a\") (raise msg (ref msg 'code))) 'regex/re-find-all-out-of-memory)",
            pass, fail);
        pika::g_regex_force_result_string_alloc_fail_nth = saved_force;
    }
    {
        int saved_force = pika::g_regex_force_result_string_alloc_fail_nth;
        pika::g_regex_force_result_string_alloc_fail_nth = 1;
        test_truthy_interp(interp, "re-split result string allocation raises structured code",
            "(= (handle (re-split \" \" \"a b\") (raise msg (ref msg 'code))) 'regex/re-split-out-of-memory)",
            pass, fail);
        pika::g_regex_force_result_string_alloc_fail_nth = saved_force;
    }
    {
        int saved_force = g_value_force_make_cons_or_error_alloc_fail_nth;
        g_value_force_make_cons_or_error_alloc_fail_nth = 1;
        test_truthy_interp(interp, "re-find-all cons allocation raises structured code",
            "(= (handle (re-find-all \"a\" \"a\") (raise msg (ref msg 'code))) 'regex/re-find-all-out-of-memory)",
            pass, fail);
        g_value_force_make_cons_or_error_alloc_fail_nth = saved_force;
    }
    {
        int saved_force = g_value_force_make_cons_or_error_alloc_fail_nth;
        g_value_force_make_cons_or_error_alloc_fail_nth = 1;
        test_truthy_interp(interp, "re-split cons allocation raises structured code",
            "(= (handle (re-split \" \" \"a b\") (raise msg (ref msg 'code))) 'regex/re-split-out-of-memory)",
            pass, fail);
        g_value_force_make_cons_or_error_alloc_fail_nth = saved_force;
    }
    {
        int saved_force = g_value_force_make_cons_or_error_alloc_fail_nth;
        g_value_force_make_cons_or_error_alloc_fail_nth = 1;
        test_truthy_interp(interp, "re-match-pos cons allocation raises structured code",
            "(= (handle (re-match-pos \"a\" \"a\") (raise msg (ref msg 'code))) 'regex/re-match-pos-out-of-memory)",
            pass, fail);
        g_value_force_make_cons_or_error_alloc_fail_nth = saved_force;
    }
    {
        int saved_force = g_value_force_make_cons_or_error_alloc_fail_nth;
        g_value_force_make_cons_or_error_alloc_fail_nth = 1;
        test_truthy_interp(interp, "re-find-all-pos cons allocation raises structured code",
            "(= (handle (re-find-all-pos \"a\" \"a\") (raise msg (ref msg 'code))) 'regex/re-find-all-pos-out-of-memory)",
            pass, fail);
        g_value_force_make_cons_or_error_alloc_fail_nth = saved_force;
    }

    // Regex: escaped chars
    test_str(interp, "re-match \\d",
        "(re-match \"\\\\d+\" \"abc123\")", pass, fail);
    test_str(interp, "re-match \\w+",
        "(re-match \"\\\\w+\" \"hello_world\")", pass, fail);

    // Regex: strict grammar validation
    test_str_val(interp, "re-fullmatch unmatched close paren raises",
        "(handle (re-fullmatch \"a)\" \"a\") (raise msg (String (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch unmatched open paren raises",
        "(handle (re-fullmatch \"(ab\" \"ab\") (raise msg (String (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch leading quantifier raises",
        "(handle (re-fullmatch \"*a\" \"a\") (raise msg (String (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch trailing quantifier raises",
        "(handle (re-fullmatch \"a**\" \"a\") (raise msg (String (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch unknown group type raises",
        "(handle (re-fullmatch \"(?q)a\" \"a\") (raise msg (String (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch malformed bounded quantifier missing min raises",
        "(handle (re-fullmatch \"a{,2}\" \"aa\") (raise msg (String (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch malformed bounded quantifier upper raises",
        "(handle (re-fullmatch \"a{2,x}\" \"aa\") (raise msg (String (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_tag(interp, "re-fullmatch supports bounded quantifier >4096",
        "(re-fullmatch \"a{1,5000}\" \"a\")", STRING, pass, fail);
    test_str_val(interp, "re-fullmatch bounded quantifier overflow upper raises",
        "(handle (re-fullmatch \"a{1,2147483648}\" \"a\") (raise msg (String (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch bounded quantifier overflow lower raises",
        "(handle (re-fullmatch \"a{2147483648}\" \"a\") (raise msg (String (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch descending bounded quantifier raises",
        "(handle (re-fullmatch \"a{3,2}\" \"aaa\") (raise msg (String (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch empty char class raises",
        "(handle (re-fullmatch \"[]\" \"a\") (raise msg (String (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch invalid class range raises",
        "(handle (re-fullmatch \"[z-a]\" \"z\") (raise msg (String (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch trailing escape raises",
        "(handle (re-fullmatch \"\\\\\" \"a\") (raise msg (String (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch trailing token after end anchor raises",
        "(handle (re-fullmatch \"a$)\" \"a\") (raise msg (String (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-match malformed pattern raises",
        "(handle (re-match \"a)\" \"a\") (raise msg (String (ref msg 'code))))",
        "regex/re-match-invalid-pattern", pass, fail);
    test_str_val(interp, "re-find-all malformed pattern raises",
        "(handle (re-find-all \"a)\" \"a\") (raise msg (String (ref msg 'code))))",
        "regex/re-find-all-invalid-pattern", pass, fail);
    test_str_val(interp, "re-replace malformed pattern raises",
        "(handle (re-replace \"a)\" \"x\" \"a\") (raise msg (String (ref msg 'code))))",
        "regex/re-replace-invalid-pattern", pass, fail);
    test_str_val(interp, "re-match-pos malformed pattern raises",
        "(handle (re-match-pos \"a)\" \"a\") (raise msg (String (ref msg 'code))))",
        "regex/re-match-pos-invalid-pattern", pass, fail);
    test_str_val(interp, "re-find-all-pos malformed pattern raises",
        "(handle (re-find-all-pos \"a)\" \"a\") (raise msg (String (ref msg 'code))))",
        "regex/re-find-all-pos-invalid-pattern", pass, fail);

    // Pika grammar: seed/epsilon edge + unreachable-rule pruning
    test_eq(interp, "pika/grammar prunes unreachable rules",
        "(block (pika/grammar 'g-prune '(rule start \"a\") '(rule dead \"z\")) (length (pika/grammar-rules 'g-prune)))",
        1, pass, fail);
    test_tag(interp, "pika/parse epsilon seq matches empty input",
        "(block (pika/grammar 'g-eps '(rule start (seq)) '(rule dead \"x\")) (pika/parse 'g-eps \"\"))",
        CONS, pass, fail);
    test_tag(interp, "pika/parse reachable start still matches",
        "(block (pika/grammar 'g-start '(rule start \"a\") '(rule dead \"b\")) (pika/parse 'g-start \"a\"))",
        CONS, pass, fail);
    test_str_val(interp, "pika/grammar start uses first declared rule with forward refs",
        "(block (define g (pika/grammar 'g-forward-start "
        "  '(rule start (seq number op number)) "
        "  '(rule number (scan \"[0-9]+\")) "
        "  '(rule op (first \"+\" \"-\" \"*\" \"/\")))) "
        "  (String (car (pika/parse g \"1+2\"))))",
        "start", pass, fail);
    test_str_val(interp, "pika/grammar rejects unresolved rule reference",
        "(handle (pika/grammar 'g-undefined-rule '(rule start missing)) (raise msg (String (ref msg 'code))))",
        "parser/grammar-undefined-rule", pass, fail);
    test_str_val(interp, "pika/grammar rejects duplicate grammar name",
        "(handle (block (pika/grammar 'g-duplicate-name '(rule start \"a\")) (pika/grammar 'g-duplicate-name '(rule start \"b\"))) (raise msg (String (ref msg 'code))))",
        "parser/grammar-duplicate-name", pass, fail);
    test_str_val(interp, "pika/grammar rejects duplicate rule name",
        "(handle (pika/grammar 'g-duplicate-rule '(rule start \"a\") '(rule start \"b\")) (raise msg (String (ref msg 'code))))",
        "parser/grammar-duplicate-rule", pass, fail);
    test_tag(interp, "pika/parse scan clause basic",
        "(block (pika/grammar 'g-scan '(rule start (scan \"[a-z]+\"))) (pika/parse 'g-scan \"abc\"))",
        CONS, pass, fail);
    test_str_val(interp, "pika/parse scan clause quantifier consumes run",
        "(block (pika/grammar 'g-scan-plus '(rule start (scan \"[a-z]+\"))) (car (cdr (pika/parse 'g-scan-plus \"abc123\"))))",
        "abc", pass, fail);
    test_str_val(interp, "pika/parse scan clause negated class",
        "(block (pika/grammar 'g-scan-neg '(rule start (scan \"[^0-9]+\"))) (car (cdr (pika/parse 'g-scan-neg \"abc123\"))))",
        "abc", pass, fail);
    test_str_val(interp, "pika/parse scan clause shorthand class",
        "(block (pika/grammar 'g-scan-d '(rule start (scan \"[\\\\d]+\"))) (car (cdr (pika/parse 'g-scan-d \"123abc\"))))",
        "123", pass, fail);
    test_error_contains(interp, "pika/grammar scan rejects invalid class",
        "(pika/grammar 'g-scan-bad '(rule start (scan \"[]\")))",
        "invalid scan pattern", pass, fail);
    test_error_contains(interp, "pika/grammar many rejects extra operand",
        "(pika/grammar 'g-many-extra '(rule start (many \"a\" \"b\")))",
        "many expects exactly one operand", pass, fail);
    test_error_contains(interp, "pika/grammar scan rejects extra operand",
        "(pika/grammar 'g-scan-extra '(rule start (scan \"[a]\" \"b\")))",
        "scan expects exactly one string operand", pass, fail);
    test_error_contains(interp, "pika/grammar rejects improper clause operands",
        "(pika/grammar 'g-improper (list 'rule 'start (cons 'many (cons \"a\" \"b\"))))",
        "grammar clause must be a proper list", pass, fail);
    test_error_contains(interp, "pika/grammar rejects extra rule fields",
        "(pika/grammar 'g-rule-extra '(rule start \"a\" \"b\"))",
        "rule expects exactly name and clause", pass, fail);
    test_error_contains(interp, "pika/grammar rejects empty many",
        "(pika/grammar 'g-many-empty '(rule start (many)))",
        "many expects exactly one operand", pass, fail);
    test_error_contains(interp, "pika/grammar end rejects operands",
        "(pika/grammar 'g-end-extra '(rule start (end \"x\")))",
        "end expects no operands", pass, fail);
    test_tag(interp, "pika/parse scan clauses beyond cap",
        "(block "
        "  (pika/grammar 'g-scan-many "
        "    '(rule start (seq r1 r2 r3 r4 r5 r6 r7 r8 r9)) "
        "    '(rule r1 (scan \"[a]\")) '(rule r2 (scan \"[b]\")) '(rule r3 (scan \"[c]\")) "
        "    '(rule r4 (scan \"[d]\")) '(rule r5 (scan \"[e]\")) '(rule r6 (scan \"[f]\")) "
        "    '(rule r7 (scan \"[g]\")) '(rule r8 (scan \"[h]\")) '(rule r9 (scan \"[i]\"))) "
        "  (pika/parse 'g-scan-many \"abcdefghi\"))",
        CONS, pass, fail);
    test_str_val(interp, "pika/parse-lisp grammar drift fails closed",
        "(handle (pika/parse-lisp \"#_(+ 1 2) 3\") (raise msg (String (ref msg 'code))))",
        "parser/parse-lisp-grammar-drift", pass, fail);
    test_tag(interp, "pika/parse-lisp canonical postfix index accepted",
        "(pika/parse-lisp \"a.[0]\")",
        CONS, pass, fail);
    test_tag(interp, "pika/parse-lisp bare postfix index rejected",
        "(pika/parse-lisp \"a[0]\")",
        NIL, pass, fail);
    test_str_val(interp, "pika/parse-lisp utf8 symbol drift fails closed",
        "(handle (pika/parse-lisp \"\xce\xbb\") (raise msg (String (ref msg 'code))))",
        "parser/parse-lisp-grammar-drift", pass, fail);
    {
        bool saved_force = g_value_force_make_string_alloc_fail;
        g_value_force_make_string_alloc_fail = true;
        EvalResult r = run("(pika/parse-lisp \"(+ 1 2)\")", interp);
        g_value_force_make_string_alloc_fail = saved_force;
        bool ok = r.error.has_error || (r.value != null && r.value.tag == ERROR);
        if (ok) {
            emit_pass_literal("[PASS] pika/parse-lisp parse-tree string allocation fails closed");
            (*pass)++;
        } else {
            io::printn("[FAIL] pika/parse-lisp parse-tree string allocation fails closed");
            (*fail)++;
        }
    }
    {
        int saved_force = g_value_force_make_cons_or_error_alloc_fail_nth;
        g_value_force_make_cons_or_error_alloc_fail_nth = 1;
        EvalResult r = run(
            "(block "
            "  (pika/grammar 'g-parse-oom '(rule start \"a\")) "
            "  (pika/parse 'g-parse-oom \"a\"))",
            interp
        );
        g_value_force_make_cons_or_error_alloc_fail_nth = saved_force;
        bool ok = r.error.has_error || (r.value != null && r.value.tag == ERROR);
        if (ok) {
            emit_pass_literal("[PASS] pika/parse parse-tree cons allocation fails closed");
            (*pass)++;
        } else {
            io::printn("[FAIL] pika/parse parse-tree cons allocation fails closed");
            (*fail)++;
        }
    }
    test_eq(interp, "pika/eval-lisp literal",
        "(pika/eval-lisp \"42\")",
        42, pass, fail);
    test_eq(interp, "pika/eval-lisp expression",
        "(pika/eval-lisp \"(+ 1 2)\")",
        3, pass, fail);
    test_str_val(interp, "pika/eval-lisp grammar drift fails closed",
        "(handle (pika/eval-lisp \"#_(+ 1 2) 3\") (raise msg (String (ref msg 'code))))",
        "parser/eval-lisp-grammar-drift", pass, fail);
    test_str_val(interp, "pika/eval-lisp invalid input raises",
        "(handle (pika/eval-lisp \"(\") (raise msg (String (ref msg 'code))))",
        "parser/eval-lisp-invalid-input", pass, fail);
    test_str_val(interp, "pika/eval-lisp type mismatch raises",
        "(handle (pika/eval-lisp 42) (raise msg (String (ref msg 'code))))",
        "parser/eval-lisp-input-not-string", pass, fail);
    {
        pika::RuleBuilder rb = pika::rb_new();
        int[] seq_children = mem::new_array(int, 40);
        for (int i = 0; i < 40; i++) {
            seq_children[i] = rb.add("tok", pika::mk_token('a'));
        }
        rb.add("start", pika::mk_seq(seq_children));

        String[] starts = mem::new_array(String, 1);
        starts[0] = "start";
        pika::RuleEntry[] rules = rb.to_array();
        pika::Grammar grammar = pika::make_grammar(starts, rules);
        mem::free(starts.ptr);
        defer pika::regex_release_grammar_allocations(&grammar);
        defer pika::regex_release_rule_builder(&rb, true);

        char[64] input_buf;
        for (int i = 0; i < 40; i++) input_buf[i] = 'a';
        char[] input = input_buf[:40];
        pika::ParserState st = pika::parse_simple(&grammar, input);
        defer pika::pika_release_parser_state(&st);
        int top_match = pika::find_match_at(&st, "start", 0);

        bool parse_tree_ok = top_match >= 0;
        if (parse_tree_ok) {
            Value* tree = pika::parse_tree_to_lisp(&st, &grammar, top_match, input, interp);
            int child_count = -1;
            if (tree != null &&
                tree.tag == CONS &&
                tree.cons_val.cdr != null &&
                tree.cons_val.cdr.tag == CONS &&
                tree.cons_val.cdr.cons_val.cdr != null &&
                tree.cons_val.cdr.cons_val.cdr.tag == CONS) {
                Value* children_list = tree.cons_val.cdr.cons_val.cdr.cons_val.car;
                child_count = 0;
                Value* cur = children_list;
                while (is_cons(cur)) {
                    child_count++;
                    cur = cur.cons_val.cdr;
                }
            }
            parse_tree_ok = child_count == 40;
        }

        if (parse_tree_ok) {
            emit_pass_literal("[PASS] pika/parse-tree direct start match preserves >32 children");
            (*pass)++;
        } else {
            io::printn("[FAIL] pika/parse-tree direct start match preserves >32 children");
            (*fail)++;
        }

        bool fold_ok = top_match >= 0;
        if (fold_ok) {
            EvalResult fold_fn_result = run(
                "(lambda (name) (lambda (text) (lambda (children) (length children))))",
                interp
            );
            if (fold_fn_result.error.has_error || fold_fn_result.value == null) {
                fold_ok = false;
            } else {
                Value* folded = pika::fold_parse_tree(&st, &grammar, top_match, input, fold_fn_result.value, interp);
                fold_ok = folded != null && folded.tag == INT && folded.int_val == 40;
            }
        }

        if (fold_ok) {
            emit_pass_literal("[PASS] pika/fold direct start match preserves >32 children");
            (*pass)++;
        } else {
            io::printn("[FAIL] pika/fold direct start match preserves >32 children");
            (*fail)++;
        }
    }

    // Compiled/public contract checks (direct C3 API).
    {
        char[] pattern = "ab+c";
        char[] input = "zzabbbczz";
        bool usable = false;
        pika::RegexMatch compiled = pika::regex_search_compiled(pattern, input, &usable);
        pika::RegexMatch api = pika::regex_search(pattern, input);
        bool ok = usable
            && compiled.matched == api.matched
            && compiled.start == api.start
            && compiled.end == api.end;
        if (ok) {
            emit_pass_literal("[PASS] regex compiled/public contract basic");
            (*pass)++;
        } else {
            io::printn("[FAIL] regex compiled/public contract basic");
            (*fail)++;
        }
    }
    {
        char[] pattern = "^abc$";
        char[] input = "abc";
        bool usable = false;
        pika::RegexMatch compiled = pika::regex_fullmatch_compiled(pattern, input, &usable);
        pika::RegexMatch api = pika::regex_fullmatch(pattern, input);
        bool ok = usable
            && compiled.matched == api.matched
            && compiled.start == api.start
            && compiled.end == api.end;
        if (ok) {
            emit_pass_literal("[PASS] regex compiled/public contract anchors");
            (*pass)++;
        } else {
            io::printn("[FAIL] regex compiled/public contract anchors");
            (*fail)++;
        }
    }
    {
        char[] pattern = "a{2,4}";
        char[] input = "aaab";
        bool usable = false;
        pika::RegexMatch compiled = pika::regex_search_compiled(pattern, input, &usable);
        pika::RegexMatch api = pika::regex_search(pattern, input);
        bool ok = usable
            && compiled.matched == api.matched
            && compiled.start == api.start
            && compiled.end == api.end;
        if (ok) {
            emit_pass_literal("[PASS] regex compiled/public contract bounded quantifier");
            (*pass)++;
        } else {
            io::printn("[FAIL] regex compiled/public contract bounded quantifier");
            (*fail)++;
        }
    }
    {
        pika::regex_cache_reset();
        usz before = pika::g_regex_fallback_uses;
        usz before_compile_unusable = pika::g_regex_compiled_unusable_compile;
        usz before_cache_unusable = pika::g_regex_compiled_unusable_cache;
        bool usable = false;
        pika::RegexMatch compiled = pika::regex_search_compiled("a)", "a", &usable);
        pika::RegexMatch api = pika::regex_search("a)", "a");
        bool ok = !usable
            && !compiled.matched
            && !api.matched
            && pika::g_regex_fallback_uses == before + 1
            && pika::g_regex_compiled_unusable_compile >= before_compile_unusable + 1
            && pika::g_regex_compiled_unusable_cache == before_cache_unusable;
        if (ok) {
            emit_pass_literal("[PASS] regex public API reports unusable compiled path and explicit cause counters");
            (*pass)++;
        } else {
            io::printn("[FAIL] regex public API reports unusable compiled path and explicit cause counters");
            (*fail)++;
        }
    }
    {
        pika::regex_cache_reset();
        pika::g_regex_force_cache_alloc_fail_nth = 1;
        bool usable = false;
        pika::RegexMatch compiled = pika::regex_search_compiled("cache-init-fail", "cache-init-fail", &usable);
        bool ok = usable &&
            compiled.matched &&
            pika::g_regex_cache_len == 0 &&
            pika::g_regex_cache_cap == 0;
        if (ok) {
            emit_pass_literal("[PASS] regex cache init allocation failure falls back uncached");
            (*pass)++;
        } else {
            io::printn("[FAIL] regex cache init allocation failure falls back uncached");
            (*fail)++;
        }
    }
    {
        pika::regex_cache_reset();
        bool ok = true;
        for (int i = 0; i < 32; i++) {
            char[32] pattern_buf;
            if (try char[] pattern = io::bprintf(&pattern_buf, "cache-fill-%02d", i)) {
                bool usable = false;
                pika::RegexMatch compiled = pika::regex_search_compiled(pattern, pattern, &usable);
                ok = usable && compiled.matched;
            } else {
                ok = false;
            }
            if (!ok) break;
        }
        if (ok) {
            pika::g_regex_force_cache_alloc_fail_nth = 1;
            char[32] pattern_buf;
            if (try char[] pattern = io::bprintf(&pattern_buf, "cache-grow-fail")) {
                bool usable = false;
                pika::RegexMatch compiled = pika::regex_search_compiled(pattern, pattern, &usable);
                ok = usable &&
                    compiled.matched &&
                    pika::g_regex_cache_len == 32 &&
                    pika::g_regex_cache_cap == 32;
            } else {
                ok = false;
            }
        }
        if (ok) {
            emit_pass_literal("[PASS] regex cache grow allocation failure falls back uncached");
            (*pass)++;
        } else {
            io::printn("[FAIL] regex cache grow allocation failure falls back uncached");
            (*fail)++;
        }
    }
    {
        pika::regex_cache_reset();
        pika::regex_cache_lock();
        pika::CompiledRegex* retained = pika::regex_get_compiled_cached("a+");
        bool retained_ok = retained != null && retained.active_refs == 1 && retained.cached;
        pika::regex_cache_unlock();

        pika::regex_cache_reset();
        bool detached_ok = retained != null && retained.active_refs == 1 && !retained.cached;
        bool match_ok = false;
        if (retained != null) {
            pika::ParserState st = pika::parse_simple(&retained.grammar, "aa");
            defer pika::pika_release_parser_state(&st);
            int mid = pika::lookup_best_match_id(0, retained.start_clause, &st);
            match_ok = mid >= 0 && st.matches[mid].first == 0 && st.matches[mid].last == 1;
        }
        pika::regex_cache_release(retained);

        bool ok = retained_ok && detached_ok && match_ok;
        if (ok) {
            emit_pass_literal("[PASS] regex cache retained compiled regex survives reset until release");
            (*pass)++;
        } else {
            io::printn("[FAIL] regex cache retained compiled regex survives reset until release");
            (*fail)++;
        }
    }
}

fn void run_pika_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_pika_tests, pass, fail);
}
