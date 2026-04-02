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
    test_tag(interp, "re-match start anchor rejects non-zero start",
        "(re-match \"^abc\" \"xabc\")", NIL, pass, fail);
    test_str_val(interp, "re-match end anchor matches suffix at end",
        "(re-match \"abc$\" \"xabc\")", "abc", pass, fail);
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
            "      (let (p (string-append \"(\" (number->string i) \"|x)\") "
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
            "      (let (p (string-append \"(\" (number->string i) \"|x)\") "
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

    // Regex: re-split
    test_eq(interp, "re-split count",
        "(length (re-split \" \" \"hello world foo\"))", 3, pass, fail);
    test_str_val(interp, "re-split first",
        "(car (re-split \" \" \"hello world\"))", "hello", pass, fail);

    // Regex: re-replace
    test_str_val(interp, "re-replace first",
        "(re-replace \"[0-9]+\" \"X\" \"a1b2c3\")", "aXb2c3", pass, fail);
    test_str(interp, "re-replace global returns string",
        "(re-replace \"a\" \"X\" \"abaca\" 'global)", pass, fail);
    test_eq(interp, "re-replace first preserves >1024 output",
        "(string-byte-length (re-replace \"a\" (string-repeat \"b\" 1500) \"a\"))", 1500, pass, fail);
    test_eq(interp, "re-replace global preserves >1024 output",
        "(string-byte-length (re-replace \"a\" \"abcdef\" (string-repeat \"a\" 300) 'global))", 1800, pass, fail);

    // Regex: re-match-pos
    test_eq(interp, "re-match-pos start",
        "(car (re-match-pos \"hello\" \"say hello\"))", 4, pass, fail);

    // Regex: re-find-all-pos
    test_eq(interp, "re-find-all-pos count",
        "(length (re-find-all-pos \"a\" \"abaca\"))", 3, pass, fail);

    // Regex: escaped chars
    test_str(interp, "re-match \\d",
        "(re-match \"\\\\d+\" \"abc123\")", pass, fail);
    test_str(interp, "re-match \\w+",
        "(re-match \"\\\\w+\" \"hello_world\")", pass, fail);

    // Regex: strict grammar validation
    test_str_val(interp, "re-fullmatch unmatched close paren raises",
        "(handle (re-fullmatch \"a)\" \"a\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch unmatched open paren raises",
        "(handle (re-fullmatch \"(ab\" \"ab\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch leading quantifier raises",
        "(handle (re-fullmatch \"*a\" \"a\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch trailing quantifier raises",
        "(handle (re-fullmatch \"a**\" \"a\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch unknown group type raises",
        "(handle (re-fullmatch \"(?q)a\" \"a\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch malformed bounded quantifier missing min raises",
        "(handle (re-fullmatch \"a{,2}\" \"aa\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch malformed bounded quantifier upper raises",
        "(handle (re-fullmatch \"a{2,x}\" \"aa\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_tag(interp, "re-fullmatch supports bounded quantifier >4096",
        "(re-fullmatch \"a{1,5000}\" \"a\")", STRING, pass, fail);
    test_str_val(interp, "re-fullmatch bounded quantifier overflow upper raises",
        "(handle (re-fullmatch \"a{1,2147483648}\" \"a\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch bounded quantifier overflow lower raises",
        "(handle (re-fullmatch \"a{2147483648}\" \"a\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch descending bounded quantifier raises",
        "(handle (re-fullmatch \"a{3,2}\" \"aaa\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch empty char class raises",
        "(handle (re-fullmatch \"[]\" \"a\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch invalid class range raises",
        "(handle (re-fullmatch \"[z-a]\" \"z\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch trailing escape raises",
        "(handle (re-fullmatch \"\\\\\" \"a\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-fullmatch trailing token after end anchor raises",
        "(handle (re-fullmatch \"a$)\" \"a\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-fullmatch-invalid-pattern", pass, fail);
    test_str_val(interp, "re-match malformed pattern raises",
        "(handle (re-match \"a)\" \"a\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-match-invalid-pattern", pass, fail);
    test_str_val(interp, "re-find-all malformed pattern raises",
        "(handle (re-find-all \"a)\" \"a\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-find-all-invalid-pattern", pass, fail);
    test_str_val(interp, "re-replace malformed pattern raises",
        "(handle (re-replace \"a)\" \"x\" \"a\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-replace-invalid-pattern", pass, fail);
    test_str_val(interp, "re-match-pos malformed pattern raises",
        "(handle (re-match-pos \"a)\" \"a\") (raise msg (symbol->string (ref msg 'code))))",
        "regex/re-match-pos-invalid-pattern", pass, fail);
    test_str_val(interp, "re-find-all-pos malformed pattern raises",
        "(handle (re-find-all-pos \"a)\" \"a\") (raise msg (symbol->string (ref msg 'code))))",
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
        "(handle (pika/parse-lisp \"#_(+ 1 2) 3\") (raise msg (symbol->string (ref msg 'code))))",
        "parser/parse-lisp-grammar-drift", pass, fail);
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

        char[64] input_buf;
        for (int i = 0; i < 40; i++) input_buf[i] = 'a';
        char[] input = input_buf[:40];
        pika::ParserState st = pika::parse_simple(&grammar, input);
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
}

fn void run_pika_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_pika_tests, pass, fail);
}
