module lisp;

import std::io;

const usz AST_ARENA_BENCH_EXPR_ALLOCS = 131072;
const usz AST_ARENA_BENCH_PATTERN_ALLOCS = 65536;
const usz AST_ARENA_BENCH_RESET_INTERVAL = 8192;

fn bool ast_arena_bench_enabled() @inline {
    return boundary_c_getenv("OMNI_AST_ARENA_BENCH") != null;
}

fn bool ast_arena_bench_assertions_enabled() @inline {
    return boundary_c_getenv("OMNI_AST_ARENA_BENCH_ASSERT") != null;
}

fn bool ast_arena_bench_mutation_enabled() @inline {
    return boundary_c_getenv("OMNI_AST_ARENA_BENCH_MUT") != null;
}

fn bool ast_arena_bench_reset_enabled() @inline {
    return boundary_c_getenv("OMNI_AST_ARENA_BENCH_RESET") != null;
}

macro ast_arena_bench_summary_alloc(expr_iters, expr_ok, expr_ms, pattern_iters, pattern_ok, pattern_ms, resets, blocks, cap, ast_valid_fail) {
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=ast_arena_alloc expr_allocs=%d expr_ok=%d expr_ms=%d pattern_allocs=%d pattern_ok=%d pattern_ms=%d arena_blocks=%d arena_capacity=%d ast_resets=%d ast_validation_fail=%d",
        (long)(expr_iters),
        (long)(expr_ok),
        (long)(expr_ms),
        (long)(pattern_iters),
        (long)(pattern_ok),
        (long)(pattern_ms),
        (long)(resets),
        (long)(blocks),
        (long)(cap),
        (long)(ast_valid_fail)
    );
}

macro ast_arena_bench_summary_alloc_reset(expr_iters, expr_ok, expr_ms, pattern_iters, pattern_ok, pattern_ms, resets, blocks, cap, ast_valid_fail) {
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=ast_arena_alloc_reset expr_allocs=%d expr_ok=%d expr_ms=%d pattern_allocs=%d pattern_ok=%d pattern_ms=%d arena_blocks=%d arena_capacity=%d ast_resets=%d ast_validation_fail=%d",
        (long)(expr_iters),
        (long)(expr_ok),
        (long)(expr_ms),
        (long)(pattern_iters),
        (long)(pattern_ok),
        (long)(pattern_ms),
        (long)(resets),
        (long)(blocks),
        (long)(cap),
        (long)(ast_valid_fail)
    );
}

macro ast_arena_bench_summary_parser(parse_iters, parse_ms, parse_ok, parse_failed, parse_owned_failed, parse_mut_failed, parse_exprs, ast_blocks, parse_resets, ast_valid_fail) {
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=ast_parser_smoke iters=%d parse_ms=%d parse_ok=%d parse_failed=%d parse_owned_failed=%d parse_mut_failed=%d exprs=%d ast_blocks=%d ast_resets=%d ast_validation_fail=%d",
        (long)(parse_iters),
        (long)(parse_ms),
        (long)(parse_ok),
        (long)(parse_failed),
        (long)(parse_owned_failed),
        (long)(parse_mut_failed),
        (long)(parse_exprs),
        (long)(ast_blocks),
        (long)(parse_resets),
        (long)(ast_valid_fail)
    );
}

macro ast_arena_bench_summary_parser_reset(parse_iters, parse_ms, parse_ok, parse_failed, parse_owned_failed, parse_mut_failed, parse_exprs, ast_blocks, parse_resets, ast_valid_fail) {
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=ast_parser_smoke_reset iters=%d parse_ms=%d parse_ok=%d parse_failed=%d parse_owned_failed=%d parse_mut_failed=%d exprs=%d ast_blocks=%d ast_resets=%d ast_validation_fail=%d",
        (long)(parse_iters),
        (long)(parse_ms),
        (long)(parse_ok),
        (long)(parse_failed),
        (long)(parse_owned_failed),
        (long)(parse_mut_failed),
        (long)(parse_exprs),
        (long)(ast_blocks),
        (long)(parse_resets),
        (long)(ast_valid_fail)
    );
}

macro ast_arena_bench_summary_compile(
    compile_iters,
    compile_ms,
    compile_ok,
    compile_failed,
    compile_empty,
    compile_owned_failed,
    output_bytes,
    ast_blocks,
    compile_resets,
    ast_valid_fail
) {
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=ast_compiler_smoke iters=%d compile_ms=%d compile_ok=%d compile_failed=%d compile_empty=%d compile_owned_failed=%d output_bytes=%d ast_blocks=%d ast_resets=%d ast_validation_fail=%d",
        (long)(compile_iters),
        (long)(compile_ms),
        (long)(compile_ok),
        (long)(compile_failed),
        (long)(compile_empty),
        (long)(compile_owned_failed),
        (long)(output_bytes),
        (long)(ast_blocks),
        (long)(compile_resets),
        (long)(ast_valid_fail)
    );
}

macro ast_arena_bench_summary_compile_reset(
    compile_iters,
    compile_ms,
    compile_ok,
    compile_failed,
    compile_empty,
    compile_owned_failed,
    output_bytes,
    ast_blocks,
    compile_resets,
    ast_valid_fail
) {
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=ast_compiler_smoke_reset iters=%d compile_ms=%d compile_ok=%d compile_failed=%d compile_empty=%d compile_owned_failed=%d output_bytes=%d ast_blocks=%d ast_resets=%d ast_validation_fail=%d",
        (long)(compile_iters),
        (long)(compile_ms),
        (long)(compile_ok),
        (long)(compile_failed),
        (long)(compile_empty),
        (long)(compile_owned_failed),
        (long)(output_bytes),
        (long)(ast_blocks),
        (long)(compile_resets),
        (long)(ast_valid_fail)
    );
}

macro ast_arena_bench_summary_macro(macro_iters, macro_ms, macro_ok, macro_failed, macro_not_expanded, macro_owned_failed, macro_mutation_failed, macro_resets, ast_valid_fail) {
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=ast_macro_smoke iters=%d macro_ms=%d macro_ok=%d macro_failed=%d macro_not_expanded=%d macro_owned_failed=%d macro_mutation_failed=%d ast_resets=%d ast_validation_fail=%d",
        (long)(macro_iters),
        (long)(macro_ms),
        (long)(macro_ok),
        (long)(macro_failed),
        (long)(macro_not_expanded),
        (long)(macro_owned_failed),
        (long)(macro_mutation_failed),
        (long)(macro_resets),
        (long)(ast_valid_fail)
    );
}

macro ast_arena_bench_summary_macro_reset(macro_iters, macro_ms, macro_ok, macro_failed, macro_not_expanded, macro_owned_failed, macro_mutation_failed, macro_resets, ast_valid_fail) {
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=ast_macro_smoke_reset iters=%d macro_ms=%d macro_ok=%d macro_failed=%d macro_not_expanded=%d macro_owned_failed=%d macro_mutation_failed=%d ast_resets=%d ast_validation_fail=%d",
        (long)(macro_iters),
        (long)(macro_ms),
        (long)(macro_ok),
        (long)(macro_failed),
        (long)(macro_not_expanded),
        (long)(macro_owned_failed),
        (long)(macro_mutation_failed),
        (long)(macro_resets),
        (long)(ast_valid_fail)
    );
}

fn char[] ast_arena_bench_parser_source(usz i) @inline {
    switch (i & 3) {
        case 0: return "(let (x 10 y 20) (+ x y))";
        case 1: return "(match (list 1 2 3) ([a b c] (+ a (+ b c))))";
        case 2: return "(define [macro] double (syntax-match ([x] (template (+ (insert x) (insert x))))))";
        default: return "(cond ((> 7 3) (+ 1 2)) (else (- 3 1)))";
    }
}

fn char[] ast_arena_bench_compile_source(usz i) @inline {
    switch (i & 3) {
        case 0: return "(define (square x) (* x x)) (square 9)";
        case 1: return "(define [macro] double (syntax-match ([x] (template (+ (insert x) (insert x)))))) (double 21)";
        case 2: return "(block (define total 0) (if (> 3 1) (set! total (+ total 1)) 0) total)";
        default: return "(let (fib (lambda (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))) ) (fib 10))";
    }
}

fn char[] ast_arena_bench_macro_source(usz i) @inline {
    switch (i & 3) {
        case 0: return "(block (define [macro] double (syntax-match ([x] (template (+ (insert x) (insert x)))))) (double 6))";
        case 1: return "(block (define [macro] inc (syntax-match ([x] (template (+ (insert x) 1))))) (inc 41))";
        case 2: return "(block (define [macro] swap (syntax-match ([a b] (template (list (insert b) (insert a)))))) (swap 1 2))";
        default: return "(block (define [macro] when-positive (syntax-match ([x expr] (template (if (> (insert x) 0) (insert expr) 0))))) (when-positive 3 7))";
    }
}

fn SymbolId ast_arena_bench_macro_sym(Interp* interp, usz i) @inline {
    switch (i & 3) {
        case 0: return interp.symbols.intern("double");
        case 1: return interp.symbols.intern("inc");
        case 2: return interp.symbols.intern("swap");
        default: return interp.symbols.intern("when-positive");
    }
}

fn Expr* ast_arena_bench_mutate_expr(Expr* expr) @inline {
    if (expr == null) return null;
    expr.loc_line = expr.loc_line + 1;
    return expr;
}

fn bool ast_arena_bench_macro_expanded(Expr* root, SymbolId macro_sym) @inline {
    if (root == null) return false;

    Expr* target = root;
    if (root.tag == E_BLOCK && root.block != null && root.block.expr_count > 0) {
        target = root.block.exprs[root.block.expr_count - 1];
    }

    if (target == null) return false;
    if (target.tag != E_CALL) return true;
    if (target.call == null) return false;
    if (target.call.func == null) return false;
    if (target.call.func.tag != E_VAR) return true;
    return target.call.func.var_expr.name != macro_sym;
}

struct AstArenaBenchAllocResult {
    long expr_ms;
    long pattern_ms;
    usz expr_ok;
    usz pattern_ok;
    usz ast_resets;
    usz expr_count;
    usz pattern_count;
    usz ast_validation_failures;
}

struct AstArenaBenchCompileResult {
    bool parse_ok;
    bool compile_ok;
    bool ast_owned;
    bool mutated;
    usz output_len;
    Expr* parsed;
}

struct AstArenaBenchParserSuiteResult {
    long parse_ms;
    usz parse_ok;
    usz parse_failed;
    usz parse_owned_failed;
    usz parse_mutation_failed;
    usz exprs;
    usz ast_blocks;
    usz ast_resets;
    usz ast_validation_failures;
}

struct AstArenaBenchCompileSuiteResult {
    long compile_ms;
    usz compile_ok;
    usz compile_failed;
    usz compile_empty;
    usz compile_owned_failed;
    usz compile_mutation_failed;
    usz output_bytes;
    usz ast_blocks;
    usz ast_resets;
    usz ast_validation_failures;
}

struct AstArenaBenchMacroSuiteResult {
    long macro_ms;
    usz macro_ok;
    usz macro_failed;
    usz macro_not_expanded;
    usz macro_owned_failed;
    usz macro_mutation_failed;
    usz ast_blocks;
    usz ast_resets;
    usz ast_validation_failures;
}

fn bool ast_arena_bench_should_reset(usz iter, bool reset_enabled) @inline {
    if (!reset_enabled) return false;
    if (AST_ARENA_BENCH_RESET_INTERVAL == 0) return false;
    return iter > 0 && (iter % AST_ARENA_BENCH_RESET_INTERVAL) == 0;
}

fn AstArenaBenchAllocResult ast_arena_bench_run_alloc(
    AstArena* arena,
    usz expr_count,
    usz pattern_count,
    bool reset_mode
) {
    AstArenaBenchAllocResult result = {
        .expr_count = expr_count,
        .pattern_count = pattern_count,
    };

    long start_ms = boundary_bench_now_ms();
    for (usz i = 0; i < expr_count; i++) {
        if (ast_arena_bench_should_reset(i, reset_mode)) {
            ast_arena_destroy(arena);
            ast_arena_init(arena);
            result.ast_resets++;
        }
        Expr* expr = (Expr*)ast_arena_alloc(arena, Expr.sizeof);
        if (expr != null) result.expr_ok++;
    }
    long expr_done_ms = boundary_bench_now_ms();
    for (usz i = 0; i < pattern_count; i++) {
        if (ast_arena_bench_should_reset(AST_ARENA_BENCH_EXPR_ALLOCS + i, reset_mode)) {
            ast_arena_destroy(arena);
            ast_arena_init(arena);
            result.ast_resets++;
        }
        Pattern* pattern = (Pattern*)ast_arena_alloc(arena, Pattern.sizeof);
        if (pattern != null) result.pattern_ok++;
    }
    long end_ms = boundary_bench_now_ms();

    result.expr_ms = expr_done_ms > start_ms ? expr_done_ms - start_ms : 0;
    result.pattern_ms = end_ms > expr_done_ms ? end_ms - expr_done_ms : 0;
    result.ast_validation_failures = ast_arena_validate(arena) ? 0 : 1;
    return result;
}

fn AstArenaBenchParserSuiteResult ast_arena_bench_run_parser_suite(
    Interp* interp,
    bool collect_assertions,
    bool collect_mutation,
    bool reset_mode
) {
    AstArenaBenchParserSuiteResult result = {};

    long start_ms = boundary_bench_now_ms();
    for (usz i = 0; i < AST_ARENA_BENCH_EXPR_ALLOCS; i++) {
        if (ast_arena_bench_should_reset(i, reset_mode)) {
            ast_arena_destroy(&interp.ast_arena);
            ast_arena_init(&interp.ast_arena);
            result.ast_resets++;
        }

        char[] source = ast_arena_bench_parser_source(i);
        Expr* parsed = (Expr*)parse(source, interp);
        if (parsed == null) {
            result.parse_failed++;
            continue;
        }

        bool parse_ok_status = true;
        if (collect_assertions) {
            if (!ast_arena_contains(&interp.ast_arena, parsed)) {
                result.parse_owned_failed++;
                parse_ok_status = false;
            }
        }

        if (collect_mutation) {
            if (ast_arena_bench_mutate_expr(parsed) == null) {
                result.parse_mutation_failed++;
                parse_ok_status = false;
            }
        }

        result.exprs++;
        if (parse_ok_status) {
            result.parse_ok++;
        }
    }
    long end_ms = boundary_bench_now_ms();
    result.parse_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    result.ast_blocks = interp.ast_arena.count;
    result.ast_validation_failures = ast_arena_validate(&interp.ast_arena) ? 0 : 1;
    return result;
}

fn AstArenaBenchCompileSuiteResult ast_arena_bench_run_compile_suite(
    Interp* interp,
    bool collect_assertions,
    bool collect_mutation,
    bool reset_mode
) {
    AstArenaBenchCompileSuiteResult result = {};

    long start_ms = boundary_bench_now_ms();
    for (usz i = 0; i < AST_ARENA_BENCH_PATTERN_ALLOCS; i++) {
        if (ast_arena_bench_should_reset(i, reset_mode)) {
            ast_arena_destroy(&interp.ast_arena);
            ast_arena_init(&interp.ast_arena);
            result.ast_resets++;
        }

        char[] source = ast_arena_bench_compile_source(i);
        AstArenaBenchCompileResult compile_result = ast_arena_bench_compile_once(interp, source, collect_assertions, collect_mutation);

        if (compile_result.parse_ok && compile_result.ast_owned && compile_result.mutated && compile_result.compile_ok && compile_result.output_len > 0) {
            result.compile_ok++;
            result.output_bytes += compile_result.output_len;
        } else {
            result.compile_failed++;
            if (compile_result.parse_ok) {
                if (!compile_result.ast_owned) result.compile_owned_failed++;
                if (!compile_result.mutated) result.compile_mutation_failed++;
                if (compile_result.output_len == 0) result.compile_empty++;
            }
        }
    }
    long end_ms = boundary_bench_now_ms();
    result.compile_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    result.ast_blocks = interp.ast_arena.count;
    result.ast_validation_failures = ast_arena_validate(&interp.ast_arena) ? 0 : 1;
    return result;
}

fn AstArenaBenchMacroSuiteResult ast_arena_bench_run_macro_suite(
    Interp* interp,
    bool collect_assertions,
    bool collect_mutation,
    bool reset_mode
) {
    AstArenaBenchMacroSuiteResult result = {};

    long start_ms = boundary_bench_now_ms();
    for (usz i = 0; i < AST_ARENA_BENCH_EXPR_ALLOCS; i++) {
        if (ast_arena_bench_should_reset(i, reset_mode)) {
            ast_arena_destroy(&interp.ast_arena);
            ast_arena_init(&interp.ast_arena);
            result.ast_resets++;
        }

        char[] source = ast_arena_bench_macro_source(i);
        SymbolId macro_sym = ast_arena_bench_macro_sym(interp, i);

        Expr* parsed = (Expr*)parse(source, interp);
        if (parsed == null) {
            result.macro_failed++;
            continue;
        }

        bool macro_ok_status = true;
        if (collect_assertions) {
            if (!ast_arena_contains(&interp.ast_arena, parsed)) {
                result.macro_owned_failed++;
                macro_ok_status = false;
            }
        }

        if (collect_mutation) {
            if (ast_arena_bench_mutate_expr(parsed) == null) {
                result.macro_mutation_failed++;
                macro_ok_status = false;
            }
        }

        Expr* expanded = expand_macros_in_expr(parsed, interp);
        bool expanded_ok = ast_arena_bench_macro_expanded(expanded, macro_sym);
        if (!expanded_ok) {
            result.macro_not_expanded++;
            macro_ok_status = false;
        }

        if (macro_ok_status) {
            result.macro_ok++;
        } else {
            result.macro_failed++;
        }
    }
    long end_ms = boundary_bench_now_ms();
    result.macro_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    result.ast_blocks = interp.ast_arena.count;
    result.ast_validation_failures = ast_arena_validate(&interp.ast_arena) ? 0 : 1;
    return result;
}

fn AstArenaBenchCompileResult ast_arena_bench_compile_once(
    Interp* interp,
    char[] source,
    bool collect_assertions,
    bool collect_mutation
) {
    AstArenaBenchCompileResult result = {
        .parse_ok = false,
        .compile_ok = false,
        .ast_owned = true,
        .mutated = true,
        .output_len = 0,
        .parsed = null,
    };

    Expr* parsed = parse(source, interp);
    result.parsed = parsed;
    if (parsed == null) {
        return result;
    }
    result.parse_ok = true;

    if (collect_assertions && !ast_arena_contains(&interp.ast_arena, parsed)) {
        result.ast_owned = false;
        return result;
    }

    if (collect_mutation) {
        result.parsed = ast_arena_bench_mutate_expr(parsed);
        result.mutated = result.parsed != null;
        if (!result.mutated) return result;
    }

    Compiler compiler;
    compiler.init(interp);
    char[] output = compiler.compile_program(source);
    result.output_len = output.len;
    compiler.free();
    result.compile_ok = output.len > 0;
    return result;
}

fn void run_memory_lifetime_ast_arena_benchmark(Interp* interp) {
    if (!ast_arena_bench_enabled()) return;
    if (interp == null) return;

    bool assertions_enabled = ast_arena_bench_assertions_enabled();
    bool mutation_enabled = ast_arena_bench_mutation_enabled();
    bool reset_enabled = ast_arena_bench_reset_enabled();

    Interp bench_interp = {};
    bench_interp.init();
    defer bench_interp.destroy();

    bench_interp.current_scope = bench_interp.root_scope;
    bench_interp.releasing_scope = null;

    AstArena arena = {};
    ast_arena_init(&arena);

    AstArenaBenchAllocResult alloc_result = {
        .expr_count = AST_ARENA_BENCH_EXPR_ALLOCS,
        .pattern_count = AST_ARENA_BENCH_PATTERN_ALLOCS,
    };
    AstArenaBenchParserSuiteResult parse_result = {};
    AstArenaBenchParserSuiteResult parse_result_reset = {};
    AstArenaBenchCompileSuiteResult compile_result = {};
    AstArenaBenchCompileSuiteResult compile_result_reset = {};
    AstArenaBenchMacroSuiteResult macro_result = {};
    AstArenaBenchMacroSuiteResult macro_result_reset = {};

    ast_arena_destroy(&bench_interp.ast_arena);
    ast_arena_init(&bench_interp.ast_arena);
    parse_result = ast_arena_bench_run_parser_suite(&bench_interp, assertions_enabled, mutation_enabled, false);
    if (reset_enabled) {
        ast_arena_destroy(&bench_interp.ast_arena);
        ast_arena_init(&bench_interp.ast_arena);
        parse_result_reset = ast_arena_bench_run_parser_suite(&bench_interp, assertions_enabled, mutation_enabled, true);
    }
    ast_arena_destroy(&bench_interp.ast_arena);
    ast_arena_init(&bench_interp.ast_arena);

    ast_arena_destroy(&bench_interp.ast_arena);
    ast_arena_init(&bench_interp.ast_arena);
    compile_result = ast_arena_bench_run_compile_suite(&bench_interp, assertions_enabled, mutation_enabled, false);
    if (reset_enabled) {
        ast_arena_destroy(&bench_interp.ast_arena);
        ast_arena_init(&bench_interp.ast_arena);
        compile_result_reset = ast_arena_bench_run_compile_suite(&bench_interp, assertions_enabled, mutation_enabled, true);
    }
    ast_arena_destroy(&bench_interp.ast_arena);
    ast_arena_init(&bench_interp.ast_arena);

    ast_arena_destroy(&bench_interp.ast_arena);
    ast_arena_init(&bench_interp.ast_arena);
    macro_result = ast_arena_bench_run_macro_suite(&bench_interp, assertions_enabled, mutation_enabled, false);
    if (reset_enabled) {
        ast_arena_destroy(&bench_interp.ast_arena);
        ast_arena_init(&bench_interp.ast_arena);
        macro_result_reset = ast_arena_bench_run_macro_suite(&bench_interp, assertions_enabled, mutation_enabled, true);
    }
    ast_arena_destroy(&bench_interp.ast_arena);
    ast_arena_init(&bench_interp.ast_arena);

    alloc_result = ast_arena_bench_run_alloc(
        &arena,
        AST_ARENA_BENCH_EXPR_ALLOCS,
        AST_ARENA_BENCH_PATTERN_ALLOCS,
        false
    );
    usz alloc_result_blocks = arena.count;
    usz alloc_result_capacity = arena.capacity;
    if (reset_enabled) {
        AstArenaBenchAllocResult alloc_result_reset = ast_arena_bench_run_alloc(
            &arena,
            AST_ARENA_BENCH_EXPR_ALLOCS,
            AST_ARENA_BENCH_PATTERN_ALLOCS,
            true
        );
        ast_arena_bench_summary_alloc_reset(
            alloc_result_reset.expr_count,
            alloc_result_reset.expr_ok,
            alloc_result_reset.expr_ms,
            alloc_result_reset.pattern_count,
            alloc_result_reset.pattern_ok,
            alloc_result_reset.pattern_ms,
            alloc_result_reset.ast_resets,
            arena.count,
            arena.capacity,
            alloc_result_reset.ast_validation_failures
        );
        ast_arena_destroy(&arena);
        ast_arena_init(&arena);
    }

    ast_arena_bench_summary_alloc(
        alloc_result.expr_count,
        alloc_result.expr_ok,
        alloc_result.expr_ms,
        alloc_result.pattern_count,
        alloc_result.pattern_ok,
        alloc_result.pattern_ms,
        alloc_result.ast_resets,
        alloc_result_blocks,
        alloc_result_capacity,
        alloc_result.ast_validation_failures
    );
    ast_arena_bench_summary_parser(
        AST_ARENA_BENCH_EXPR_ALLOCS,
        parse_result.parse_ms,
        parse_result.parse_ok,
        parse_result.parse_failed,
        parse_result.parse_owned_failed,
        parse_result.parse_mutation_failed,
        parse_result.exprs,
        parse_result.ast_blocks,
        parse_result.ast_resets,
        parse_result.ast_validation_failures
    );
    if (reset_enabled) {
        ast_arena_bench_summary_parser_reset(
            AST_ARENA_BENCH_EXPR_ALLOCS,
            parse_result_reset.parse_ms,
            parse_result_reset.parse_ok,
            parse_result_reset.parse_failed,
            parse_result_reset.parse_owned_failed,
            parse_result_reset.parse_mutation_failed,
            parse_result_reset.exprs,
            parse_result_reset.ast_blocks,
            parse_result_reset.ast_resets,
            parse_result_reset.ast_validation_failures
        );
    }
    ast_arena_bench_summary_compile(
        AST_ARENA_BENCH_PATTERN_ALLOCS,
        compile_result.compile_ms,
        compile_result.compile_ok,
        compile_result.compile_failed,
        compile_result.compile_empty,
        compile_result.compile_owned_failed,
        compile_result.output_bytes,
        compile_result.ast_blocks,
        compile_result.ast_resets,
        compile_result.ast_validation_failures
    );
    if (reset_enabled) {
        ast_arena_bench_summary_compile_reset(
            AST_ARENA_BENCH_PATTERN_ALLOCS,
            compile_result_reset.compile_ms,
            compile_result_reset.compile_ok,
            compile_result_reset.compile_failed,
            compile_result_reset.compile_empty,
            compile_result_reset.compile_owned_failed,
            compile_result_reset.output_bytes,
            compile_result_reset.ast_blocks,
            compile_result_reset.ast_resets,
            compile_result_reset.ast_validation_failures
        );
    }
    ast_arena_bench_summary_macro(
        AST_ARENA_BENCH_EXPR_ALLOCS,
        macro_result.macro_ms,
        macro_result.macro_ok,
        macro_result.macro_failed,
        macro_result.macro_not_expanded,
        macro_result.macro_owned_failed,
        macro_result.macro_mutation_failed,
        macro_result.ast_resets,
        macro_result.ast_validation_failures
    );
    if (reset_enabled) {
        ast_arena_bench_summary_macro_reset(
            AST_ARENA_BENCH_EXPR_ALLOCS,
            macro_result_reset.macro_ms,
            macro_result_reset.macro_ok,
            macro_result_reset.macro_failed,
            macro_result_reset.macro_not_expanded,
            macro_result_reset.macro_owned_failed,
            macro_result_reset.macro_mutation_failed,
            macro_result_reset.ast_resets,
            macro_result_reset.ast_validation_failures
        );
    }

    ast_arena_destroy(&arena);
    ast_arena_destroy(&bench_interp.ast_arena);
}
