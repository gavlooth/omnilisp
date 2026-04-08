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
