module lisp;

import std::core::mem;
import std::io;

// =============================================================================
// E2E COMPILER TEST GENERATION
// =============================================================================

struct E2ETestCase {
    char[] name;
    char[] expr;
}

fn void append_line_to_buf(char* buf, usz* pos, usz cap, char[] text) {
    for (usz i = 0; i < text.len && (*pos) < cap - 2; i++) {
        buf[(*pos)++] = text[i];
    }
    if ((*pos) < cap - 1) buf[(*pos)++] = '\n';
}

fn void generate_e2e_run_setups(Interp* e2e_interp) {
    for (usz i = 0; i < E2E_SETUPS.len; i++) {
        EvalResult r = run(E2E_SETUPS[i], e2e_interp);
        if (r.error.has_error) {
            io::printfn("WARNING: Setup %d failed, skipping", (int)i);
        }
    }
}

fn void generate_e2e_build_source_prefix(char* source_buf, usz* source_pos, usz buf_size) {
    for (usz i = 0; i < E2E_SETUPS.len; i++) {
        append_line_to_buf(source_buf, source_pos, buf_size, E2E_SETUPS[i]);
    }
    append_line_to_buf(source_buf, source_pos, buf_size, "(define __e2e_start__ true)");
}

fn void generate_e2e_collect_case_set(
    Interp* e2e_interp,
    E2ETestCase[] test_cases,
    char* expected_buf,
    usz* expected_pos,
    char* source_buf,
    usz* source_pos,
    usz buf_size,
    int* test_count,
    int* skip_count
) {
    for (usz i = 0; i < test_cases.len; i++) {
        EvalResult r = run(test_cases[i].expr, e2e_interp);
        if (r.error.has_error) {
            io::printfn("  SKIP (interp error): %s", (ZString)test_cases[i].name);
            (*skip_count)++;
            continue;
        }

        char[1024] val_buf;
        usz val_len = print_value_to_buf(r.value, &e2e_interp.symbols, &val_buf, val_buf.len);
        append_line_to_buf(expected_buf, expected_pos, buf_size, val_buf[:val_len]);
        append_line_to_buf(source_buf, source_pos, buf_size, test_cases[i].expr);
        (*test_count)++;
    }
}

fn void generate_e2e_collect_cases(
    Interp* e2e_interp,
    char* expected_buf,
    usz* expected_pos,
    char* source_buf,
    usz* source_pos,
    usz buf_size,
    int* test_count,
    int* skip_count
) {
    generate_e2e_collect_case_set(
        e2e_interp,
        E2E_TESTS_CORE,
        expected_buf,
        expected_pos,
        source_buf,
        source_pos,
        buf_size,
        test_count,
        skip_count);

    generate_e2e_collect_case_set(
        e2e_interp,
        E2E_TESTS_EXTENDED,
        expected_buf,
        expected_pos,
        source_buf,
        source_pos,
        buf_size,
        test_count,
        skip_count);
}

fn bool write_text_file(char[] path, char[] text) {
    if (try file = io::file::open((String)path, "w")) {
        defer (void)file.close();
        file.write(text)!!;
        return true;
    }
    return false;
}

fn void generate_e2e_tests(Interp* _interp) {
    (void)_interp;
    io::printn("=== Generating E2E Compiler Tests ===");

    Interp* e2e_interp = (Interp*)mem::malloc(Interp.sizeof);
    defer {
        e2e_interp.destroy();
        mem::free(e2e_interp);
    }
    e2e_interp.init();
    register_primitives(e2e_interp);
    register_stdlib(e2e_interp);
    generate_e2e_run_setups(e2e_interp);

    usz buf_size = 131072;
    char* expected_buf = (char*)mem::malloc(buf_size);
    char* source_buf = (char*)mem::malloc(buf_size);
    defer {
        mem::free(expected_buf);
        mem::free(source_buf);
    }

    usz expected_pos = 0;
    usz source_pos = 0;
    int test_count = 0;
    int skip_count = 0;
    generate_e2e_build_source_prefix(source_buf, &source_pos, buf_size);
    generate_e2e_collect_cases(
        e2e_interp,
        expected_buf, &expected_pos,
        source_buf, &source_pos,
        buf_size,
        &test_count, &skip_count);

    io::printfn("  Compiling %d test expressions...", test_count);
    char[] c3_code = compile_to_c3_ext(source_buf[:source_pos], e2e_interp, false, true);
    if (c3_code.len == 0) {
        io::printn("ERROR: Compilation produced empty output");
        return;
    }

    if (!write_text_file("build/e2e_test.c3", c3_code)) {
        io::printn("ERROR: Cannot write build/e2e_test.c3");
        return;
    }
    io::printfn("  Wrote build/e2e_test.c3 (%d bytes)", (int)c3_code.len);

    char[] expected_out = expected_buf[:expected_pos];
    if (!write_text_file("build/e2e_expected.txt", expected_out)) {
        io::printn("ERROR: Cannot write build/e2e_expected.txt");
        return;
    }
    io::printfn("  Wrote build/e2e_expected.txt (%d lines)", test_count);
    io::printfn("=== Generated %d e2e tests (%d skipped) ===", test_count, skip_count);
}
