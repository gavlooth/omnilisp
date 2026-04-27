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

fn bool e2e_generation_quiet_requested() @inline {
    return test_quiet_requested();
}

fn bool e2e_generation_fail(char[] msg) {
    io::printn((ZString)msg);
    return false;
}

fn bool append_line_to_buf(char* buf, usz* pos, usz cap, char[] text) {
    if (*pos + text.len + 1 >= cap) return false;
    foreach (item : text) {
        buf[(*pos)++] = item;
    }
    buf[(*pos)++] = '\n';
    return true;
}

fn bool generate_e2e_run_setups(Interp* e2e_interp) {
    for (usz i = 0; i < E2E_SETUPS.len; i++) {
        EvalResult r = run(E2E_SETUPS[i], e2e_interp);
        if (r.error.has_error) {
            usz shown = r.error.message.len < 256 ? r.error.message.len : 256;
            io::printfn("ERROR: E2E setup %d failed: %s", (int)i, (ZString)r.error.message[:shown]);
            return false;
        }
    }
    return true;
}

fn bool generate_e2e_build_source_prefix(char* source_buf, usz* source_pos, usz buf_size) {
    foreach (item : E2E_SETUPS) {
        if (!append_line_to_buf(source_buf, source_pos, buf_size, item)) return false;
    }
    return append_line_to_buf(source_buf, source_pos, buf_size, "(define __e2e_start__ true)");
}

fn bool generate_e2e_collect_case_set(
    Interp* e2e_interp,
    E2ETestCase[] test_cases,
    char* expected_buf,
    usz* expected_pos,
    char* source_buf,
    usz* source_pos,
    usz buf_size,
    int* test_count
) {
    foreach (test_case : test_cases) {
        EvalResult r = run(test_case.expr, e2e_interp);
        if (r.error.has_error) {
            usz shown = r.error.message.len < 256 ? r.error.message.len : 256;
            io::printfn(
                "ERROR: E2E case '%s' failed during expected-output generation: %s",
                (ZString)test_case.name,
                (ZString)r.error.message[:shown]
            );
            return false;
        }

        char[1024] val_buf;
        usz val_len = print_value_to_buf(r.value, &e2e_interp.symbols, &val_buf, val_buf.len);
        if (val_len + 1 >= val_buf.len) {
            io::printfn("ERROR: E2E rendered value too large for '%s'", (ZString)test_case.name);
            return false;
        }
        if (!append_line_to_buf(expected_buf, expected_pos, buf_size, val_buf[:val_len])) {
            io::printfn("ERROR: E2E expected buffer exhausted at '%s'", (ZString)test_case.name);
            return false;
        }
        if (!append_line_to_buf(source_buf, source_pos, buf_size, test_case.expr)) {
            io::printfn("ERROR: E2E source buffer exhausted at '%s'", (ZString)test_case.name);
            return false;
        }
        (*test_count)++;
    }
    return true;
}

fn bool generate_e2e_collect_cases(
    Interp* e2e_interp,
    char* expected_buf,
    usz* expected_pos,
    char* source_buf,
    usz* source_pos,
    usz buf_size,
    int* test_count
) {
    if (!generate_e2e_collect_case_set(
        e2e_interp,
        E2E_TESTS_CORE,
        expected_buf,
        expected_pos,
        source_buf,
        source_pos,
        buf_size,
        test_count)) return false;

    return generate_e2e_collect_case_set(
        e2e_interp,
        E2E_TESTS_EXTENDED,
        expected_buf,
        expected_pos,
        source_buf,
        source_pos,
        buf_size,
        test_count);
}

fn bool write_text_file(char[] path, char[] text) {
    if (try file = io::file::open((String)path, "w")) {
        char[] remaining = text;
        bool write_ok = true;
        while (write_ok && remaining.len > 0) {
            usz? written_opt = file.write(remaining);
            if (catch err = written_opt) {
                write_ok = false;
                break;
            }
            if (written_opt == 0) {
                write_ok = false;
                break;
            }
            remaining = remaining[written_opt..];
        }
        if (catch err = file.close()) write_ok = false;
        return write_ok;
    }
    return false;
}

fn bool generate_e2e_tests(Interp* e2e_interp) {
    if (!e2e_generation_quiet_requested()) {
        io::printn("=== Generating E2E Compiler Tests ===");
    }
    if (e2e_interp == null) return e2e_generation_fail("ERROR: runtime bootstrap failed");
    if (!generate_e2e_run_setups(e2e_interp)) return false;

    usz buf_size = 131072;
    char* expected_buf = (char*)mem::malloc(buf_size);
    char* source_buf = (char*)mem::malloc(buf_size);
    if (expected_buf == null || source_buf == null) {
        if (expected_buf != null) mem::free(expected_buf);
        if (source_buf != null) mem::free(source_buf);
        return e2e_generation_fail("ERROR: Cannot allocate E2E buffers");
    }
    defer {
        mem::free(expected_buf);
        mem::free(source_buf);
    }

    usz expected_pos = 0;
    usz source_pos = 0;
    int test_count = 0;
    if (!generate_e2e_build_source_prefix(source_buf, &source_pos, buf_size)) {
        return e2e_generation_fail("ERROR: E2E source buffer exhausted while emitting setup prefix");
    }
    if (!generate_e2e_collect_cases(
        e2e_interp,
        expected_buf, &expected_pos,
        source_buf, &source_pos,
        buf_size,
        &test_count)) {
        return false;
    }

    if (!e2e_generation_quiet_requested()) {
        io::printfn("  Compiling %d test expressions...", test_count);
    }
    char[] compile_error = "";
    char[] c3_code = compile_to_c3_ext(source_buf[:source_pos], e2e_interp, false, true, &compile_error);
    if (c3_code.len == 0) {
        if (compile_error.len > 0) {
            usz shown = compile_error.len < 256 ? compile_error.len : 256;
            io::printfn("ERROR: E2E compilation failed: %s", (ZString)compile_error[:shown]);
        }
        return e2e_generation_fail("ERROR: Compilation produced empty output");
    }

    if (!write_text_file("build/e2e_test.c3", c3_code)) {
        return e2e_generation_fail("ERROR: Cannot write build/e2e_test.c3");
    }
    if (!e2e_generation_quiet_requested()) {
        io::printfn("  Wrote build/e2e_test.c3 (%d bytes)", (int)c3_code.len);
    }

    char[] expected_out = expected_buf[:expected_pos];
    if (!write_text_file("build/e2e_expected.txt", expected_out)) {
        return e2e_generation_fail("ERROR: Cannot write build/e2e_expected.txt");
    }
    if (!e2e_generation_quiet_requested()) {
        io::printfn("  Wrote build/e2e_expected.txt (%d lines)", test_count);
    }
    io::printfn("=== Generated %d e2e tests ===", test_count);
    return true;
}
