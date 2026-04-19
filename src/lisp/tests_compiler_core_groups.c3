module lisp;


import std::collections::list;
import std::core::mem;
import std::io;
import main;
import lisp::aot;

fn bool compiler_str_eq(char[] left, char[] right) {
    if (left.len != right.len) return false;
    for (usz i = 0; i < left.len; i++) {
        if (left[i] != right[i]) return false;
    }
    return true;
}

fn bool compiler_serializes_to_roundtrip(Interp* interp, char[] source, char[] expected) {
    Lexer lex;
    lex.init(source);
    defer lex.destroy();

    Parser p;
    p.init(&lex, interp);
    Expr* expr = p.parse_expr();
    if (expr == null || p.has_error) {
        io::printfn("[DEBUG] serializer source parse failed: %s", (ZString)source);
        return false;
    }

    Compiler compiler;
    compiler.init(interp);
    defer compiler.free();

    List{char} buf;
    defer buf.free();
    compiler.serialize_expr_to_buf(expr, &buf);

    char[] actual = "";
    if (buf.len() > 0) {
        actual = buf.entries[:buf.len()];
    }
    if (!compiler_str_eq(actual, expected)) {
        io::print("[DEBUG] serializer expected: ");
        io::printn(expected);
        io::print("[DEBUG] serializer actual:   ");
        io::printn(actual);
        return false;
    }

    Expr* reparsed = parse(actual, interp);
    if (reparsed == null) {
        io::printfn("[DEBUG] serializer reparse failed: %s", (ZString)actual);
        return false;
    }
    return true;
}

fn Value* compiler_aot_test_unary_invoke(void* data, Value* arg) {
    return arg != null ? arg : aot::make_nil();
}

fn Value* compiler_aot_test_variadic_invoke(void* data, Value* arg_list) {
    return arg_list != null ? arg_list : aot::make_nil();
}

fn Value* compiler_aot_test_exact_pair_sum(void* data, Value* arg_list) {
    usz arg_count = 0;
    Value* count_error = aot::count_arg_list(arg_list, &arg_count);
    if (count_error != null) return count_error;
    if (arg_count < 2) return aot::make_arity_error(2, arg_count);

    Value* curr = arg_list;
    Value* x = aot::car(curr);
    curr = aot::cdr(curr);
    Value* y = aot::car(curr);
    curr = aot::cdr(curr);

    long x_val = x != null && x.tag == ValueTag.INT ? x.int_val : 0;
    long y_val = y != null && y.tag == ValueTag.INT ? y.int_val : 0;
    Value* result = aot::make_int(x_val + y_val);
    if (arg_count > 2) return aot::apply_multi(result, curr, (long)(arg_count - 2));
    return result;
}

struct CompilerAotSumData {
    Value* first;
}

fn bool compiler_error_value_contains(Value* v, char[] needle) {
    return v != null && v.tag == ValueTag.ERROR && str_contains(v.str_chars[:v.str_len], needle);
}

fn Value* compiler_aot_test_sum_second(void* data, Value* arg) {
    CompilerAotSumData* sum_data = (CompilerAotSumData*)data;
    long first = 0;
    if (sum_data != null && sum_data.first != null && sum_data.first.tag == ValueTag.INT) {
        first = sum_data.first.int_val;
    }
    long second = arg != null && arg.tag == ValueTag.INT ? arg.int_val : 0;
    if (sum_data != null) mem::free(sum_data);
    return aot::make_int(first + second);
}

fn Value* compiler_aot_test_capture_first(void* data, Value* arg) {
    CompilerAotSumData* sum_data = (CompilerAotSumData*)mem::malloc(CompilerAotSumData.sizeof);
    if (sum_data == null) return aot::make_error("compiler test: failed to allocate sum payload");
    sum_data.first = arg;
    return aot::make_closure(sum_data, &compiler_aot_test_sum_second);
}

fn Value* compiler_aot_test_signal_my_eff(void* data, Value* arg) {
    return aot::compiled_signal(aot::make_symbol("my-eff"), aot::make_int(42));
}

fn Value* compiler_aot_test_other_eff_handler(void* data, Value* pair) {
    if (pair != null && pair.tag == ValueTag.CONS) return pair.cons_val.cdr;
    return aot::make_nil();
}

fn Value* compiler_aot_test_constant_zero(void* data, Value* arg) {
    return aot::make_int(0);
}

fn void run_compiler_group_syntax_begin_zero_tests(Interp* interp, usz* pass, usz* fail) {
    // --- E_BLOCK tests ---
    {
        char[] code = compile_to_c3("(block 42)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: (block 42)", "[FAIL] Compiler: (block 42)", pass, fail);
    }
    {
        char[] code = compile_to_c3("(block 1 2 3)", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "make_int(3)");
        compiler_test_finish_bool(ok, "Compiler: (block 1 2 3)", "[FAIL] Compiler: (block 1 2 3)", pass, fail);
    }
    {
        char[] code = compile_to_c3("(block (define x 1) x)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: (block (define x 1) x)", "[FAIL] Compiler: (block (define x 1) x)", pass, fail);
    }
    {
        char[] code = compile_to_c3("(lambda (x) (block x x))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: (lambda (x) (block x x))", "[FAIL] Compiler: (lambda (x) (block x x))", pass, fail);
    }

    // --- Zero-arg lambda tests ---
    {
        char[] code = compile_to_c3("((lambda () 42))", interp);
        bool ok = !str_contains(code, "unsupported") && str_contains(code, "_unused");
        compiler_test_finish_bool(ok, "Compiler: ((lambda () 42))", "[FAIL] Compiler: ((lambda () 42))", pass, fail);
    }
    {
        char[] code = compile_to_c3("(define thunk (lambda () 42)) (thunk)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: zero-arg define + call", "[FAIL] Compiler: zero-arg define + call", pass, fail);
    }
    {
        char[] code = compile_to_c3("(let (f (lambda () 10)) (f))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: zero-arg in let", "[FAIL] Compiler: zero-arg in let", pass, fail);
    }
}

fn void run_compiler_group_syntax_multiarg_tests(Interp* interp, usz* pass, usz* fail) {
    // --- Multi-param + multi-binding (parser desugar) ---
    {
        char[] code = compile_to_c3("((lambda (x y) (+ x y)) 3 4)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: multi-param lambda", "[FAIL] Compiler: multi-param lambda", pass, fail);
    }
    {
        char[] code = compile_to_c3("(let (x 1 y 2) (+ x y))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: multi-binding let", "[FAIL] Compiler: multi-binding let", pass, fail);
    }
    {
        char[] code = compile_to_c3("((lambda (a b c) a) 1 2 3)", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: 3-param lambda", "[FAIL] Compiler: 3-param lambda", pass, fail);
    }
}

fn void run_compiler_group_syntax_basics(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_syntax_begin_zero_tests(interp, &pass, &fail);
    run_compiler_group_syntax_multiarg_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}

fn void run_compiler_group_stdlib_availability_tests(Interp* interp, usz* pass, usz* fail) {
    // --- Stdlib availability ---
    {
        char[] code = compile_to_c3("(map (lambda (x) (+ x 1)) (list 1))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: map", "[FAIL] Compiler: map", pass, fail);
    }
    {
        char[] code = compile_to_c3("(filter (lambda (x) (> x 2)) (list 1 2 3))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: filter", "[FAIL] Compiler: filter", pass, fail);
    }
    {
        char[] code = compile_to_c3("(foldl + 0 (list 1 2 3))", interp);
        bool ok = !str_contains(code, "unsupported");
        compiler_test_finish_bool(ok, "Compiler: foldl", "[FAIL] Compiler: foldl", pass, fail);
    }
}


fn void run_compiler_group_stdlib_existing(Interp* interp, usz* pass_out, usz* fail_out) {
    usz pass = 0;
    usz fail = 0;
    run_compiler_group_allocation_failure_seam_tests(interp, &pass, &fail);
    run_compiler_group_stdlib_availability_tests(interp, &pass, &fail);
    run_compiler_group_existing_feature_tests(interp, &pass, &fail);
    run_compiler_group_aot_runtime_parity_tests(interp, &pass, &fail);
    run_compiler_group_type_dispatch_bridge_tests(interp, &pass, &fail);
    run_compiler_group_serializer_roundtrip_tests(interp, &pass, &fail);
    run_compiler_group_foreign_handle_metadata_dict_tests(interp, &pass, &fail);
    *pass_out += pass;
    *fail_out += fail;
}
