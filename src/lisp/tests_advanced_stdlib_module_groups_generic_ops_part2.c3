module lisp;

import std::io;

import main;

fn void run_advanced_collections_generic_ops_part2(Interp* interp, int* pass, int* fail) {
    {
        char[] name = "map vulkan mixed cpu lazy operand rejects before cpu materialization";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float64)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map + (map (lambda (x) 'bad) (Tensor Float64 [1] [1.0])) (to-device (Tensor Float64 [1] [2.0]) 'vulkan))",
                "map: Vulkan operands must remain Vulkan-placed", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "map vulkan unsupported binary callable rejects before cpu materialization";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float64)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map floor (map (lambda (x) 'bad) (Tensor Float64 [1] [1.0])) (to-device (Tensor Float64 [1] [2.0]) 'vulkan))",
                "map: Vulkan currently supports Float64 and Float32 arithmetic kernels", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    test_truthy(interp, "map vulkan scientific unary Float32 family roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (and (< (abs (ref (to-device (map sin (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0])) 0.0001) (< (abs (- (ref (to-device (map cos (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0]) 1.0)) 0.0001) (< (abs (ref (to-device (map tan (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0])) 0.0001) (< (abs (ref (to-device (map asin (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0])) 0.0001) (< (abs (ref (to-device (map acos (to-device (Tensor Float32 [1] [1.0]) 'vulkan)) 'cpu) [0])) 0.0001) (< (abs (ref (to-device (map atan (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0])) 0.0001) (< (abs (ref (to-device (map sinh (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0])) 0.0001) (< (abs (- (ref (to-device (map cosh (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0]) 1.0)) 0.0001) (< (abs (ref (to-device (map tanh (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0])) 0.0001) (< (abs (- (ref (to-device (map exp (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0]) 1.0)) 0.0001) (< (abs (ref (to-device (map log (to-device (Tensor Float32 [1] [1.0]) 'vulkan)) 'cpu) [0])) 0.0001) (< (abs (- (ref (to-device (map log10 (to-device (Tensor Float32 [1] [100.0]) 'vulkan)) 'cpu) [0]) 2.0)) 0.0001)) true) true))", pass, fail);
    test_truthy(interp, "map vulkan stats.normal-cdf Float32 roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (out (map stats.normal-cdf (to-device (Tensor Float32 [1] [1.0]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (< (abs (- (ref (to-device out 'cpu) [0]) 0.8413447)) 0.0002))) true) true))", pass, fail);
    test_truthy(interp, "map vulkan stats.normal-cdf Float64 preserves placement and values when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (out (map stats.normal-cdf (to-device (Tensor Float64 [3] [-1.0 0.0 1.0]) 'vulkan))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (< (abs (- (ref cpu [0]) 0.158655253931457)) 0.000001) (< (abs (- (ref cpu [1]) 0.5)) 0.000001) (< (abs (- (ref cpu [2]) 0.841344746068543)) 0.000001)))) true) true))", pass, fail);
    test_truthy(interp, "map vulkan math.erf Float32 preserves placement and dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (out (map math.erf (to-device (Tensor Float32 [2] [0.0 1.0]) 'vulkan))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (< (abs (ref cpu [0])) 0.0001) (< (abs (- (ref cpu [1]) 0.8427008)) 0.0002)))) true) true))", pass, fail);
    test_truthy(interp, "map vulkan math.erfc Float32 preserves placement and dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (out (map math.erfc (to-device (Tensor Float32 [2] [0.0 1.0]) 'vulkan))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (< (abs (- (ref cpu [0]) 1.0)) 0.0001) (< (abs (- (ref cpu [1]) 0.15729921)) 0.0002)))) true) true))", pass, fail);
    {
        char[] name = "map vulkan math.erf Float64 remains fail-closed";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float64)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map math.erf (to-device (Tensor Float64 [1] [1.0]) 'vulkan))",
                "map: Vulkan currently supports dense row-major Float64 and Float32 unary operations", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    test_truthy(interp, "sin vulkan tensor roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (< (abs (ref (to-device (sin (to-device (Tensor Float32 [1] [0.0]) 'vulkan)) 'cpu) [0])) 0.0001) true) true))", pass, fail);
    test_truthy(interp, "direct Float32 log10 vulkan tensor preserves placement and dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (out (log10 (to-device (Tensor Float32 [1] [100.0]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (< (abs (- (ref (to-device out 'cpu) [0]) 2.0)) 0.0001))) true) true))", pass, fail);
    test_truthy(interp, "direct stats.normal-cdf vulkan Float32 preserves placement and dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (out (stats.normal-cdf (to-device (Tensor Float32 [1] [0.0]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (< (abs (- (ref (to-device out 'cpu) [0]) 0.5)) 0.0002))) true) true))", pass, fail);
    test_truthy(interp, "direct stats.normal-cdf vulkan Float64 preserves placement and dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (let (out (stats.normal-cdf (to-device (Tensor Float64 [1] [1.0]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (< (abs (- (ref (to-device out 'cpu) [0]) 0.841344746068543)) 0.000001))) true) true))", pass, fail);
    test_truthy(interp, "direct math.erf vulkan Float32 preserves placement and dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (out (math.erf (to-device (Tensor Float32 [1] [1.0]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (< (abs (- (ref (to-device out 'cpu) [0]) 0.8427008)) 0.0002))) true) true))", pass, fail);
    test_truthy(interp, "direct math.erfc vulkan Float32 preserves placement and dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (out (math.erfc (to-device (Tensor Float32 [1] [1.0]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (< (abs (- (ref (to-device out 'cpu) [0]) 0.15729921)) 0.0002))) true) true))", pass, fail);
    test_truthy(interp, "direct stats.normal-quantile vulkan Float32 preserves placement dtype and values when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (out (stats.normal-quantile (to-device (Tensor Float32 [3] [0.025 0.5 0.975]) 'vulkan))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (< (abs (- (ref cpu [0]) -1.959964)) 0.0005) (< (abs (ref cpu [1])) 0.0002) (< (abs (- (ref cpu [2]) 1.959964)) 0.0005)))) true) true))", pass, fail);
    test_truthy(interp, "map vulkan stats.normal-quantile Float32 preserves placement dtype and values when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (let (out (map stats.normal-quantile (to-device (Tensor Float32 [3] [0.025 0.5 0.975]) 'vulkan))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float32\") (< (abs (- (ref cpu [0]) -1.959964)) 0.0005) (< (abs (ref cpu [1])) 0.0002) (< (abs (- (ref cpu [2]) 1.959964)) 0.0005)))) true) true))", pass, fail);
    {
        char[] name = "direct stats.normal-quantile vulkan Float32 rejects zero probability without CPU fallback";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float32)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(stats.normal-quantile (to-device (Tensor Float32 [1] [0.0]) 'vulkan))",
                "stats.normal-quantile: probability must be between 0 and 1", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "map vulkan stats.normal-quantile Float32 rejects one probability without CPU fallback";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float32)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map stats.normal-quantile (to-device (Tensor Float32 [1] [1.0]) 'vulkan))",
                "stats.normal-quantile: probability must be between 0 and 1", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "direct stats.normal-quantile vulkan Float32 rejects vulkan-produced non-finite probability";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float32)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(stats.normal-quantile (sqrt (to-device (Tensor Float32 [1] [-1.0]) 'vulkan)))",
                "stats.normal-quantile: expected finite numeric input", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "direct stats.normal-quantile vulkan Float32 gives non-finite status priority";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float32)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(stats.normal-quantile (sqrt (to-device (Tensor Float32 [2] [0.0 -1.0]) 'vulkan)))",
                "stats.normal-quantile: expected finite numeric input", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        EvalResult support = run("(let (vulkan-backend (ref (tensor-backends) 3)) (if (and (ref vulkan-backend 'available) (ref vulkan-backend 'float64)) (handle (let (out (stats.normal-quantile (to-device (Tensor Float64 [1] [0.5]) 'vulkan))) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (< (abs (ref (to-device out 'cpu) [0])) 0.000001))) (raise msg false)) false))", interp);
        bool enabled = !support.error.has_error && !is_nil(support.value) && !(is_symbol(support.value) && support.value.sym_val == interp.sym_false);
        if (enabled) {
            test_truthy(interp, "direct stats.normal-quantile vulkan Float64 preserves placement dtype and values when available",
                "(let (out (stats.normal-quantile (to-device (Tensor Float64 [3] [0.025 0.5 0.975]) 'vulkan))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (< (abs (- (ref cpu [0]) -1.95996398454005)) 0.00001) (< (abs (ref cpu [1])) 0.000001) (< (abs (- (ref cpu [2]) 1.95996398454005)) 0.00001))))", pass, fail);
            test_truthy(interp, "map vulkan stats.normal-quantile Float64 preserves placement dtype and values when available",
                "(let (out (map stats.normal-quantile (to-device (Tensor Float64 [3] [0.025 0.5 0.975]) 'vulkan))) (let (cpu (to-device out 'cpu)) (and (= (device out) 'vulkan) (= (String (dtype out)) \"Float64\") (< (abs (- (ref cpu [0]) -1.95996398454005)) 0.00001) (< (abs (ref cpu [1])) 0.000001) (< (abs (- (ref cpu [2]) 1.95996398454005)) 0.00001))))", pass, fail);
            test_error_contains(interp, "direct stats.normal-quantile vulkan Float64 rejects zero probability without CPU fallback",
                "(stats.normal-quantile (to-device (Tensor Float64 [1] [0.0]) 'vulkan))",
                "stats.normal-quantile: probability must be between 0 and 1", pass, fail);
            test_error_contains(interp, "map vulkan stats.normal-quantile Float64 rejects one probability without CPU fallback",
                "(map stats.normal-quantile (to-device (Tensor Float64 [1] [1.0]) 'vulkan))",
                "stats.normal-quantile: probability must be between 0 and 1", pass, fail);
            test_error_contains(interp, "direct stats.normal-quantile vulkan Float64 rejects vulkan-produced non-finite probability",
                "(stats.normal-quantile (sqrt (to-device (Tensor Float64 [1] [-1.0]) 'vulkan)))",
                "stats.normal-quantile: expected finite numeric input", pass, fail);
            test_error_contains(interp, "direct stats.normal-quantile vulkan Float64 gives non-finite status priority",
                "(stats.normal-quantile (sqrt (to-device (Tensor Float64 [2] [0.0 -1.0]) 'vulkan)))",
                "stats.normal-quantile: expected finite numeric input", pass, fail);
        } else {
            EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float64)))", interp);
            bool can_check_failclosed = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
            if (can_check_failclosed) {
                test_error_contains(interp, "direct stats.normal-quantile vulkan Float64 remains fail-closed when unavailable",
                    "(stats.normal-quantile (to-device (Tensor Float64 [1] [0.5]) 'vulkan))",
                    "stats.normal-quantile: Vulkan currently supports dense row-major Float32 tensors", pass, fail);
                test_error_contains(interp, "map vulkan stats.normal-quantile Float64 remains fail-closed when unavailable",
                    "(map stats.normal-quantile (to-device (Tensor Float64 [1] [0.5]) 'vulkan))",
                    "stats.normal-quantile: Vulkan currently supports dense row-major Float32 tensors", pass, fail);
            } else {
                emit_pass("direct stats.normal-quantile vulkan Float64 remains fail-closed when unavailable");
                (*pass)++;
                emit_pass("map vulkan stats.normal-quantile Float64 remains fail-closed when unavailable");
                (*pass)++;
            }
        }
    }
    {
        test_truthy(interp, "realize lazy cpu map succeeds inside handle stack",
            "(= (ref (handle (realize (map + (Tensor Float64 [2] [1.25 2.75]) 0.0)) (raise msg (ref msg 'message))) [1]) 2.75)", pass, fail);
        test_error_contains(interp, "realize propagates single-arg source errors",
            "(realize (/ 1 0))", "/: division by zero", pass, fail);
        test_error_contains(interp, "realize destination propagates source errors",
            "(realize (/ 1 0) (Tensor Float64 [1] 0.0))", "/: division by zero", pass, fail);
    }
    {
        char[] name = "map vulkan unsupported callable fails closed";
        char[] handler_code_name = "map vulkan unsupported callable raise handler preserves code";
        char[] handler_message_name = "map vulkan unsupported callable raise handler preserves message";
        char[] lazy_handler_name = "map vulkan unsupported callable lazy source handler preserves code";
        char[] realize_name = "realize vulkan unsupported callable fails closed";
        char[] realize_handler_code_name = "realize vulkan unsupported callable handler preserves code";
        char[] realize_lazy_handler_message_name = "realize vulkan unsupported callable lazy source handler preserves message";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float64)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map floor (to-device (Tensor Float64 [1] [1.2]) 'vulkan))",
                "map: Vulkan currently supports Float64 and Float32 arithmetic kernels", pass, fail);
            test_truthy(interp, handler_code_name,
                "(= (handle (map floor (to-device (Tensor Float64 [1] [1.2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported)", pass, fail);
            test_truthy(interp, handler_message_name,
                "(= (handle (map floor (to-device (Tensor Float64 [1] [1.2]) 'vulkan)) (raise msg (ref msg 'message))) \"map: Vulkan currently supports Float64 and Float32 arithmetic kernels\")", pass, fail);
            test_truthy(interp, lazy_handler_name,
                "(= (handle (map floor (map + (to-device (Tensor Float64 [1] [1.2]) 'vulkan) 0.0)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported)", pass, fail);
            test_error_contains(interp, realize_name,
                "(realize (map floor (to-device (Tensor Float64 [1] [1.2]) 'vulkan)))",
                "map: Vulkan currently supports Float64 and Float32 arithmetic kernels", pass, fail);
            test_truthy(interp, realize_handler_code_name,
                "(= (handle (realize (map floor (to-device (Tensor Float64 [1] [1.2]) 'vulkan))) (raise msg (ref msg 'code))) 'tensor/backend-unsupported)", pass, fail);
            test_truthy(interp, realize_lazy_handler_message_name,
                "(= (handle (realize (map floor (map + (to-device (Tensor Float64 [1] [1.2]) 'vulkan) 0.0))) (raise msg (ref msg 'message))) \"map: Vulkan currently supports Float64 and Float32 arithmetic kernels\")", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
            emit_pass(handler_code_name);
            (*pass)++;
            emit_pass(handler_message_name);
            (*pass)++;
            emit_pass(lazy_handler_name);
            (*pass)++;
            emit_pass(realize_name);
            (*pass)++;
            emit_pass(realize_handler_code_name);
            (*pass)++;
            emit_pass(realize_lazy_handler_message_name);
            (*pass)++;
        }
    }
    {
        char[] name = "minmax vulkan mixed cpu lazy operand rejects before cpu materialization";
        EvalResult guard = run("(let (vulkan-backend (ref (tensor-backends) 3)) (and (ref vulkan-backend 'available) (ref vulkan-backend 'float64)))", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(min (map (lambda (x) 'bad) (Tensor Float64 [1] [1.0])) (to-device (Tensor Float64 [1] [2.0]) 'vulkan))",
                "minmax: Vulkan operands must remain Vulkan-placed", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    test_truthy(interp, "min vulkan tensor scalar roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (min (to-device (Tensor Float64 [3] [1.0 5.0 -2.0]) 'vulkan) 0.0) 'cpu) [0]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "max vulkan scalar tensor roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (max 0.0 (to-device (Tensor Float64 [3] [1.0 -5.0 2.0]) 'vulkan)) 'cpu) [1]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "min vulkan tensor-tensor broadcast roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (min (to-device (Tensor Float64 [2 1] [2.0 3.0]) 'vulkan) (to-device (Tensor Float64 [1 2] [1.0 4.0]) 'vulkan)) 'cpu) [1 1]) 3.0) true) true))", pass, fail);
    test_truthy(interp, "min vulkan Float32 tensor-tensor broadcast roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (min (to-device (Tensor Float32 [2 1] [2.5 3.5]) 'vulkan) (to-device (Tensor Float32 [1 2] [1.5 4.5]) 'vulkan)) 'cpu) [1 1]) 3.5) true) true))", pass, fail);
    test_truthy(interp, "max vulkan tensor scalar returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (max (to-device (Tensor Float64 [2] [-1.0 2.0]) 'vulkan) 0.0)) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "max vulkan Float32 tensor scalar returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (device (max (to-device (Tensor Float32 [2] [-1.25 2.5]) 'vulkan) 0)) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "max vulkan lazy map returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (max (map + (to-device (Tensor Float64 [2] [-1.0 2.0]) 'vulkan) 0.0) 0.0)) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "min vulkan mixed cpu operand fails closed",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (min (to-device (Tensor Float64 [1] [1.0]) 'vulkan) (Tensor Float64 [1] [0.0])) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    {
        char[] name = "direct vulkan rounding returns BigInteger tensor when capability is enabled";
        EvalResult guard = run("(ref (ref (tensor-backends) 3) 'rounding-big-integer)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_truthy(interp, name,
                "(let (f64 (to-device (floor (to-device (Tensor Float64 [2] [3.7 -3.2]) 'vulkan)) 'cpu) f32 (to-device (round (to-device (Tensor Float32 [2] [3.5 -3.5]) 'vulkan)) 'cpu)) (and (= (String (dtype f64)) \"BigInteger\") (= (String (dtype f32)) \"BigInteger\") (= (String (ref f64 [0])) \"3\") (= (String (ref f64 [1])) \"-4\") (= (String (ref f32 [0])) \"4\") (= (String (ref f32 [1])) \"-4\")))", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "direct vulkan ceiling and truncate return BigInteger tensor when capability is enabled";
        EvalResult guard = run("(ref (ref (tensor-backends) 3) 'rounding-big-integer)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_truthy(interp, name,
                "(let (ceil (to-device (ceiling (to-device (Tensor Float64 [2] [3.2 -3.7]) 'vulkan)) 'cpu) trunc (to-device (truncate (to-device (Tensor Float32 [2] [3.9 -3.9]) 'vulkan)) 'cpu)) (and (= (String (dtype ceil)) \"BigInteger\") (= (String (dtype trunc)) \"BigInteger\") (= (String (ref ceil [0])) \"4\") (= (String (ref ceil [1])) \"-3\") (= (String (ref trunc [0])) \"3\") (= (String (ref trunc [1])) \"-3\")))", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    test_truthy(interp, "floor vulkan tensor fails closed without rounding capability",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'rounding-big-integer) true (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (floor (to-device (Tensor Float64 [1] [1.2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true)))", pass, fail);
    test_truthy(interp, "ceiling vulkan tensor fails closed without rounding capability",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'rounding-big-integer) true (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (ceiling (to-device (Tensor Float64 [1] [1.2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true)))", pass, fail);
    test_truthy(interp, "round vulkan tensor fails closed without rounding capability",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'rounding-big-integer) true (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (round (to-device (Tensor Float64 [1] [1.2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true)))", pass, fail);
    test_truthy(interp, "truncate vulkan tensor fails closed without rounding capability",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'rounding-big-integer) true (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (truncate (to-device (Tensor Float64 [1] [1.2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true)))", pass, fail);
    test_truthy(interp, "floor lazy vulkan tensor fails closed without rounding capability",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'rounding-big-integer) true (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (floor (map + (to-device (Tensor Float64 [1] [1.2]) 'vulkan) 0.0)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true)))", pass, fail);
    test_truthy(interp, "floor vulkan Float32 tensor fails closed without rounding capability",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'rounding-big-integer) true (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (handle (floor (to-device (Tensor Float32 [1] [1.2]) 'vulkan)) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true)))", pass, fail);
    test_truthy(interp, "pow vulkan tensor fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (pow (to-device (Tensor Float64 [1] [2.0]) 'vulkan) 2.0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "pow lazy vulkan tensor fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (pow (map + (to-device (Tensor Float64 [1] [2.0]) 'vulkan) 0.0) 2.0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "atan2 vulkan tensor fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (atan2 (to-device (Tensor Float64 [1] [1.0]) 'vulkan) 1.0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "atan2 lazy vulkan tensor fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (atan2 (map + (to-device (Tensor Float64 [1] [1.0]) 'vulkan) 0.0) 1.0) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "gcd vulkan tensor fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (gcd (to-device (Tensor Float64 [1] [2.0]) 'vulkan) 2) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "lcm vulkan tensor fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (lcm (to-device (Tensor Float64 [1] [2.0]) 'vulkan) 2) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "lcm lazy vulkan tensor fails closed when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (lcm (map + (to-device (Tensor Float64 [1] [2.0]) 'vulkan) 0.0) 2) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-1 dot roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [0 0]) 'cpu) []) 140.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-1 dot explicit axes roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [0] [0]) 'cpu) []) 140.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-1 dot returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (contract (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [0 0])) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "contract eager vulkan map operand realizes on vulkan when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (realize (contract (map + (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) 0.0) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [0 0]))) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "contract eager vulkan map operand copies to cpu when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (map + (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) 0.0) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [0 0]) 'cpu) []) 140.0) true) true))", pass, fail);
    test_truthy(interp, "contract eager vulkan Float32 map operand copies to cpu when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float32) (= (ref (to-device (contract (map + (to-device (Tensor Float32 [3] [1 2 3]) 'vulkan) 0.0) (to-device (Tensor Float32 [3] [10 20 30]) 'vulkan) [0 0]) 'cpu) []) 140.0) true) true))", pass, fail);
	    {
	        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
	        bool ok = !vulkan_available;
	        if (vulkan_available) {
            EvalResult left_eval = run("(to-device (Tensor Float64 [3] [1 2 3]) 'vulkan)", interp);
            EvalResult right_eval = run("(to-device (Tensor Float64 [3] [10 20 30]) 'vulkan)", interp);
            SymbolId plus_sym = interp.symbols.intern("+");
            Value* plus_func = interp.global_env.lookup(plus_sym);
            Value* zero = make_double(interp, 0.0);
            usz[1] vector_shape = { 3 };
            usz[1] left_axes = { 0 };
            usz[1] right_axes = { 0 };
            usz[1] scalar_shape = { 0 };
            bool setup_ok = !left_eval.error.has_error && !right_eval.error.has_error &&
                left_eval.value != null && left_eval.value.tag == TENSOR && left_eval.value.tensor_val != null &&
                right_eval.value != null && right_eval.value.tag == TENSOR && right_eval.value.tensor_val != null &&
                plus_func != null && zero != null && zero.tag != ERROR;
            Value* lazy_map = setup_ok ? make_tensor_map_expr(
                plus_func,
                left_eval.value,
                left_eval.value.tensor_val,
                zero,
                null,
                true,
                TENSOR_DTYPE_DOUBLE,
                vector_shape[0:1],
                interp
            ) : null;
            Value* contract_expr = lazy_map != null && lazy_map.tag == TENSOR ? make_tensor_contract_expr(
                lazy_map,
                right_eval.value,
                &left_axes[0],
                &right_axes[0],
                1,
                scalar_shape[0:0],
                interp
            ) : null;
            Value* realized = contract_expr != null && contract_expr.tag == TENSOR ? tensor_realize_to_value(contract_expr, interp) : null;
            Value* cpu_value = realized != null && realized.tag == TENSOR ? tensor_copy_device_to_cpu_value(realized, interp) : null;
            ok = setup_ok &&
                lazy_map != null && lazy_map.tag == TENSOR && lazy_map.tensor_val.kind == TENSOR_PAYLOAD_MAP &&
                contract_expr != null && contract_expr.tag == TENSOR && contract_expr.tensor_val.kind == TENSOR_PAYLOAD_CONTRACT &&
                realized != null && realized.tag == TENSOR && realized.tensor_val != null &&
                realized.tensor_val.kind == TENSOR_PAYLOAD_CONCRETE &&
                realized.tensor_val.device == TENSOR_DEVICE_VULKAN &&
                cpu_value != null && cpu_value.tag == TENSOR && cpu_value.tensor_val != null &&
                cpu_value.tensor_val.data != null &&
                ((double*)cpu_value.tensor_val.data)[0] == 140.0;
        }
        if (ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor lazy Vulkan contract helper realizes mapped operand"
                : "[PASS] tensor lazy Vulkan contract helper unavailable; mapped operand not exercised");
            *pass += 1;
        } else {
	            io::printn("[FAIL] tensor lazy Vulkan contract helper realizes mapped operand");
	            *fail += 1;
	        }
	    }
	    {
	        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
	        bool ok = !vulkan_available;
	        if (vulkan_available) {
	            EvalResult left_eval = run("(to-device (Tensor Float64 [2] [1 2]) 'vulkan)", interp);
	            EvalResult right_eval = run("(to-device (Tensor Float64 [3] [10 20 30]) 'vulkan)", interp);
	            SymbolId plus_sym = interp.symbols.intern("+");
	            Value* plus_func = interp.global_env.lookup(plus_sym);
	            Value* zero = make_double(interp, 0.0);
	            usz[1] vector_shape = { 2 };
	            usz[2] result_shape = { 2, 3 };
	            bool setup_ok = !left_eval.error.has_error && !right_eval.error.has_error &&
	                left_eval.value != null && left_eval.value.tag == TENSOR && left_eval.value.tensor_val != null &&
	                right_eval.value != null && right_eval.value.tag == TENSOR && right_eval.value.tensor_val != null &&
	                plus_func != null && zero != null && zero.tag != ERROR;
	            Value* lazy_map = setup_ok ? make_tensor_map_expr(
	                plus_func,
	                left_eval.value,
	                left_eval.value.tensor_val,
	                zero,
	                null,
	                true,
	                TENSOR_DTYPE_DOUBLE,
	                vector_shape[0:1],
	                interp
	            ) : null;
	            Value* contract_expr = lazy_map != null && lazy_map.tag == TENSOR ? make_tensor_contract_expr(
	                lazy_map,
	                right_eval.value,
	                null,
	                null,
	                0,
	                result_shape[0:2],
	                interp
	            ) : null;
	            Value* realized = contract_expr != null && contract_expr.tag == TENSOR ? tensor_realize_to_value(contract_expr, interp) : null;
	            Value* cpu_value = realized != null && realized.tag == TENSOR ? tensor_copy_device_to_cpu_value(realized, interp) : null;
	            ok = setup_ok &&
	                lazy_map != null && lazy_map.tag == TENSOR && lazy_map.tensor_val.kind == TENSOR_PAYLOAD_MAP &&
	                contract_expr != null && contract_expr.tag == TENSOR && contract_expr.tensor_val.kind == TENSOR_PAYLOAD_CONTRACT &&
	                realized != null && realized.tag == TENSOR && realized.tensor_val != null &&
	                realized.tensor_val.kind == TENSOR_PAYLOAD_CONCRETE &&
	                realized.tensor_val.device == TENSOR_DEVICE_VULKAN &&
	                cpu_value != null && cpu_value.tag == TENSOR && cpu_value.tensor_val != null &&
	                cpu_value.tensor_val.data != null &&
	                ((double*)cpu_value.tensor_val.data)[5] == 60.0;
	        }
	        if (ok) {
	            emit_pass_literal(vulkan_available
	                ? "[PASS] tensor lazy Vulkan contract helper realizes zero-axis mapped operand"
	                : "[PASS] tensor lazy Vulkan contract helper unavailable; zero-axis mapped operand not exercised");
	            *pass += 1;
	        } else {
	            io::printn("[FAIL] tensor lazy Vulkan contract helper realizes zero-axis mapped operand");
	            *fail += 1;
	        }
	    }
	    {
	        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
	        bool ok = !vulkan_available;
	        if (vulkan_available) {
	            EvalResult left_eval = run("(to-device (Tensor Float64 [2 2 2] [1 2 3 4 5 6 7 8]) 'vulkan)", interp);
	            EvalResult right_eval = run("(to-device (Tensor Float64 [2 2 3] [1 2 3 4 5 6 7 8 9 10 11 12]) 'vulkan)", interp);
	            SymbolId plus_sym = interp.symbols.intern("+");
	            Value* plus_func = interp.global_env.lookup(plus_sym);
	            Value* zero = make_double(interp, 0.0);
	            usz[3] left_shape = { 2, 2, 2 };
	            usz[2] left_axes = { 1, 2 };
	            usz[2] right_axes = { 0, 1 };
	            usz[2] result_shape = { 2, 3 };
	            bool setup_ok = !left_eval.error.has_error && !right_eval.error.has_error &&
	                left_eval.value != null && left_eval.value.tag == TENSOR && left_eval.value.tensor_val != null &&
	                right_eval.value != null && right_eval.value.tag == TENSOR && right_eval.value.tensor_val != null &&
	                plus_func != null && zero != null && zero.tag != ERROR;
	            Value* lazy_map = setup_ok ? make_tensor_map_expr(
	                plus_func,
	                left_eval.value,
	                left_eval.value.tensor_val,
	                zero,
	                null,
	                true,
	                TENSOR_DTYPE_DOUBLE,
	                left_shape[0:3],
	                interp
	            ) : null;
	            Value* contract_expr = lazy_map != null && lazy_map.tag == TENSOR ? make_tensor_contract_expr(
	                lazy_map,
	                right_eval.value,
	                &left_axes[0],
	                &right_axes[0],
	                2,
	                result_shape[0:2],
	                interp
	            ) : null;
	            Value* realized = contract_expr != null && contract_expr.tag == TENSOR ? tensor_realize_to_value(contract_expr, interp) : null;
	            Value* cpu_value = realized != null && realized.tag == TENSOR ? tensor_copy_device_to_cpu_value(realized, interp) : null;
	            ok = setup_ok &&
	                lazy_map != null && lazy_map.tag == TENSOR && lazy_map.tensor_val.kind == TENSOR_PAYLOAD_MAP &&
	                contract_expr != null && contract_expr.tag == TENSOR && contract_expr.tensor_val.kind == TENSOR_PAYLOAD_CONTRACT &&
	                realized != null && realized.tag == TENSOR && realized.tensor_val != null &&
	                realized.tensor_val.kind == TENSOR_PAYLOAD_CONCRETE &&
	                realized.tensor_val.device == TENSOR_DEVICE_VULKAN &&
	                cpu_value != null && cpu_value.tag == TENSOR && cpu_value.tensor_val != null &&
	                cpu_value.tensor_val.data != null &&
	                ((double*)cpu_value.tensor_val.data)[5] == 210.0;
	        }
	        if (ok) {
	            emit_pass_literal(vulkan_available
	                ? "[PASS] tensor lazy Vulkan contract helper realizes multi-axis mapped operand"
	                : "[PASS] tensor lazy Vulkan contract helper unavailable; multi-axis mapped operand not exercised");
	            *pass += 1;
	        } else {
	            io::printn("[FAIL] tensor lazy Vulkan contract helper realizes multi-axis mapped operand");
	            *fail += 1;
	        }
	    }
	    {
	        bool vulkan_available = omni_tensor_backend_vulkan_available() != 0 && omni_tensor_backend_vulkan_float64_available() != 0;
	        bool ok = !vulkan_available;
        if (vulkan_available) {
            EvalResult left_eval = run("(to-device (Tensor Float64 [3] [1 2 3]) 'vulkan)", interp);
            EvalResult right_eval = run("(Tensor Float64 [3] [10 20 30])", interp);
            SymbolId plus_sym = interp.symbols.intern("+");
            Value* plus_func = interp.global_env.lookup(plus_sym);
            Value* zero = make_double(interp, 0.0);
            usz[1] vector_shape = { 3 };
            usz[1] left_axes = { 0 };
            usz[1] right_axes = { 0 };
            usz[1] scalar_shape = { 0 };
            bool setup_ok = !left_eval.error.has_error && !right_eval.error.has_error &&
                left_eval.value != null && left_eval.value.tag == TENSOR && left_eval.value.tensor_val != null &&
                right_eval.value != null && right_eval.value.tag == TENSOR && right_eval.value.tensor_val != null &&
                plus_func != null && zero != null && zero.tag != ERROR;
            Value* lazy_map = setup_ok ? make_tensor_map_expr(
                plus_func,
                left_eval.value,
                left_eval.value.tensor_val,
                zero,
                null,
                true,
                TENSOR_DTYPE_DOUBLE,
                vector_shape[0:1],
                interp
            ) : null;
            Value* contract_expr = lazy_map != null && lazy_map.tag == TENSOR ? make_tensor_contract_expr(
                lazy_map,
                right_eval.value,
                &left_axes[0],
                &right_axes[0],
                1,
                scalar_shape[0:0],
                interp
            ) : null;
            Value* realized = contract_expr != null && contract_expr.tag == TENSOR ? tensor_realize_to_value(contract_expr, interp) : null;
            ok = setup_ok &&
                lazy_map != null && lazy_map.tag == TENSOR && lazy_map.tensor_val.kind == TENSOR_PAYLOAD_MAP &&
                contract_expr != null && contract_expr.tag == TENSOR && contract_expr.tensor_val.kind == TENSOR_PAYLOAD_CONTRACT &&
                realized != null && realized.tag == ERROR &&
                str_contains(realized.str_chars[:realized.str_len], "contract: Vulkan operands must both be Vulkan-placed");
        }
        if (ok) {
            emit_pass_literal(vulkan_available
                ? "[PASS] tensor lazy Vulkan contract helper rejects mixed CPU operand"
                : "[PASS] tensor lazy Vulkan contract helper unavailable; mixed CPU operand not exercised");
            *pass += 1;
        } else {
            io::printn("[FAIL] tensor lazy Vulkan contract helper rejects mixed CPU operand");
            *fail += 1;
        }
    }
    test_truthy(interp, "contract vulkan rank-2 axis [1 0] roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [3 2] [7 8 9 10 11 12]) 'vulkan) [1 0]) 'cpu) [1 1]) 154.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-2 axis [0 0] roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [2 4] [1 2 3 4 5 6 7 8]) 'vulkan) [0 0]) 'cpu) [2 3]) 60.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-2 axis [1 1] roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [4 3] [1 2 3 4 5 6 7 8 9 10 11 12]) 'vulkan) [1 1]) 'cpu) [1 3]) 167.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-2 axis [0 1] roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [4 2] [1 2 3 4 5 6 7 8]) 'vulkan) [0 1]) 'cpu) [2 3]) 69.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-2 rank-1 axis [1 0] roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [3] [7 8 9]) 'vulkan) [1 0]) 'cpu) [1]) 122.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-2 rank-1 axis [0 0] roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [2] [7 8]) 'vulkan) [0 0]) 'cpu) [2]) 69.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-1 rank-2 axis [0 0] roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (to-device (Tensor Float64 [3 2] [4 5 6 7 8 9]) 'vulkan) [0 0]) 'cpu) [1]) 46.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-1 rank-2 axis [0 1] roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2] [2 3]) 'vulkan) (to-device (Tensor Float64 [3 2] [4 5 6 7 8 9]) 'vulkan) [0 1]) 'cpu) [2]) 43.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-2 returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [3 2] [7 8 9 10 11 12]) 'vulkan) [1 0])) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-3 rank-2 single-axis roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3 4] [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24]) 'vulkan) (to-device (Tensor Float64 [4 2] [1 2 3 4 5 6 7 8]) 'vulkan) [2 0]) 'cpu) [1 2 1]) 460.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-2 rank-3 single-axis roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) (to-device (Tensor Float64 [2 3 2] [1 2 3 4 5 6 7 8 9 10 11 12]) 'vulkan) [1 1]) 'cpu) [1 1 0]) 139.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-3 rank-3 single-axis roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 3 4] [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24]) 'vulkan) (to-device (Tensor Float64 [5 4 2] [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40]) 'vulkan) [2 1]) 'cpu) [1 2 4 1]) 3340.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-3 returns vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (contract (to-device (Tensor Float64 [2 3 4] [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24]) 'vulkan) (to-device (Tensor Float64 [4 2] [1 2 3 4 5 6 7 8]) 'vulkan) [2 0])) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero-axis rank-1 outer product roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2] [1 2]) 'vulkan) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [] []) 'cpu) [1 2]) 60.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero-axis rank-2 rank-1 outer product roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'vulkan) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [] []) 'cpu) [1 1 2]) 120.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero-axis rank-0 scalar roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [] 3.0) 'vulkan) (to-device (Tensor Float64 [] 4.0) 'vulkan) [] []) 'cpu) []) 12.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero-axis result remains vulkan tensor when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (contract (to-device (Tensor Float64 [2] [1 2]) 'vulkan) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [] [])) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero-axis zero-free output preserves zero length",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (length (to-device (contract (to-device (Tensor Float64 [0] 0.0) 'vulkan) (to-device (Tensor Float64 [3] [10 20 30]) 'vulkan) [] []) 'cpu)) 0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero-size rank-1 dot returns additive identity",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [0] 0.0) 'vulkan) (to-device (Tensor Float64 [0] 0.0) 'vulkan) [0 0]) 'cpu) []) 0.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero contracted rank-2 fills additive identity",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 0] 0.0) 'vulkan) (to-device (Tensor Float64 [0 3] 0.0) 'vulkan) [1 0]) 'cpu) [1 2]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero-free rank-2 preserves zero-length output",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (length (to-device (contract (to-device (Tensor Float64 [0 2] 0.0) 'vulkan) (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) [1 0]) 'cpu)) 0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero-free result remains vulkan tensor",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (device (contract (to-device (Tensor Float64 [0 2] 0.0) 'vulkan) (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'vulkan) [1 0])) 'vulkan) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero contracted rank-2 rank-1 fills additive identity",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 0] 0.0) 'vulkan) (to-device (Tensor Float64 [0] 0.0) 'vulkan) [1 0]) 'cpu) [1]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero contracted rank-1 rank-2 fills additive identity",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [0] 0.0) 'vulkan) (to-device (Tensor Float64 [0 3] 0.0) 'vulkan) [0 0]) 'cpu) [2]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan zero contracted rank-3 fills additive identity",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 0 3] 0.0) 'vulkan) (to-device (Tensor Float64 [0 4] 0.0) 'vulkan) [1 0]) 'cpu) [1 2 3]) 0.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rejects mixed devices",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (handle (contract (to-device (Tensor Float64 [3] [1 2 3]) 'vulkan) (Tensor Float64 [3] [10 20 30]) [0 0]) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true) true))", pass, fail);
    test_truthy(interp, "to-device vulkan zero-size follows backend availability",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (= (device (to-device (Tensor Float64 [0] []) 'vulkan)) 'vulkan) (= (handle (to-device (Tensor Float64 [0] []) 'vulkan) (raise msg (ref msg 'code))) 'tensor/backend-unavailable)))", pass, fail);
    test_truthy(interp, "to-device vulkan rejects unsupported dtype when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (= (handle (to-device (Tensor BigInteger [1] [(BigInteger \"1\")]) 'vulkan) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_eq_double(interp, "to-device cuda roundtrip when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (ref (to-device (to-device (Tensor Float64 [2] [1 2]) 'cuda) 'cpu) [1]) 2.0))", 2.0, pass, fail);
    test_truthy(interp, "to-device cuda Float32 roundtrip when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (= (ref (to-device (to-device (Tensor Float32 [2] [1.25 2.5]) 'cuda) 'cpu) [1]) (Float32 2.5)) true))", pass, fail);
    test_str_val(interp, "to-device cuda Float32 copyback preserves dtype when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (String (dtype (to-device (to-device (Tensor Float32 [2] [1.25 2.5]) 'cuda) 'cpu))) \"Float32\"))", "Float32", pass, fail);
    test_truthy(interp, "map cuda add scalar roundtrips when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (and (= (device (map + (to-device (Tensor Float64 [3] [1 2 3]) 'cuda) 5.0)) 'cuda) (= (ref (to-device (map + (to-device (Tensor Float64 [3] [1 2 3]) 'cuda) 5.0) 'cpu) [2]) 8.0)) true))", pass, fail);
    test_truthy(interp, "map cuda Float32 add scalar preserves dtype when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float32) (let (out (map + (to-device (Tensor Float32 [2] [1.25 2.5]) 'cuda) 0.5)) (and (= (device out) 'cuda) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [1]) (Float32 3.0)))) true))", pass, fail);
    test_truthy(interp, "map cuda binary arithmetic ops roundtrip when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (let (a (to-device (Tensor Float64 [3] [6 8 10]) 'cuda) b (to-device (Tensor Float64 [3] [1 2 5]) 'cuda)) (and (= (ref (to-device (map - a b) 'cpu) [1]) 6.0) (= (ref (to-device (map * a b) 'cpu) [2]) 50.0) (= (ref (to-device (map / a b) 'cpu) [0]) 6.0) (= (ref (to-device (map min a b) 'cpu) [0]) 1.0) (= (ref (to-device (map max a b) 'cpu) [2]) 10.0) (= (device (map + a b)) 'cuda))) true))", pass, fail);
    {
        char[] name = "map cuda unsupported callable fails closed";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'elementwise-map-float64)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map floor (to-device (Tensor Float64 [1] [1.2]) 'cuda))",
                "map: CUDA currently supports Float64 and Float32 arithmetic and scientific unary kernels", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    {
        char[] name = "map cuda mixed cpu operand fails closed";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'elementwise-map-float64)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map + (to-device (Tensor Float64 [1] [1.0]) 'cuda) (Tensor Float64 [1] [1.0]))",
                "map: CUDA operands must remain CUDA-placed", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    test_truthy(interp, "map cuda tensor broadcast roundtrips when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float64) (let (out (map + (to-device (Tensor Float64 [2 1] [1 2]) 'cuda) (to-device (Tensor Float64 [1 3] [10 20 30]) 'cuda))) (and (= (device out) 'cuda) (= (ref (to-device out 'cpu) [1 2]) 32.0))) true))", pass, fail);
    test_truthy(interp, "map cuda Float32 tensor broadcast preserves dtype when available",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'elementwise-map-float32) (let (out (map + (to-device (Tensor Float32 [2 1] [1.25 2.5]) 'cuda) (to-device (Tensor Float32 [1 2] [10.0 20.0]) 'cuda))) (and (= (device out) 'cuda) (= (String (dtype out)) \"Float32\") (= (ref (to-device out 'cpu) [1 1]) (Float32 22.5)))) true))", pass, fail);
    {
        char[] name = "map cuda incompatible tensor broadcast fails closed";
        EvalResult guard = run("(ref (ref (tensor-backends) 1) 'elementwise-map-float64)", interp);
        bool enabled = !guard.error.has_error && !is_nil(guard.value) && !(is_symbol(guard.value) && guard.value.sym_val == interp.sym_false);
        if (enabled) {
            test_error_contains(interp, name,
                "(map + (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'cuda) (to-device (Tensor Float64 [3] [10 20 30]) 'cuda))",
                "map: tensor shape mismatch", pass, fail);
        } else {
            emit_pass(name);
            (*pass)++;
        }
    }
    test_eq_double(interp, "contract cuda cublas rank-2 when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [3 2] [7 8 9 10 11 12]) 'cuda) [1 0]) 'cpu) [1 1]) 154.0))", 154.0, pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-2 when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float32 [3 2] [7 8 9 10 11 12]) 'cuda) [1 0]) 'cpu) [1 1]) (Float32 154.0)) true))", pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 preserves device and dtype when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (let (r (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float32 [3 2] [7 8 9 10 11 12]) 'cuda) [1 0])) (and (= (device r) 'cuda) (= (String (dtype r)) \"Float32\"))) true))", pass, fail);
    test_eq_double(interp, "contract cuda cublas rank-2 axis [0 0] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [2 4] [1 2 3 4 5 6 7 8]) 'cuda) [0 0]) 'cpu) [2 3]) 60.0))", 60.0, pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-2 axis [0 0] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float32 [2 4] [1 2 3 4 5 6 7 8]) 'cuda) [0 0]) 'cpu) [2 3]) (Float32 60.0)) true))", pass, fail);
    test_eq_double(interp, "contract cuda cublas rank-2 axis [1 1] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [4 3] [1 2 3 4 5 6 7 8 9 10 11 12]) 'cuda) [1 1]) 'cpu) [1 3]) 167.0))", 167.0, pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-2 axis [1 1] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float32 [4 3] [1 2 3 4 5 6 7 8 9 10 11 12]) 'cuda) [1 1]) 'cpu) [1 3]) (Float32 167.0)) true))", pass, fail);
    test_eq_double(interp, "contract cuda cublas rank-2 axis [0 1] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [4 2] [1 2 3 4 5 6 7 8]) 'cuda) [0 1]) 'cpu) [2 3]) 69.0))", 69.0, pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-2 axis [0 1] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float32 [4 2] [1 2 3 4 5 6 7 8]) 'cuda) [0 1]) 'cpu) [2 3]) (Float32 69.0)) true))", pass, fail);
    test_eq_double(interp, "contract cuda cublas rank-2 rank-1 when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [3] [7 8 9]) 'cuda) [1 0]) 'cpu) [1]) 122.0))", 122.0, pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-2 rank-1 when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float32 [3] [7 8 9]) 'cuda) [1 0]) 'cpu) [1]) (Float32 122.0)) true))", pass, fail);
    test_eq_double(interp, "contract cuda cublas rank-2 rank-1 axis [0 0] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (ref (to-device (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [2] [7 8]) 'cuda) [0 0]) 'cpu) [2]) 69.0))", 69.0, pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-2 rank-1 axis [0 0] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float32 [2] [7 8]) 'cuda) [0 0]) 'cpu) [2]) (Float32 69.0)) true))", pass, fail);
    test_eq_double(interp, "contract cuda cublas rank-1 rank-2 when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (ref (to-device (contract (to-device (Tensor Float64 [3] [1 2 3]) 'cuda) (to-device (Tensor Float64 [3 2] [4 5 6 7 8 9]) 'cuda) [0 0]) 'cpu) [1]) 46.0))", 46.0, pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-1 rank-2 when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [3] [1 2 3]) 'cuda) (to-device (Tensor Float32 [3 2] [4 5 6 7 8 9]) 'cuda) [0 0]) 'cpu) [1]) (Float32 46.0)) true))", pass, fail);
    test_eq_double(interp, "contract cuda cublas rank-1 rank-2 axis [0 1] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (ref (to-device (contract (to-device (Tensor Float64 [2] [2 3]) 'cuda) (to-device (Tensor Float64 [3 2] [4 5 6 7 8 9]) 'cuda) [0 1]) 'cpu) [2]) 43.0))", 43.0, pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-1 rank-2 axis [0 1] when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (= (ref (to-device (contract (to-device (Tensor Float32 [2] [2 3]) 'cuda) (to-device (Tensor Float32 [3 2] [4 5 6 7 8 9]) 'cuda) [0 1]) 'cpu) [2]) (Float32 43.0)) true))", pass, fail);
    test_truthy(interp, "contract cuda cublas rank-1 dot when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (let (r (contract (to-device (Tensor Float64 [3] [1 2 3]) 'cuda) (to-device (Tensor Float64 [3] [10 20 30]) 'cuda) [0 0])) (and (= (device r) 'cuda) (= (ref (to-device r 'cpu) []) 140.0))) true))", pass, fail);
    test_truthy(interp, "contract cuda cublas rank-1 dot explicit axes when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'available) (= (ref (to-device (contract (to-device (Tensor Float64 [3] [1 2 3]) 'cuda) (to-device (Tensor Float64 [3] [10 20 30]) 'cuda) [0] [0]) 'cpu) []) 140.0) true))", pass, fail);
    test_truthy(interp, "contract cuda cublas Float32 rank-1 dot when available",
        "(let (cublas-backend (ref (tensor-backends) 2)) (if (ref cublas-backend 'float32) (let (r (contract (to-device (Tensor Float32 [3] [1 2 3]) 'cuda) (to-device (Tensor Float32 [3] [10 20 30]) 'cuda) [0 0])) (let (cpu (to-device r 'cpu)) (and (= (device r) 'cuda) (= (String (dtype r)) \"Float32\") (= (String (type-of (ref cpu []))) \"Float32\") (= (ref cpu []) (Float32 140.0))))) true))", pass, fail);
    omni_tensor_backend_cublas_disable_for_tests(1);
    test_truthy(interp, "tensor-backends cublas disabled reports unavailable",
        "(= (ref (ref (tensor-backends) 2) 'available) false)", pass, fail);
    test_truthy(interp, "contract cuda cublas disabled fails unavailable",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (= (handle (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [3 2] [7 8 9 10 11 12]) 'cuda) [1 0]) (raise msg (ref msg 'code))) 'tensor/backend-unavailable) true))", pass, fail);
    test_truthy(interp, "contract cuda rank-1 dot cublas disabled fails unavailable",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (= (handle (contract (to-device (Tensor Float64 [3] [1 2 3]) 'cuda) (to-device (Tensor Float64 [3] [10 20 30]) 'cuda) [0 0]) (raise msg (ref msg 'code))) 'tensor/backend-unavailable) true))", pass, fail);
    test_truthy(interp, "contract cuda zero contracted dimension succeeds with cublas disabled",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (r (contract (to-device (Tensor Float64 [2 0] []) 'cuda) (to-device (Tensor Float64 [0 3] []) 'cuda) [1 0])) (and (= (device r) 'cuda) (= (ref (to-device r 'cpu) [1 2]) 0.0))) true))", pass, fail);
    omni_tensor_backend_cublas_disable_for_tests(0);
    test_truthy(interp, "contract cuda rejects mixed devices",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (= (handle (contract (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) (Tensor Float64 [3 2] [7 8 9 10 11 12]) [1 0]) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "contract cuda rejects mixed Float32 Float64 dtypes",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (= (handle (contract (to-device (Tensor Float32 [2 3] [1 2 3 4 5 6]) 'cuda) (to-device (Tensor Float64 [3 2] [7 8 9 10 11 12]) 'cuda) [1 0]) (raise msg (ref msg 'code))) 'tensor/dtype-mismatch) true))", pass, fail);
    test_truthy(interp, "contract cuda rejects unsupported multi-axis contraction",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (= (handle (contract (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'cuda) (to-device (Tensor Float64 [2 2] [5 6 7 8]) 'cuda) [[0 0] [1 1]]) (raise msg (ref msg 'code))) 'tensor/backend-unsupported) true))", pass, fail);
    test_truthy(interp, "contract cuda zero contracted rank-2 fills Float64 additive identity",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (r (contract (to-device (Tensor Float64 [2 0] []) 'cuda) (to-device (Tensor Float64 [0 3] []) 'cuda) [1 0])) (and (= (device r) 'cuda) (= (ref (shape r) 0) 2) (= (ref (shape r) 1) 3) (= (ref (to-device r 'cpu) [1 2]) 0.0))) true))", pass, fail);
    test_truthy(interp, "contract cuda zero contracted rank-2 fills Float32 additive identity",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (r (contract (to-device (Tensor Float32 [2 0] []) 'cuda) (to-device (Tensor Float32 [0 3] []) 'cuda) [1 0])) (let (cpu (to-device r 'cpu)) (and (= (device r) 'cuda) (= (String (dtype r)) \"Float32\") (= (String (type-of (ref cpu [1 2]))) \"Float32\") (= (ref cpu [1 2]) (Float32 0.0))))) true))", pass, fail);
    test_truthy(interp, "contract cuda zero-free rank-2 preserves empty Float64 output",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (r (contract (to-device (Tensor Float64 [0 2] []) 'cuda) (to-device (Tensor Float64 [2 3] [1 2 3 4 5 6]) 'cuda) [1 0])) (and (= (device r) 'cuda) (= (ref (shape r) 0) 0) (= (ref (shape r) 1) 3) (= (length (to-device r 'cpu)) 0))) true))", pass, fail);
    test_truthy(interp, "contract cuda zero contracted rank-2 rank-1 fills additive identity",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (r (contract (to-device (Tensor Float64 [2 0] []) 'cuda) (to-device (Tensor Float64 [0] []) 'cuda) [1 0])) (and (= (device r) 'cuda) (= (ref (shape r) 0) 2) (= (ref (to-device r 'cpu) [1]) 0.0))) true))", pass, fail);
    test_truthy(interp, "contract cuda zero contracted rank-1 rank-2 fills additive identity",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (r (contract (to-device (Tensor Float64 [0] []) 'cuda) (to-device (Tensor Float64 [0 2] []) 'cuda) [0 0])) (and (= (device r) 'cuda) (= (ref (shape r) 0) 2) (= (ref (to-device r 'cpu) [1]) 0.0))) true))", pass, fail);
    test_truthy(interp, "contract cuda zero-size rank-1 dot returns additive identity",
        "(let (cuda-backend (ref (tensor-backends) 1)) (if (ref cuda-backend 'available) (let (r (contract (to-device (Tensor Float64 [0] []) 'cuda) (to-device (Tensor Float64 [0] []) 'cuda) [0 0])) (and (= (device r) 'cuda) (= (ref (to-device r 'cpu) []) 0.0))) true))", pass, fail);
    test_truthy(interp, "contract vulkan multi-axis scalar roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 2] [1 2 3 4]) 'vulkan) (to-device (Tensor Float64 [2 2] [5 6 7 8]) 'vulkan) [[0 0] [1 1]]) 'cpu) []) 70.0) true) true))", pass, fail);
    test_truthy(interp, "contract vulkan rank-3 multi-axis roundtrips when available",
        "(let (vulkan-backend (ref (tensor-backends) 3)) (if (ref vulkan-backend 'available) (if (ref vulkan-backend 'float64) (= (ref (to-device (contract (to-device (Tensor Float64 [2 2 2] [1 2 3 4 5 6 7 8]) 'vulkan) (to-device (Tensor Float64 [2 2 3] [1 2 3 4 5 6 7 8 9 10 11 12]) 'vulkan) [[1 0] [2 1]]) 'cpu) [1 2]) 210.0) true) true))", pass, fail);
}
