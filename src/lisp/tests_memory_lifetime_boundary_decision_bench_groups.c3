module lisp;

import std::io;
import std::core::mem;
import main;

const usz BOUNDARY_BENCH_DECISION_ITERS = 2048;
const usz BOUNDARY_BENCH_SPLICE_ITERS = 2048;
const usz BOUNDARY_BENCH_SPLICE_ESCAPES_PER_ITER = 64;
const usz BOUNDARY_BENCH_SHAPE_ITERS = 128;
const usz BOUNDARY_BENCH_SHAPE_ITEMS = 24;
const usz BOUNDARY_BENCH_PRODUCT_ITERS = 64;
const usz BOUNDARY_BENCH_PRODUCT_QUOTES = 12;
const usz BOUNDARY_BENCH_PRODUCT_HOLDINGS = 8;
const usz BOUNDARY_BENCH_CLOSURE_ITER_ITERS = 64;
const usz BOUNDARY_BENCH_CLOSURE_ITER_EXPECTED_COUNT = 10;
const long BOUNDARY_BENCH_CLOSURE_ITER_EXPECTED_SUM = 355;
const usz BOUNDARY_BENCH_TENSOR_ITERS = 64;
const usz BOUNDARY_BENCH_TENSORS_PER_ITER = 4;
const usz BOUNDARY_BENCH_TENSOR_ROWS = 2;
const usz BOUNDARY_BENCH_TENSOR_COLS = 3;
const usz BOUNDARY_BENCH_TENSOR_ELEMENTS = BOUNDARY_BENCH_TENSOR_ROWS * BOUNDARY_BENCH_TENSOR_COLS;
const usz BOUNDARY_BENCH_NESTED_MODULE_ITERS = 64;
const long BOUNDARY_BENCH_NESTED_MODULE_INPUT = 5;
const long BOUNDARY_BENCH_NESTED_MODULE_SEED = 13;
const usz EQUALITY_BENCH_ITERS = 2048;
const usz EQUALITY_BENCH_NESTED_LIST_DEPTH = 1024;
const usz EQUALITY_BENCH_ARRAY_LENGTH = 1024;

struct BoundaryBenchCounterSnapshot {
    main::ScopeMemoryTelemetryStats scope;
    main::FiberTempPoolStats fiber;
    BoundaryEscapeShapeStats escape_shape;
    BoundaryValueShapeStats value_shape;
    BoundaryDecisionStats decisions;
}

fn bool boundary_bench_enabled() @inline {
    return boundary_c_getenv("OMNI_BOUNDARY_BENCH") != null;
}

fn long boundary_bench_now_ms() @inline {
    long now_ms = scheduler_monotonic_ms();
    if (now_ms > 0) return now_ms;
    return 0;
}

fn BoundaryBenchCounterSnapshot boundary_bench_counter_snapshot() @inline {
    BoundaryBenchCounterSnapshot snapshot = {};
    snapshot.scope = main::g_scope_memory_telemetry_stats;
    snapshot.fiber = main::g_fiber_temp_pool_stats;
    snapshot.escape_shape = boundary_escape_shape_stats_snapshot();
    snapshot.value_shape = boundary_value_shape_stats_snapshot();
    snapshot.decisions = boundary_decision_stats_snapshot();
    return snapshot;
}

fn usz boundary_bench_delta(usz after, usz before) @inline {
    return after >= before ? after - before : 0;
}

fn char[] boundary_bench_finwatch_symbol_name(usz index) @inline {
    switch (index % 8) {
        case 0: return "BTC";
        case 1: return "ETH";
        case 2: return "AAPL";
        case 3: return "MSFT";
        case 4: return "NVDA";
        case 5: return "EURUSD";
        case 6: return "SPY";
        default: return "TLT";
    }
}

fn bool boundary_bench_dict_set(Interp* interp, Value* dict, char[] key, Value* value) @inline {
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) return false;
    if (value == null || value.tag == ERROR) return false;
    SymbolId key_sym = interp.symbols.intern(key);
    if (key_sym == INVALID_SYMBOL_ID) return false;
    return hashmap_set_symbol_checked(dict.hashmap_val, key_sym, value, interp);
}

fn Value* boundary_bench_make_finwatch_quote(Interp* interp, usz iter, usz index) {
    Value* quote = make_hashmap_for_entry_count_checked(interp, 5, "finwatch bench: quote out of memory");
    if (quote == null || quote.tag == ERROR) return quote;
    char[] symbol_name = boundary_bench_finwatch_symbol_name(index);
    if (!boundary_bench_dict_set(interp, quote, "symbol", make_string(interp, symbol_name))) return null;
    if (!boundary_bench_dict_set(interp, quote, "price", make_double(interp, 100.0 + (double)(iter + index)))) return null;
    if (!boundary_bench_dict_set(interp, quote, "change", make_double(interp, (double)((long)(index % 5) - 2) / 10.0))) return null;
    if (!boundary_bench_dict_set(interp, quote, "source", make_string(interp, "finwatch-bench-feed"))) return null;
    if (!boundary_bench_dict_set(interp, quote, "ts", make_int(interp, (long)(1700000000 + iter)))) return null;
    return quote;
}

fn Value* boundary_bench_make_finwatch_holding(Interp* interp, usz iter, usz index) {
    Value* holding = make_hashmap_for_entry_count_checked(interp, 4, "finwatch bench: holding out of memory");
    if (holding == null || holding.tag == ERROR) return holding;
    char[] symbol_name = boundary_bench_finwatch_symbol_name(index);
    if (!boundary_bench_dict_set(interp, holding, "symbol", make_string(interp, symbol_name))) return null;
    if (!boundary_bench_dict_set(interp, holding, "qty", make_double(interp, 1.0 + (double)(index % 3)))) return null;
    if (!boundary_bench_dict_set(interp, holding, "cost", make_double(interp, 80.0 + (double)(iter + index)))) return null;
    if (!boundary_bench_dict_set(interp, holding, "bucket", make_symbol(interp, interp.symbols.intern(index % 2 == 0 ? "core" : "watch")))) return null;
    return holding;
}

fn Value* boundary_bench_make_finwatch_array(Interp* interp, usz len, bool quotes, usz iter) {
    Value* arr = make_array(interp, len);
    if (arr == null || arr.array_val == null) return null;
    for (usz i = 0; i < len; i++) {
        Value* item = quotes
            ? boundary_bench_make_finwatch_quote(interp, iter, i)
            : boundary_bench_make_finwatch_holding(interp, iter, i);
        if (item == null || item.tag == ERROR) return null;
        arr.array_val.items[i] = item;
        arr.array_val.length++;
    }
    return arr;
}

fn Value* boundary_bench_make_finwatch_watchlist(Interp* interp) {
    Value* watchlist = make_set_for_entry_count_checked(
        interp,
        BOUNDARY_BENCH_PRODUCT_QUOTES,
        "finwatch bench: watchlist out of memory");
    if (watchlist == null || watchlist.tag != SET) return null;
    for (usz i = 0; i < BOUNDARY_BENCH_PRODUCT_QUOTES; i++) {
        Value*[2] set_args = { watchlist, make_string(interp, boundary_bench_finwatch_symbol_name(i)) };
        Value* added = prim_set_add(set_args[0:2], null, interp);
        if (added == null || added.tag == ERROR) return null;
    }
    return watchlist;
}

fn Value* boundary_bench_make_finwatch_response(Interp* interp, usz iter) {
    Value* response = make_hashmap_for_entry_count_checked(interp, 7, "finwatch bench: response out of memory");
    if (response == null || response.tag == ERROR) return response;
    Value* quotes = boundary_bench_make_finwatch_array(interp, BOUNDARY_BENCH_PRODUCT_QUOTES, true, iter);
    Value* holdings = boundary_bench_make_finwatch_array(interp, BOUNDARY_BENCH_PRODUCT_HOLDINGS, false, iter);
    Value* watchlist = boundary_bench_make_finwatch_watchlist(interp);
    Value* request = make_hashmap_for_entry_count_checked(interp, 3, "finwatch bench: request out of memory");
    if (quotes == null || holdings == null || watchlist == null || request == null || request.tag == ERROR) return null;
    if (!boundary_bench_dict_set(interp, request, "method", make_string(interp, "POST"))) return null;
    if (!boundary_bench_dict_set(interp, request, "path", make_string(interp, "/portfolio"))) return null;
    if (!boundary_bench_dict_set(interp, request, "route", make_symbol(interp, interp.symbols.intern("portfolio")))) return null;

    void* feed_payload = mem::malloc(64);
    Value* feed_handle = make_ffi_handle_ex(interp, feed_payload, "finwatch-feed-cache", null, true, false);
    if (feed_handle == null) return null;

    if (!boundary_bench_dict_set(interp, response, "status", make_string(interp, "ok"))) return null;
    if (!boundary_bench_dict_set(interp, response, "quotes", quotes)) return null;
    if (!boundary_bench_dict_set(interp, response, "portfolio", holdings)) return null;
    if (!boundary_bench_dict_set(interp, response, "watchlist", watchlist)) return null;
    if (!boundary_bench_dict_set(interp, response, "request", request)) return null;
    if (!boundary_bench_dict_set(interp, response, "feed-handle", feed_handle)) return null;
    if (!boundary_bench_dict_set(interp, response, "server", make_string(interp, "examples/finwatch"))) return null;
    return response;
}

fn Value* boundary_bench_make_shape_array(Interp* interp, usz rows, usz cols) {
    Value* shape = make_array(interp, 2);
    if (shape == null || shape.tag != ARRAY || shape.array_val == null) return null;
    shape.array_val.items[0] = make_int(interp, (long)rows);
    shape.array_val.items[1] = make_int(interp, (long)cols);
    shape.array_val.length = 2;
    return shape;
}

fn Value* boundary_bench_make_tensor_metadata_tensor(Interp* interp, usz iter, usz index) {
    usz[2] shape = { BOUNDARY_BENCH_TENSOR_ROWS, BOUNDARY_BENCH_TENSOR_COLS };
    Value* tensor = make_tensor_double_uninitialized_checked(
        interp,
        shape[0:2],
        "tensor metadata bench: tensor out of memory");
    if (tensor == null || tensor.tag != TENSOR || tensor.tensor_val == null || tensor.tensor_val.data == null) return tensor;

    double* data = (double*)tensor.tensor_val.data;
    double base = (double)(iter * 100 + index * 10);
    for (usz i = 0; i < BOUNDARY_BENCH_TENSOR_ELEMENTS; i++) {
        data[i] = base + (double)i;
    }
    return tensor;
}

fn Value* boundary_bench_make_tensor_metadata_layout(Interp* interp, Value* tensor, usz iter, usz index) {
    if (tensor == null || tensor.tag != TENSOR || tensor.tensor_val == null) return null;
    TensorVal* meta = tensor.tensor_val;
    if (meta.rank != 2 || meta.shape == null) return null;
    Value* layout = make_hashmap_for_entry_count_checked(interp, 8, "tensor metadata bench: layout out of memory");
    if (layout == null || layout.tag == ERROR) return layout;
    if (!boundary_bench_dict_set(interp, layout, "kind", make_symbol(interp, interp.symbols.intern("tensor-layout")))) return null;
    if (!boundary_bench_dict_set(interp, layout, "dtype", make_symbol(interp, interp.sym_Double))) return null;
    if (!boundary_bench_dict_set(interp, layout, "rank", make_int(interp, (long)meta.rank))) return null;
    if (!boundary_bench_dict_set(interp, layout, "shape", boundary_bench_make_shape_array(interp, meta.shape[0], meta.shape[1]))) return null;
    if (!boundary_bench_dict_set(interp, layout, "elements", make_int(interp, (long)meta.element_count))) return null;
    if (!boundary_bench_dict_set(interp, layout, "iter", make_int(interp, (long)iter))) return null;
    if (!boundary_bench_dict_set(interp, layout, "index", make_int(interp, (long)index))) return null;
    if (!boundary_bench_dict_set(interp, layout, "tensor", tensor)) return null;
    return layout;
}

fn Value* boundary_bench_make_tensor_metadata_response(Interp* interp, usz iter) {
    Value* records = make_nil(interp);

    for (usz i = 0; i < BOUNDARY_BENCH_TENSORS_PER_ITER; i++) {
        Value* tensor = boundary_bench_make_tensor_metadata_tensor(interp, iter, i);
        Value* shape = boundary_bench_make_shape_array(interp, BOUNDARY_BENCH_TENSOR_ROWS, BOUNDARY_BENCH_TENSOR_COLS);
        if (tensor == null || tensor.tag == ERROR ||
            shape == null || shape.tag == ERROR) return null;

        Value* record = make_cons(
            interp,
            tensor,
            make_cons(
                interp,
                shape,
                make_cons(
                    interp,
                    make_int(interp, (long)BOUNDARY_BENCH_TENSOR_ELEMENTS),
                    make_cons(
                        interp,
                        make_int(interp, (long)iter),
                        make_cons(interp, make_int(interp, (long)i), make_nil(interp))))));
        if (record == null || record.tag != CONS) return null;
        records = make_cons(interp, record, records);
        if (records == null || records.tag != CONS) return null;
    }

    return make_cons(
        interp,
        make_int(interp, (long)(BOUNDARY_BENCH_TENSORS_PER_ITER * BOUNDARY_BENCH_TENSOR_ELEMENTS)),
        records);
}

fn bool boundary_bench_tensor_metadata_response_ok(Value* response, Interp* interp) {
    (void)interp;
    if (response == null || response.tag != CONS) return false;
    Value* total = car(response);
    Value* records = cdr(response);
    if (total == null || total.tag != INT ||
        total.int_val != (long)(BOUNDARY_BENCH_TENSORS_PER_ITER * BOUNDARY_BENCH_TENSOR_ELEMENTS)) return false;

    usz count = 0;
    while (records != null && records.tag == CONS) {
        Value* record = car(records);
        if (record == null || record.tag != CONS) return false;
        Value* tensor = car(record);
        Value* fields = cdr(record);
        if (fields == null || fields.tag != CONS) return false;
        Value* shape = car(fields);
        fields = cdr(fields);
        if (fields == null || fields.tag != CONS) return false;
        Value* elements = car(fields);
        fields = cdr(fields);
        if (fields == null || fields.tag != CONS) return false;
        Value* iter = car(fields);
        fields = cdr(fields);
        if (fields == null || fields.tag != CONS) return false;
        Value* index = car(fields);
        Value* tail = cdr(fields);
        if (tail == null || !is_nil(tail)) return false;

        if (tensor == null || tensor.tag != TENSOR || tensor.tensor_val == null ||
            shape == null || shape.tag != ARRAY || shape.array_val == null ||
            elements == null || elements.tag != INT ||
            iter == null || iter.tag != INT ||
            index == null || index.tag != INT) return false;
        TensorVal* meta = tensor.tensor_val;
        if (meta.rank != 2 || meta.shape == null ||
            meta.shape[0] != BOUNDARY_BENCH_TENSOR_ROWS ||
            meta.shape[1] != BOUNDARY_BENCH_TENSOR_COLS ||
            meta.element_count != BOUNDARY_BENCH_TENSOR_ELEMENTS ||
            meta.byte_len != BOUNDARY_BENCH_TENSOR_ELEMENTS * double.sizeof ||
            meta.data == null) return false;
        if (shape.array_val.length != 2 ||
            shape.array_val.items[0] == null ||
            shape.array_val.items[0].tag != INT ||
            shape.array_val.items[0].int_val != (long)BOUNDARY_BENCH_TENSOR_ROWS ||
            shape.array_val.items[1] == null ||
            shape.array_val.items[1].tag != INT ||
            shape.array_val.items[1].int_val != (long)BOUNDARY_BENCH_TENSOR_COLS ||
            elements.int_val != (long)BOUNDARY_BENCH_TENSOR_ELEMENTS ||
            iter.int_val < 0 ||
            index.int_val < 0 ||
            index.int_val >= (long)BOUNDARY_BENCH_TENSORS_PER_ITER) return false;
        count++;
        records = cdr(records);
    }

    return records != null && is_nil(records) && count == BOUNDARY_BENCH_TENSORS_PER_ITER;
}

fn bool boundary_bench_setup_nested_module_return_fixture(Interp* interp) {
    if (interp == null) return false;
    SymbolId outer_name = interp.symbols.intern("mem006-return-outer");
    if (outer_name == INVALID_SYMBOL_ID) return false;
    if (find_module(outer_name, interp) != null) return true;

    EvalResult r = run(
        "(module mem006-return-outer (export make-batch) "
        "  (module mem006-return-inner (export seed make-row make-module-pair) "
        "    (define seed 13) "
        "    (define make-row "
        "      (lambda (n) (list n (+ n seed) (Array n (+ n 1) (+ n 2))))) "
        "    (define make-module-pair "
        "      (lambda (n) (list (make-row n) (make-row (+ n 1)))))) "
        "  (define make-batch "
        "    (lambda (n) "
        "      (with mem006-return-inner "
        "        (list (make-row n) "
        "              (make-module-pair (+ n 1)) "
        "              (Array (make-row (+ n 3)) (make-row (+ n 4)))))))) "
        "(import mem006-return-outer)",
        interp
    );
    return !r.error.has_error;
}

fn bool boundary_bench_nested_module_read_list_item(Value** cursor, Value** item_out) {
    if (cursor == null || item_out == null || *cursor == null || (*cursor).tag != CONS) return false;
    *item_out = car(*cursor);
    *cursor = cdr(*cursor);
    return *item_out != null && *cursor != null;
}

fn bool boundary_bench_nested_module_row_ok(Value* row, long n) {
    if (row == null || row.tag != CONS) return false;
    Value* cursor = row;
    Value* first = null;
    Value* second = null;
    Value* arr = null;
    if (!boundary_bench_nested_module_read_list_item(&cursor, &first) ||
        !boundary_bench_nested_module_read_list_item(&cursor, &second) ||
        !boundary_bench_nested_module_read_list_item(&cursor, &arr) ||
        !is_nil(cursor)) return false;

    if (first == null || first.tag != INT || first.int_val != n ||
        second == null || second.tag != INT || second.int_val != n + BOUNDARY_BENCH_NESTED_MODULE_SEED ||
        arr == null || arr.tag != ARRAY || arr.array_val == null ||
        arr.array_val.length != 3) return false;

    for (usz i = 0; i < 3; i++) {
        Value* item = arr.array_val.items[i];
        if (item == null || item.tag != INT || item.int_val != n + (long)i) return false;
    }
    return true;
}

fn bool boundary_bench_nested_module_pair_ok(Value* pair, long n) {
    if (pair == null || pair.tag != CONS) return false;
    Value* cursor = pair;
    Value* first_row = null;
    Value* second_row = null;
    if (!boundary_bench_nested_module_read_list_item(&cursor, &first_row) ||
        !boundary_bench_nested_module_read_list_item(&cursor, &second_row) ||
        !is_nil(cursor)) return false;
    return boundary_bench_nested_module_row_ok(first_row, n) &&
        boundary_bench_nested_module_row_ok(second_row, n + 1);
}

fn bool boundary_bench_nested_module_batch_ok(Value* batch, long n) {
    if (batch == null || batch.tag != CONS) return false;
    Value* cursor = batch;
    Value* row = null;
    Value* pair = null;
    Value* rows = null;
    if (!boundary_bench_nested_module_read_list_item(&cursor, &row) ||
        !boundary_bench_nested_module_read_list_item(&cursor, &pair) ||
        !boundary_bench_nested_module_read_list_item(&cursor, &rows) ||
        !is_nil(cursor)) return false;

    if (row == null || pair == null ||
        rows == null || rows.tag != ARRAY || rows.array_val == null ||
        rows.array_val.length != 2) return false;

    return boundary_bench_nested_module_row_ok(row, n) &&
        boundary_bench_nested_module_pair_ok(pair, n + 1) &&
        boundary_bench_nested_module_row_ok(rows.array_val.items[0], n + 3) &&
        boundary_bench_nested_module_row_ok(rows.array_val.items[1], n + 4);
}

fn Value* boundary_bench_make_nested_int_list(Interp* interp, usz depth, long tail_delta = 0) {
    Value* list = make_nil(interp);
    for (usz i = depth; i > 0; i--) {
        long v = (long)(i - 1);
        if (tail_delta != 0 && i == depth) v += tail_delta;
        list = make_cons(interp, make_int(interp, v), list);
    }
    return list;
}

fn Value* boundary_bench_make_int_array(Interp* interp, usz len, long tail_delta = 0) {
    Value* arr = make_array(interp, len);
    if (arr == null || arr.array_val == null) return null;
    for (usz i = 0; i < len; i++) {
        long v = (long)i;
        if (tail_delta != 0 && i + 1 == len) v += tail_delta;
        arr.array_val.items[i] = make_int(interp, v);
        arr.array_val.length++;
    }
    return arr;
}

fn Value* boundary_bench_make_mixed_nested_fixture(Interp* interp, long tail_delta = 0) {
    Value* nested_list = boundary_bench_make_nested_int_list(interp, EQUALITY_BENCH_NESTED_LIST_DEPTH / 2, tail_delta);
    Value* nested_array = boundary_bench_make_int_array(interp, EQUALITY_BENCH_ARRAY_LENGTH / 2, tail_delta);
    if (nested_list == null || nested_array == null) return null;

    Value* pair = make_cons(interp, nested_list, make_cons(interp, nested_array, make_nil(interp)));
    return make_cons(interp, pair, make_cons(interp, make_nil(interp), make_nil(interp)));
}

fn void boundary_bench_accumulate_equality_workspace(
    EqualityWorkspaceStats* total,
    EqualityWorkspaceStats sample
) @inline {
    total.stack_heap_growths += sample.stack_heap_growths;
    total.seen_heap_growths += sample.seen_heap_growths;
    total.stack_heap_bytes += sample.stack_heap_bytes;
    total.seen_heap_bytes += sample.seen_heap_bytes;
}

fn bool boundary_bench_fill_and_grow_array(Interp* interp, Value* arr, usz seed) {
    if (arr == null || arr.tag != ARRAY || arr.array_val == null) return false;
    Array* vec = arr.array_val;
    for (usz i = 0; i < vec.capacity; i++) {
        vec.items[i] = make_int(interp, (long)(seed + i));
    }
    vec.length = vec.capacity;
    Value*[2] push_args = { arr, make_int(interp, (long)(seed + vec.capacity + 1)) };
    Value* pushed = prim_array_push(push_args[0:2], null, interp);
    return pushed != null && pushed.tag != ERROR && vec.length == vec.capacity / 2 + 1;
}

fn bool boundary_bench_fill_hashmap(Interp* interp, Value* dict, usz seed) {
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) return false;
    for (usz i = 0; i < BOUNDARY_BENCH_SHAPE_ITEMS; i++) {
        if (!hashmap_set_checked(
            dict.hashmap_val,
            make_int(interp, (long)(seed + i)),
            make_int(interp, (long)(seed + i + 1000)),
            interp
        )) return false;
    }
    return dict.hashmap_val.count == BOUNDARY_BENCH_SHAPE_ITEMS;
}

fn bool boundary_bench_fill_set(Interp* interp, Value* set, usz seed) {
    if (set == null || set.tag != SET || set.hashmap_val == null) return false;
    for (usz i = 0; i < BOUNDARY_BENCH_SHAPE_ITEMS; i++) {
        Value*[2] set_args = { set, make_int(interp, (long)(seed + i)) };
        Value* added = prim_set_add(set_args[0:2], null, interp);
        if (added == null || added.tag == ERROR) return false;
    }
    return set.hashmap_val.count == BOUNDARY_BENCH_SHAPE_ITEMS;
}

fn bool boundary_bench_exercise_payload_shapes(Interp* interp, usz seed) {
    (void)seed;
    Value* str = make_string(interp, "memory-boundary-telemetry-payload-shape-benchmark-string");
    Value* err = make_error(interp, "memory-boundary-telemetry-payload-shape-benchmark-error");
    Value* big = make_big_integer_from_string(interp, "12345678901234567890123456789012345678901234567890");
    usz[2] tensor_shape = { 8, 8 };
    Value* tensor = make_tensor_double_uninitialized(interp, tensor_shape[0:2]);
    void* ffi_payload = mem::malloc(32);
    Value* ffi = make_ffi_handle_ex(interp, ffi_payload, "bench-ffi-owned", null, true, false);
    return str != null && str.tag == STRING &&
           err != null && err.tag == ERROR &&
           big != null && big.tag == BIG_INTEGER &&
           tensor != null && tensor.tag == TENSOR &&
           ffi != null && ffi.tag == FFI_HANDLE;
}

fn bool boundary_bench_exercise_stable_passport(Interp* interp) {
    StableEscapeHandle handle = {};
    main::ScopeRegion* saved_scope = interp.current_scope;
    interp.current_scope = interp.root_scope;
    defer interp.current_scope = saved_scope;

    Value* prepared_root = make_cons(interp, make_int(interp, 7701), make_nil(interp));
    if (prepared_root == null || prepared_root.tag != CONS) return false;
    if (!stable_escape_store_prepare_value(interp, prepared_root, &handle)) return false;
    prepared_root.cons_val.car = make_int(interp, 7702);
    StableGraphPassport passport = {};
    bool snapshot_ok = stable_escape_store_passport_snapshot(interp, &handle, &passport);
    bool invalidated = snapshot_ok &&
        passport.invalidation_reason == STABLE_GRAPH_PASSPORT_PREPARED_GRAPH_STALE;
    bool retire_ok = stable_escape_store_retire(interp, &handle);
    return invalidated && retire_ok;
}

fn bool boundary_bench_sum_int_list(Value* list, usz* count_out, long* sum_out) {
    if (count_out == null || sum_out == null) return false;
    *count_out = 0;
    *sum_out = 0;

    Value* cur = list;
    while (cur != null && cur.tag == CONS) {
        Value* item = car(cur);
        if (item == null || item.tag != INT) return false;
        *sum_out += item.int_val;
        *count_out += 1;
        cur = cdr(cur);
    }

    return cur != null && is_nil(cur);
}

fn bool boundary_bench_closure_iterator_pipeline_once(Interp* interp, usz* count_out, long* sum_out) {
    EvalResult r = run(
        "(let (scale 3 bias 1 threshold 20 cap 10) "
        "  (take cap "
        "    (filter (lambda (x) (> x threshold)) "
        "      (map (lambda (x) (+ (* x scale) bias)) "
        "        (range-from 0)))))",
        interp
    );
    if (r.error.has_error || r.value == null || r.value.tag != ITERATOR) return false;

    Value*[1] list_args = { r.value };
    Value* realized = prim_list(list_args[0:1], null, interp);
    if (realized == null || realized.tag == ERROR) return false;
    return boundary_bench_sum_int_list(realized, count_out, sum_out);
}

fn void run_memory_lifetime_boundary_benchmarks(Interp* interp) {
    if (!boundary_bench_enabled()) return;

    main::ScopeRegion* root_scope = interp.current_scope;

    usz decision_splice_ok = 0;
    usz decision_disallowed_ok = 0;
    usz decision_reuse_ok = 0;
    usz decision_destination_reuse_ok = 0;
    usz splice_tail_ok = 0;

    long decision_splice_ms = 0;
    long decision_disallowed_ms = 0;
    long decision_reuse_ms = 0;
    long decision_destination_reuse_ms = 0;
    long splice_tail_ms = 0;
    long equality_list_true_ms = 0;
    long equality_list_false_ms = 0;
    long equality_array_true_ms = 0;
    long equality_array_false_ms = 0;
    long equality_mixed_true_ms = 0;
    long equality_mixed_false_ms = 0;
    long shape_ms = 0;
    long product_ms = 0;
    long closure_iter_ms = 0;
    long tensor_metadata_ms = 0;
    long nested_module_ms = 0;

    usz equality_list_true_ok = 0;
    usz equality_list_false_ok = 0;
    usz equality_array_true_ok = 0;
    usz equality_array_false_ok = 0;
    usz equality_mixed_true_ok = 0;
    usz equality_mixed_false_ok = 0;
    bool equality_fixture_ok = false;
    EqualityWorkspaceStats equality_workspace = {};
    usz shape_ok = 0;
    usz stable_passport_ok = 0;
    usz closure_env_ok = 0;
    usz product_ok = 0;
    usz product_commit_ok = 0;
    usz closure_iter_ok = 0;
    usz closure_iter_count_total = 0;
    long closure_iter_sum_total = 0;
    usz tensor_metadata_ok = 0;
    usz tensor_metadata_commit_ok = 0;
    usz nested_module_setup_ok = 0;
    usz nested_module_batch_ok = 0;
    BoundaryBenchCounterSnapshot shape_before = {};
    BoundaryBenchCounterSnapshot shape_after = {};
    BoundaryBenchCounterSnapshot product_before = {};
    BoundaryBenchCounterSnapshot product_after = {};
    BoundaryBenchCounterSnapshot closure_iter_before = {};
    BoundaryBenchCounterSnapshot closure_iter_after = {};
    BoundaryBenchCounterSnapshot tensor_metadata_before = {};
    BoundaryBenchCounterSnapshot tensor_metadata_after = {};
    BoundaryBenchCounterSnapshot nested_module_before = {};
    BoundaryBenchCounterSnapshot nested_module_after = {};

    // Decision benchmark: splice-commit path.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_DECISION_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = releasing_scope;
            Value* src = make_cons(interp, make_int(interp, (long)(i + 1)), make_nil(interp));

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                src,
                interp,
                target_scope,
                releasing_scope,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);

            bool ok = (committed.releasing_scope_spliced ||
                       committed.outcome == BOUNDARY_COMMIT_DESTINATION_CONS_BUILT) &&
                      committed.value != null;
            if (ok) decision_splice_ok++;

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }
        }
        long end_ms = boundary_bench_now_ms();
        decision_splice_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    // Decision benchmark: budget-abort hard-disallowed path.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_DECISION_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = releasing_scope;
            Value* src = boundary_commit_make_temp_int_list(interp, 8);

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, 0);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                src,
                interp,
                target_scope,
                releasing_scope,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);

            bool ok = !committed.releasing_scope_spliced &&
                      committed.outcome == BOUNDARY_COMMIT_FALLBACK_DISALLOWED &&
                      committed.fault_code == BOUNDARY_COMMIT_FAULT_PROMOTION_ABORTED &&
                      committed.value != null &&
                      committed.value.tag == ERROR;
            if (ok) decision_disallowed_ok++;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);
        }
        long end_ms = boundary_bench_now_ms();
        decision_disallowed_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    // Decision benchmark: target-chain reuse path.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_DECISION_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = target_scope;
            Value* safe = make_cons(interp, make_int(interp, (long)(i + 17)), make_nil(interp));
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                safe,
                interp,
                target_scope,
                releasing_scope
            );

            bool ok = !committed.releasing_scope_spliced &&
                      committed.reused_in_target_chain &&
                      committed.value == safe;
            if (ok) decision_reuse_ok++;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);
        }
        long end_ms = boundary_bench_now_ms();
        decision_reuse_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    // Decision benchmark: destination partial build with target-escape routed args.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_DECISION_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = target_scope;
            Value* escaped_first = promote_to_escape(make_int(interp, (long)(i + 501)), interp);
            Value* escaped_second = promote_to_escape(make_int(interp, (long)(i + 901)), interp);

            interp.current_scope = releasing_scope;
            Value* src = interp.alloc_value();
            src.tag = PARTIAL_PRIM;
            src.partial_val.func = null;
            src.partial_val.remaining = 1;
            src.partial_val.first_arg = escaped_first;
            src.partial_val.second_arg = escaped_second;

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            Value* destination_partial = boundary_build_destination_partial_escape(
                src,
                interp,
                target_scope,
                releasing_scope,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);

            bool ok = destination_partial != null &&
                      destination_partial.tag == PARTIAL_PRIM &&
                      destination_partial.scope_gen == target_scope.escape_generation &&
                      destination_partial.partial_val.first_arg.scope_gen == target_scope.escape_generation &&
                      destination_partial.partial_val.second_arg.scope_gen == target_scope.escape_generation;
            if (ok) decision_destination_reuse_ok++;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);
        }
        long end_ms = boundary_bench_now_ms();
        decision_destination_reuse_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    // Splice benchmark: focus on escape-lane concat throughput/tail consistency.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_SPLICE_ITERS; i++) {
            main::ScopeRegion* parent = main::scope_create(root_scope);
            main::ScopeRegion* child = parent == null ? null : main::scope_create(parent);
            if (parent == null || child == null) {
                if (child != null) main::scope_release(child);
                if (parent != null) main::scope_release(parent);
                continue;
            }

            interp.current_scope = child;
            Value* first_escape = null;
            for (usz j = 0; j < BOUNDARY_BENCH_SPLICE_ESCAPES_PER_ITER; j++) {
                Value* v = interp.alloc_value_escape();
                if (v != null) {
                    v.tag = INT;
                    v.int_val = (long)(j + 1);
                    if (j == 0) first_escape = v;
                }
            }
            interp.current_scope = root_scope;

            BoundaryScopeTransferResult ready = boundary_try_scope_splice_escapes_result(parent, child);
            bool ok = false;
            if (ready.allowed) {
                ok = first_escape != null &&
                     main::is_in_scope((void*)first_escape, parent) &&
                     main::scope_escape_chunk_tail_consistent(parent) &&
                     main::scope_escape_dtor_tail_consistent(parent);
            }
            if (ok) splice_tail_ok++;
            main::scope_release(parent);
        }
        long end_ms = boundary_bench_now_ms();
        splice_tail_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    // Counter-shape benchmark: exercise the allocator/value-shape fields added
    // for the telemetry benchmark lane.
    {
        shape_before = boundary_bench_counter_snapshot();
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_SHAPE_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            main::ScopeRegion* reset_scope = main::scope_create(root_scope);
            if (target_scope == null || releasing_scope == null || reset_scope == null) {
                if (reset_scope != null) main::scope_release(reset_scope);
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = reset_scope;
            void* reset_temp = reset_scope.alloc(96);
            void* reset_escape = reset_scope.alloc_escape(96);
            (void)reset_temp;
            (void)reset_escape;
            main::scope_reset(reset_scope);
            main::scope_release(reset_scope);

            interp.current_scope = releasing_scope;
            void* slow_temp = releasing_scope.alloc(8192 + i);
            void* slow_escape = releasing_scope.alloc_escape(4096 + i);
            (void)slow_temp;
            (void)slow_escape;

            Value* arr = make_array(interp, 1);
            Value* dict = make_hashmap_for_entry_count_checked(
                interp,
                BOUNDARY_BENCH_SHAPE_ITEMS,
                "boundary bench: dictionary out of memory");
            Value* set = make_set_for_entry_count_checked(
                interp,
                BOUNDARY_BENCH_SHAPE_ITEMS,
                "boundary bench: set out of memory");
            bool collection_ok =
                boundary_bench_fill_and_grow_array(interp, arr, i * 100) &&
                boundary_bench_fill_hashmap(interp, dict, i * 1000) &&
                boundary_bench_fill_set(interp, set, i * 2000);
            bool payload_ok = boundary_bench_exercise_payload_shapes(interp, i);

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                arr,
                interp,
                target_scope,
                releasing_scope,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);

            bool commit_ok = committed.value != null && committed.value.tag == ARRAY;
            if (collection_ok && payload_ok && commit_ok) shape_ok++;

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }
        }

        for (usz i = 0; i < BOUNDARY_BENCH_SHAPE_ITERS / 4; i++) {
            EvalResult r = run("(let (x 41 y 1) (lambda (z) (+ x y z)))", interp);
            if (!r.error.has_error && r.value != null && r.value.tag == CLOSURE) closure_env_ok++;
        }
        if (boundary_bench_exercise_stable_passport(interp)) stable_passport_ok++;
        long end_ms = boundary_bench_now_ms();
        shape_ms = end_ms > start_ms ? end_ms - start_ms : 0;
        shape_after = boundary_bench_counter_snapshot();
    }

    // Product-style benchmark: route-shaped Finwatch portfolio/quote payloads.
    {
        product_before = boundary_bench_counter_snapshot();
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_PRODUCT_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = releasing_scope;
            Value* response = boundary_bench_make_finwatch_response(interp, i);
            bool fixture_ok = response != null && response.tag == HASHMAP;

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = fixture_ok
                ? boundary_commit_escape(response, interp, target_scope, releasing_scope, &ctx)
                : (BoundaryCommitEscapeResult){};
            boundary_promotion_context_end(interp, &ctx);

            bool commit_ok = committed.value != null && committed.value.tag == HASHMAP;
            if (fixture_ok) product_ok++;
            if (commit_ok) product_commit_ok++;

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }
        }
        long end_ms = boundary_bench_now_ms();
        product_ms = end_ms > start_ms ? end_ms - start_ms : 0;
        product_after = boundary_bench_counter_snapshot();
    }

    // Closure-heavy iterator pipeline: return a lazy iterator with captured
    // mapping/filter closures across the top-level boundary, then consume it.
    {
        closure_iter_before = boundary_bench_counter_snapshot();
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_CLOSURE_ITER_ITERS; i++) {
            usz count = 0;
            long sum = 0;
            bool ok = boundary_bench_closure_iterator_pipeline_once(interp, &count, &sum) &&
                      count == BOUNDARY_BENCH_CLOSURE_ITER_EXPECTED_COUNT &&
                      sum == BOUNDARY_BENCH_CLOSURE_ITER_EXPECTED_SUM;
            if (ok) {
                closure_iter_ok++;
                closure_iter_count_total += count;
                closure_iter_sum_total += sum;
            }
        }
        long end_ms = boundary_bench_now_ms();
        closure_iter_ms = end_ms > start_ms ? end_ms - start_ms : 0;
        closure_iter_after = boundary_bench_counter_snapshot();
    }

    // Tensor metadata crossing: commit tensor-heavy graph payloads with shared
    // layout dictionaries so tensor storage and graph sharing stay visible.
    {
        tensor_metadata_before = boundary_bench_counter_snapshot();
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_TENSOR_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = releasing_scope;
            Value* response = boundary_bench_make_tensor_metadata_response(interp, i);
            bool fixture_ok = boundary_bench_tensor_metadata_response_ok(response, interp);

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = fixture_ok
                ? boundary_commit_escape(response, interp, target_scope, releasing_scope, &ctx)
                : (BoundaryCommitEscapeResult){};
            boundary_promotion_context_end(interp, &ctx);

            bool commit_ok = boundary_bench_tensor_metadata_response_ok(committed.value, interp);
            if (fixture_ok) tensor_metadata_ok++;
            if (commit_ok) tensor_metadata_commit_ok++;

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }
        }
        long end_ms = boundary_bench_now_ms();
        tensor_metadata_ms = end_ms > start_ms ? end_ms - start_ms : 0;
        tensor_metadata_after = boundary_bench_counter_snapshot();
    }

    // Nested module returns: repeatedly return graph payloads produced inside
    // nested module bodies across the boundary.
    {
        bool setup_ok = boundary_bench_setup_nested_module_return_fixture(interp);
        if (setup_ok) nested_module_setup_ok = 1;
        nested_module_before = boundary_bench_counter_snapshot();
        long start_ms = boundary_bench_now_ms();
        if (setup_ok) {
            for (usz i = 0; i < BOUNDARY_BENCH_NESTED_MODULE_ITERS; i++) {
                EvalResult batch_result = run("(mem006-return-outer.make-batch 5)", interp);
                if (!batch_result.error.has_error &&
                    boundary_bench_nested_module_batch_ok(batch_result.value, BOUNDARY_BENCH_NESTED_MODULE_INPUT)) {
                    nested_module_batch_ok++;
                }
            }
        }
        long end_ms = boundary_bench_now_ms();
        nested_module_ms = end_ms > start_ms ? end_ms - start_ms : 0;
        nested_module_after = boundary_bench_counter_snapshot();
    }

    // Structural equality benchmark: measure large nested equality throughput
    // and workspace heap-growth churn.
    {
        interp.current_scope = root_scope;
        Value* list_left = boundary_bench_make_nested_int_list(interp, EQUALITY_BENCH_NESTED_LIST_DEPTH);
        Value* list_right = boundary_bench_make_nested_int_list(interp, EQUALITY_BENCH_NESTED_LIST_DEPTH);
        Value* list_mismatch = boundary_bench_make_nested_int_list(interp, EQUALITY_BENCH_NESTED_LIST_DEPTH, 1);

        Value* array_left = boundary_bench_make_int_array(interp, EQUALITY_BENCH_ARRAY_LENGTH);
        Value* array_right = boundary_bench_make_int_array(interp, EQUALITY_BENCH_ARRAY_LENGTH);
        Value* array_mismatch = boundary_bench_make_int_array(interp, EQUALITY_BENCH_ARRAY_LENGTH, 1);
        Value* mixed_left = boundary_bench_make_mixed_nested_fixture(interp);
        Value* mixed_right = boundary_bench_make_mixed_nested_fixture(interp);
        Value* mixed_mismatch = boundary_bench_make_mixed_nested_fixture(interp, 1);

        equality_fixture_ok = list_left != null &&
            list_right != null &&
            list_mismatch != null &&
            array_left != null &&
            array_right != null &&
            array_mismatch != null &&
            mixed_left != null &&
            mixed_right != null &&
            mixed_mismatch != null;

        if (equality_fixture_ok) {
            long start_ms = boundary_bench_now_ms();
            for (usz i = 0; i < EQUALITY_BENCH_ITERS; i++) {
                EqualityWorkspaceStats stats = {};
                bool eq = values_equal_with_workspace_stats(list_left, list_right, &stats);
                if (eq) equality_list_true_ok++;
                boundary_bench_accumulate_equality_workspace(&equality_workspace, stats);
            }
            long list_true_done_ms = boundary_bench_now_ms();

            for (usz i = 0; i < EQUALITY_BENCH_ITERS; i++) {
                EqualityWorkspaceStats stats = {};
                bool eq = values_equal_with_workspace_stats(list_left, list_mismatch, &stats);
                if (!eq) equality_list_false_ok++;
                boundary_bench_accumulate_equality_workspace(&equality_workspace, stats);
            }
            long list_false_done_ms = boundary_bench_now_ms();

            for (usz i = 0; i < EQUALITY_BENCH_ITERS; i++) {
                EqualityWorkspaceStats stats = {};
                bool eq = values_equal_with_workspace_stats(array_left, array_right, &stats);
                if (eq) equality_array_true_ok++;
                boundary_bench_accumulate_equality_workspace(&equality_workspace, stats);
            }
            long array_true_done_ms = boundary_bench_now_ms();

            for (usz i = 0; i < EQUALITY_BENCH_ITERS; i++) {
                EqualityWorkspaceStats stats = {};
                bool eq = values_equal_with_workspace_stats(array_left, array_mismatch, &stats);
                if (!eq) equality_array_false_ok++;
                boundary_bench_accumulate_equality_workspace(&equality_workspace, stats);
            }
            long mixed_true_start_ms = boundary_bench_now_ms();

            for (usz i = 0; i < EQUALITY_BENCH_ITERS; i++) {
                EqualityWorkspaceStats stats = {};
                bool eq = values_equal_with_workspace_stats(mixed_left, mixed_right, &stats);
                if (eq) equality_mixed_true_ok++;
                boundary_bench_accumulate_equality_workspace(&equality_workspace, stats);
            }
            long mixed_true_done_ms = boundary_bench_now_ms();

            for (usz i = 0; i < EQUALITY_BENCH_ITERS; i++) {
                EqualityWorkspaceStats stats = {};
                bool eq = values_equal_with_workspace_stats(mixed_left, mixed_mismatch, &stats);
                if (!eq) equality_mixed_false_ok++;
                boundary_bench_accumulate_equality_workspace(&equality_workspace, stats);
            }
            long end_ms = boundary_bench_now_ms();

            equality_list_true_ms = list_true_done_ms > start_ms ? list_true_done_ms - start_ms : 0;
            equality_list_false_ms = list_false_done_ms > list_true_done_ms ? list_false_done_ms - list_true_done_ms : 0;
            equality_array_true_ms = array_true_done_ms > list_false_done_ms ? array_true_done_ms - list_false_done_ms : 0;
            equality_array_false_ms = mixed_true_start_ms > array_true_done_ms ? mixed_true_start_ms - array_true_done_ms : 0;
            equality_mixed_true_ms = mixed_true_done_ms > mixed_true_start_ms ? mixed_true_done_ms - mixed_true_start_ms : 0;
            equality_mixed_false_ms = end_ms > mixed_true_done_ms ? end_ms - mixed_true_done_ms : 0;
        }
    }

    interp.current_scope = root_scope;
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=boundary_decision_cost iters=%d splice_ms=%d disallowed_ms=%d reuse_ms=%d splice_ok=%d disallowed_ok=%d reuse_ok=%d",
        (long)BOUNDARY_BENCH_DECISION_ITERS,
        decision_splice_ms,
        decision_disallowed_ms,
        decision_reuse_ms,
        (long)decision_splice_ok,
        (long)decision_disallowed_ok,
        (long)decision_reuse_ok
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=boundary_destination_routed_escape iters=%d partial_ms=%d partial_ok=%d",
        (long)BOUNDARY_BENCH_DECISION_ITERS,
        decision_destination_reuse_ms,
        (long)decision_destination_reuse_ok
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=scope_splice_tail iters=%d escapes_per_iter=%d splice_ms=%d splice_ok=%d",
        (long)BOUNDARY_BENCH_SPLICE_ITERS,
        (long)BOUNDARY_BENCH_SPLICE_ESCAPES_PER_ITER,
        splice_tail_ms,
        (long)splice_tail_ok
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=equality_nested_workspace iters=%d fixture_ok=%d list_depth=%d array_len=%d list_true_ms=%d list_true_ok=%d list_false_ms=%d list_false_ok=%d array_true_ms=%d array_true_ok=%d array_false_ms=%d array_false_ok=%d mixed_true_ms=%d mixed_true_ok=%d mixed_false_ms=%d mixed_false_ok=%d stack_growth=%d seen_growth=%d stack_bytes=%d seen_bytes=%d",
        (long)EQUALITY_BENCH_ITERS,
        equality_fixture_ok ? 1 : 0,
        (long)EQUALITY_BENCH_NESTED_LIST_DEPTH,
        (long)EQUALITY_BENCH_ARRAY_LENGTH,
        equality_list_true_ms,
        (long)equality_list_true_ok,
        equality_list_false_ms,
        (long)equality_list_false_ok,
        equality_array_true_ms,
        (long)equality_array_true_ok,
        equality_array_false_ms,
        (long)equality_array_false_ok,
        equality_mixed_true_ms,
        (long)equality_mixed_true_ok,
        equality_mixed_false_ms,
        (long)equality_mixed_false_ok,
        (long)equality_workspace.stack_heap_growths,
        (long)equality_workspace.seen_heap_growths,
        (long)equality_workspace.stack_heap_bytes,
        (long)equality_workspace.seen_heap_bytes
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=finwatch_product_memory iters=%d quotes_per_iter=%d holdings_per_iter=%d product_ms=%d product_ok=%d commit_ok=%d hashmap_construct_delta=%d set_construct_delta=%d array_construct_delta=%d string_payload_bytes_delta=%d ffi_wrappers_delta=%d ffi_releasable_delta=%d selected_transplant_delta=%d materialization_copy_bytes_delta=%d",
        (long)BOUNDARY_BENCH_PRODUCT_ITERS,
        (long)BOUNDARY_BENCH_PRODUCT_QUOTES,
        (long)BOUNDARY_BENCH_PRODUCT_HOLDINGS,
        product_ms,
        (long)product_ok,
        (long)product_commit_ok,
        (long)boundary_bench_delta(product_after.value_shape.hashmap_construct_count, product_before.value_shape.hashmap_construct_count),
        (long)boundary_bench_delta(product_after.value_shape.set_construct_count, product_before.value_shape.set_construct_count),
        (long)boundary_bench_delta(product_after.value_shape.array_construct_count, product_before.value_shape.array_construct_count),
        (long)boundary_bench_delta(product_after.value_shape.string_payload_bytes_total, product_before.value_shape.string_payload_bytes_total),
        (long)boundary_bench_delta(product_after.value_shape.ffi_handle_wrapper_count, product_before.value_shape.ffi_handle_wrapper_count),
        (long)boundary_bench_delta(product_after.value_shape.ffi_handle_releasable_count, product_before.value_shape.ffi_handle_releasable_count),
        (long)boundary_bench_delta(
            product_after.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT],
            product_before.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT]
        ),
        (long)boundary_bench_delta(product_after.decisions.stable_materialization_copy_bytes_total, product_before.decisions.stable_materialization_copy_bytes_total)
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=closure_iterator_pipeline_memory iters=%d expected_count=%d expected_sum=%d pipeline_ms=%d pipeline_ok=%d count_total=%d sum_total=%d iterator_roots_delta=%d partial_roots_delta=%d closure_roots_delta=%d closure_env_frame_delta=%d closure_env_binding_delta=%d selected_transplant_delta=%d materialization_copy_bytes_delta=%d",
        (long)BOUNDARY_BENCH_CLOSURE_ITER_ITERS,
        (long)BOUNDARY_BENCH_CLOSURE_ITER_EXPECTED_COUNT,
        BOUNDARY_BENCH_CLOSURE_ITER_EXPECTED_SUM,
        closure_iter_ms,
        (long)closure_iter_ok,
        (long)closure_iter_count_total,
        closure_iter_sum_total,
        (long)boundary_bench_delta(closure_iter_after.escape_shape.iterator_roots, closure_iter_before.escape_shape.iterator_roots),
        (long)boundary_bench_delta(closure_iter_after.escape_shape.partial_roots, closure_iter_before.escape_shape.partial_roots),
        (long)boundary_bench_delta(closure_iter_after.escape_shape.closure_roots, closure_iter_before.escape_shape.closure_roots),
        (long)boundary_bench_delta(closure_iter_after.value_shape.closure_env_copy_frame_count, closure_iter_before.value_shape.closure_env_copy_frame_count),
        (long)boundary_bench_delta(closure_iter_after.value_shape.closure_env_copy_binding_count_total, closure_iter_before.value_shape.closure_env_copy_binding_count_total),
        (long)boundary_bench_delta(
            closure_iter_after.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT],
            closure_iter_before.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT]
        ),
        (long)boundary_bench_delta(closure_iter_after.decisions.stable_materialization_copy_bytes_total, closure_iter_before.decisions.stable_materialization_copy_bytes_total)
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=tensor_metadata_crossing_memory iters=%d tensors_per_iter=%d tensor_ms=%d tensor_ok=%d commit_ok=%d tensor_roots_delta=%d tensor_payload_bytes_delta=%d hashmap_construct_delta=%d array_construct_delta=%d selected_transplant_delta=%d materialization_copy_bytes_delta=%d",
        (long)BOUNDARY_BENCH_TENSOR_ITERS,
        (long)BOUNDARY_BENCH_TENSORS_PER_ITER,
        tensor_metadata_ms,
        (long)tensor_metadata_ok,
        (long)tensor_metadata_commit_ok,
        (long)boundary_bench_delta(tensor_metadata_after.escape_shape.tensor_roots, tensor_metadata_before.escape_shape.tensor_roots),
        (long)boundary_bench_delta(tensor_metadata_after.value_shape.tensor_payload_bytes_total, tensor_metadata_before.value_shape.tensor_payload_bytes_total),
        (long)boundary_bench_delta(tensor_metadata_after.value_shape.hashmap_construct_count, tensor_metadata_before.value_shape.hashmap_construct_count),
        (long)boundary_bench_delta(tensor_metadata_after.value_shape.array_construct_count, tensor_metadata_before.value_shape.array_construct_count),
        (long)boundary_bench_delta(
            tensor_metadata_after.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT],
            tensor_metadata_before.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT]
        ),
        (long)boundary_bench_delta(tensor_metadata_after.decisions.stable_materialization_copy_bytes_total, tensor_metadata_before.decisions.stable_materialization_copy_bytes_total)
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=nested_module_return_memory iters=%d nested_module_ms=%d setup_ok=%d batch_ok=%d cons_roots_delta=%d array_construct_delta=%d closure_env_frame_delta=%d closure_env_binding_delta=%d selected_transplant_delta=%d selected_stable_materialize_delta=%d materialization_copy_bytes_delta=%d",
        (long)BOUNDARY_BENCH_NESTED_MODULE_ITERS,
        nested_module_ms,
        (long)nested_module_setup_ok,
        (long)nested_module_batch_ok,
        (long)boundary_bench_delta(nested_module_after.escape_shape.cons_roots, nested_module_before.escape_shape.cons_roots),
        (long)boundary_bench_delta(nested_module_after.value_shape.array_construct_count, nested_module_before.value_shape.array_construct_count),
        (long)boundary_bench_delta(nested_module_after.value_shape.closure_env_copy_frame_count, nested_module_before.value_shape.closure_env_copy_frame_count),
        (long)boundary_bench_delta(nested_module_after.value_shape.closure_env_copy_binding_count_total, nested_module_before.value_shape.closure_env_copy_binding_count_total),
        (long)boundary_bench_delta(
            nested_module_after.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT],
            nested_module_before.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT]
        ),
        (long)boundary_bench_delta(
            nested_module_after.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION],
            nested_module_before.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION]
        ),
        (long)boundary_bench_delta(nested_module_after.decisions.stable_materialization_copy_bytes_total, nested_module_before.decisions.stable_materialization_copy_bytes_total)
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=boundary_value_shape_counters iters=%d shape_ms=%d shape_ok=%d closure_env_ok=%d stable_passport_ok=%d temp_slow_delta=%d escape_slow_delta=%d temp_slow_requested_delta=%d escape_slow_requested_delta=%d temp_selected_chunk_delta=%d escape_selected_chunk_delta=%d temp_slow_slack_exact_delta=%d temp_slow_slack_le512_delta=%d temp_slow_slack_le4096_delta=%d temp_slow_slack_gt4096_delta=%d escape_slow_slack_exact_delta=%d escape_slow_slack_le512_delta=%d escape_slow_slack_le4096_delta=%d escape_slow_slack_gt4096_delta=%d temp_slow_sequence_closed_delta=%d escape_slow_sequence_closed_delta=%d temp_slow_sequence_followup_alloc_delta=%d escape_slow_sequence_followup_alloc_delta=%d temp_slow_sequence_followup_bytes_delta=%d escape_slow_sequence_followup_bytes_delta=%d temp_slow_sequence_unused_close_delta=%d escape_slow_sequence_unused_close_delta=%d temp_slow_sequence_no_followup_delta=%d escape_slow_sequence_no_followup_delta=%d escape_slow_sequence_no_followup_request_le512_delta=%d escape_slow_sequence_no_followup_request_le4096_delta=%d escape_slow_sequence_no_followup_request_gt4096_delta=%d escape_slow_sequence_no_followup_unused_exact_delta=%d escape_slow_sequence_no_followup_unused_le512_delta=%d escape_slow_sequence_no_followup_unused_le4096_delta=%d escape_slow_sequence_no_followup_unused_gt4096_delta=%d escape_slow_sequence_no_followup_source_unknown_delta=%d escape_slow_sequence_no_followup_source_direct_delta=%d escape_slow_sequence_no_followup_source_dtor_delta=%d escape_slow_sequence_no_followup_source_interp_value_delta=%d escape_slow_sequence_no_followup_source_interp_env_delta=%d escape_slow_sequence_no_followup_source_boundary_payload_delta=%d escape_slow_sequence_no_followup_source_promotion_signature_delta=%d escape_slow_sequence_no_followup_source_promotion_closure_delta=%d escape_slow_sequence_no_followup_source_jit_args_delta=%d temp_slow_sequence_large_delta=%d escape_slow_sequence_large_delta=%d temp_slow_sequence_large_followup_bytes_delta=%d escape_slow_sequence_large_followup_bytes_delta=%d temp_slow_sequence_large_unused_close_delta=%d escape_slow_sequence_large_unused_close_delta=%d temp_slow_sequence_large_no_followup_delta=%d escape_slow_sequence_large_no_followup_delta=%d temp_reset_slack_delta=%d escape_reset_slack_delta=%d temp_destroy_slack_delta=%d escape_destroy_slack_delta=%d scope_fresh_delta=%d scope_recycle_hit_delta=%d array_construct_delta=%d array_growth_delta=%d hashmap_construct_delta=%d hashmap_growth_delta=%d set_construct_delta=%d set_growth_delta=%d closure_env_frame_delta=%d closure_env_binding_delta=%d string_payload_bytes_delta=%d error_payload_bytes_delta=%d big_integer_wrappers_delta=%d big_integer_input_bytes_delta=%d tensor_payload_bytes_delta=%d ffi_wrappers_delta=%d ffi_releasable_delta=%d stable_stale_delta=%d selected_transplant_delta=%d materialization_copy_bytes_delta=%d",
        (long)BOUNDARY_BENCH_SHAPE_ITERS,
        shape_ms,
        (long)shape_ok,
        (long)closure_env_ok,
        (long)stable_passport_ok,
        (long)boundary_bench_delta(shape_after.scope.temp_slow_alloc_count, shape_before.scope.temp_slow_alloc_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_alloc_count, shape_before.scope.escape_slow_alloc_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_requested_bytes, shape_before.scope.temp_slow_requested_bytes),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_requested_bytes, shape_before.scope.escape_slow_requested_bytes),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_selected_chunk_bytes, shape_before.scope.temp_slow_selected_chunk_bytes),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_selected_chunk_bytes, shape_before.scope.escape_slow_selected_chunk_bytes),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_slack_exact_count, shape_before.scope.temp_slow_slack_exact_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_slack_le_512_count, shape_before.scope.temp_slow_slack_le_512_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_slack_le_4096_count, shape_before.scope.temp_slow_slack_le_4096_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_slack_gt_4096_count, shape_before.scope.temp_slow_slack_gt_4096_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_slack_exact_count, shape_before.scope.escape_slow_slack_exact_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_slack_le_512_count, shape_before.scope.escape_slow_slack_le_512_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_slack_le_4096_count, shape_before.scope.escape_slow_slack_le_4096_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_slack_gt_4096_count, shape_before.scope.escape_slow_slack_gt_4096_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_closed_count, shape_before.scope.temp_slow_sequence_closed_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_closed_count, shape_before.scope.escape_slow_sequence_closed_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_followup_alloc_count, shape_before.scope.temp_slow_sequence_followup_alloc_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_followup_alloc_count, shape_before.scope.escape_slow_sequence_followup_alloc_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_followup_bytes, shape_before.scope.temp_slow_sequence_followup_bytes),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_followup_bytes, shape_before.scope.escape_slow_sequence_followup_bytes),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_unused_at_close_bytes, shape_before.scope.temp_slow_sequence_unused_at_close_bytes),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_unused_at_close_bytes, shape_before.scope.escape_slow_sequence_unused_at_close_bytes),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_no_followup_count, shape_before.scope.temp_slow_sequence_no_followup_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_count, shape_before.scope.escape_slow_sequence_no_followup_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_request_le_512_count, shape_before.scope.escape_slow_sequence_no_followup_request_le_512_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_request_le_4096_count, shape_before.scope.escape_slow_sequence_no_followup_request_le_4096_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_request_gt_4096_count, shape_before.scope.escape_slow_sequence_no_followup_request_gt_4096_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_unused_exact_count, shape_before.scope.escape_slow_sequence_no_followup_unused_exact_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_unused_le_512_count, shape_before.scope.escape_slow_sequence_no_followup_unused_le_512_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_unused_le_4096_count, shape_before.scope.escape_slow_sequence_no_followup_unused_le_4096_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_unused_gt_4096_count, shape_before.scope.escape_slow_sequence_no_followup_unused_gt_4096_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_unknown_count, shape_before.scope.escape_slow_sequence_no_followup_source_unknown_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_direct_count, shape_before.scope.escape_slow_sequence_no_followup_source_direct_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_dtor_count, shape_before.scope.escape_slow_sequence_no_followup_source_dtor_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_interp_value_count, shape_before.scope.escape_slow_sequence_no_followup_source_interp_value_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_interp_env_count, shape_before.scope.escape_slow_sequence_no_followup_source_interp_env_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_boundary_payload_count, shape_before.scope.escape_slow_sequence_no_followup_source_boundary_payload_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_promotion_signature_count, shape_before.scope.escape_slow_sequence_no_followup_source_promotion_signature_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_promotion_closure_count, shape_before.scope.escape_slow_sequence_no_followup_source_promotion_closure_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_no_followup_source_jit_args_count, shape_before.scope.escape_slow_sequence_no_followup_source_jit_args_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_large_slack_count, shape_before.scope.temp_slow_sequence_large_slack_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_large_slack_count, shape_before.scope.escape_slow_sequence_large_slack_count),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_large_slack_followup_bytes, shape_before.scope.temp_slow_sequence_large_slack_followup_bytes),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_large_slack_followup_bytes, shape_before.scope.escape_slow_sequence_large_slack_followup_bytes),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_large_slack_unused_at_close_bytes, shape_before.scope.temp_slow_sequence_large_slack_unused_at_close_bytes),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_large_slack_unused_at_close_bytes, shape_before.scope.escape_slow_sequence_large_slack_unused_at_close_bytes),
        (long)boundary_bench_delta(shape_after.scope.temp_slow_sequence_large_slack_no_followup_count, shape_before.scope.temp_slow_sequence_large_slack_no_followup_count),
        (long)boundary_bench_delta(shape_after.scope.escape_slow_sequence_large_slack_no_followup_count, shape_before.scope.escape_slow_sequence_large_slack_no_followup_count),
        (long)boundary_bench_delta(shape_after.scope.temp_unused_bytes_at_reset, shape_before.scope.temp_unused_bytes_at_reset),
        (long)boundary_bench_delta(shape_after.scope.escape_unused_bytes_at_reset, shape_before.scope.escape_unused_bytes_at_reset),
        (long)boundary_bench_delta(shape_after.scope.temp_unused_bytes_at_destroy, shape_before.scope.temp_unused_bytes_at_destroy),
        (long)boundary_bench_delta(shape_after.scope.escape_unused_bytes_at_destroy, shape_before.scope.escape_unused_bytes_at_destroy),
        (long)boundary_bench_delta(shape_after.scope.scope_fresh_alloc_count, shape_before.scope.scope_fresh_alloc_count),
        (long)boundary_bench_delta(shape_after.scope.scope_recycle_hit_count, shape_before.scope.scope_recycle_hit_count),
        (long)boundary_bench_delta(shape_after.value_shape.array_construct_count, shape_before.value_shape.array_construct_count),
        (long)boundary_bench_delta(shape_after.value_shape.array_growth_count, shape_before.value_shape.array_growth_count),
        (long)boundary_bench_delta(shape_after.value_shape.hashmap_construct_count, shape_before.value_shape.hashmap_construct_count),
        (long)boundary_bench_delta(shape_after.value_shape.hashmap_growth_count, shape_before.value_shape.hashmap_growth_count),
        (long)boundary_bench_delta(shape_after.value_shape.set_construct_count, shape_before.value_shape.set_construct_count),
        (long)boundary_bench_delta(shape_after.value_shape.set_growth_count, shape_before.value_shape.set_growth_count),
        (long)boundary_bench_delta(shape_after.value_shape.closure_env_copy_frame_count, shape_before.value_shape.closure_env_copy_frame_count),
        (long)boundary_bench_delta(shape_after.value_shape.closure_env_copy_binding_count_total, shape_before.value_shape.closure_env_copy_binding_count_total),
        (long)boundary_bench_delta(shape_after.value_shape.string_payload_bytes_total, shape_before.value_shape.string_payload_bytes_total),
        (long)boundary_bench_delta(shape_after.value_shape.error_payload_bytes_total, shape_before.value_shape.error_payload_bytes_total),
        (long)boundary_bench_delta(shape_after.value_shape.big_integer_wrapper_count, shape_before.value_shape.big_integer_wrapper_count),
        (long)boundary_bench_delta(shape_after.value_shape.big_integer_decimal_input_bytes_total, shape_before.value_shape.big_integer_decimal_input_bytes_total),
        (long)boundary_bench_delta(shape_after.value_shape.tensor_payload_bytes_total, shape_before.value_shape.tensor_payload_bytes_total),
        (long)boundary_bench_delta(shape_after.value_shape.ffi_handle_wrapper_count, shape_before.value_shape.ffi_handle_wrapper_count),
        (long)boundary_bench_delta(shape_after.value_shape.ffi_handle_releasable_count, shape_before.value_shape.ffi_handle_releasable_count),
        (long)boundary_bench_delta(shape_after.value_shape.stable_passport_prepared_graph_stale_count, shape_before.value_shape.stable_passport_prepared_graph_stale_count),
        (long)boundary_bench_delta(
            shape_after.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT],
            shape_before.decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT]
        ),
        (long)boundary_bench_delta(shape_after.decisions.stable_materialization_copy_bytes_total, shape_before.decisions.stable_materialization_copy_bytes_total)
    );
}
