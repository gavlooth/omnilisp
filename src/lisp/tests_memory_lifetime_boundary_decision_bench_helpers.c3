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
    snapshot.scope = main::scope_memory_telemetry_stats_snapshot();
    snapshot.fiber = main::fiber_temp_pool_stats_snapshot();
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
