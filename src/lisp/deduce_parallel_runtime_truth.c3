module lisp;

fn SymbolId deduce_parallel_runtime_mode_symbol(
    long parallel_batched_component_count,
    long parallel_max_batch_size,
    Interp* interp
) {
    if (interp == null) return (SymbolId)0;
    if (parallel_batched_component_count > 0 || parallel_max_batch_size > 1) {
        return interp.symbols.intern("serial-runtime-parallel-topology-planned");
    }
    return interp.symbols.intern("serial-runtime");
}

fn Value* deduce_set_parallel_runtime_truth_fields(
    Value* out,
    long parallel_batched_component_count,
    long parallel_max_batch_size,
    Interp* interp
) {
    if (out == null || interp == null) return null;
    if (Value* err = explain_dict_set_symbol( out, interp, "parallel-runtime-mode", deduce_parallel_runtime_mode_symbol( parallel_batched_component_count, parallel_max_batch_size, interp ) )) return err;
    // Runtime currently executes SCC components serially.
    if (Value* err = explain_dict_set(out, interp, "parallel-runtime-executed-batch-count", make_int(interp, 0))) return err;
    if (Value* err = explain_dict_set(out, interp, "parallel-runtime-executed-component-count", make_int(interp, 0))) return err;
    if (Value* err = explain_dict_set(out, interp, "parallel-runtime-max-concurrency", make_int(interp, 1))) return err;
    return null;
}
