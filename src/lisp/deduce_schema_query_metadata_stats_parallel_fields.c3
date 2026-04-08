module lisp;

import std::core::mem;

fn Value* deduce_stats_set_parallel_topology_fields(
    Value* out,
    Relation* rel,
    Interp* interp
) {
    DeduceSccPlan scc_plan = {};
    Value* plan_err = deduce_build_scc_plan(rel.db, &scc_plan, interp);
    if (plan_err != null) return plan_err;
    defer deduce_scc_plan_free(&scc_plan);

    long parallel_recursive_component_count = 0;
    long parallel_batched_component_count = 0;
    long parallel_batch_count = 0;
    long parallel_max_batch_size = 0;
    usz[] parallel_wave_ids = {};
    usz[] parallel_batch_sizes = {};
    Value* parallel_meta_err = deduce_build_parallel_component_batch_metadata(
        &scc_plan,
        &parallel_wave_ids,
        &parallel_batch_sizes,
        interp
    );
    if (parallel_meta_err != null) return parallel_meta_err;
    defer if (parallel_wave_ids.ptr != null) mem::free(parallel_wave_ids.ptr);
    defer if (parallel_batch_sizes.ptr != null) mem::free(parallel_batch_sizes.ptr);

    deduce_parallel_batch_topology_counts_from_metadata(
        &scc_plan,
        parallel_wave_ids[..],
        parallel_batch_sizes[..],
        &parallel_recursive_component_count,
        &parallel_batched_component_count,
        &parallel_batch_count,
        &parallel_max_batch_size
    );

    explain_dict_set(out, interp, "parallel-recursive-component-count",
        make_int(interp, parallel_recursive_component_count));
    explain_dict_set(out, interp, "parallel-batched-component-count",
        make_int(interp, parallel_batched_component_count));
    explain_dict_set(out, interp, "parallel-batch-count",
        make_int(interp, parallel_batch_count));
    explain_dict_set(out, interp, "parallel-max-batch-size",
        make_int(interp, parallel_max_batch_size));
    explain_dict_set_symbol(out, interp, "parallel-runtime-mode",
        interp.symbols.intern("metadata-only"));
    explain_dict_set(out, interp, "parallel-batch-topology",
        deduce_parallel_component_batch_topology_payload(
            &scc_plan,
            parallel_wave_ids[..],
            parallel_batch_sizes[..],
            interp
        ));
    return null;
}
