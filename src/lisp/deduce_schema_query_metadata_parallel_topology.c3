module lisp;

fn void deduce_parallel_batch_topology_counts_from_metadata(
    DeduceSccPlan* scc_plan,
    usz[] parallel_wave_ids,
    usz[] parallel_batch_sizes,
    long* recursive_component_count_out,
    long* batched_component_count_out,
    long* batch_count_out,
    long* max_batch_size_out
) {
    if (recursive_component_count_out != null) *recursive_component_count_out = 0;
    if (batched_component_count_out != null) *batched_component_count_out = 0;
    if (batch_count_out != null) *batch_count_out = 0;
    if (max_batch_size_out != null) *max_batch_size_out = 0;
    if (scc_plan == null) return;

    long recursive_component_count = 0;
    long batched_component_count = 0;
    long batch_count = 0;
    long max_batch_size = 0;
    for (usz component_id = 0; component_id < scc_plan.component_count; component_id++) {
        bool recursive =
            scc_plan.component_recursive.ptr != null &&
            component_id < scc_plan.component_recursive.len &&
            scc_plan.component_recursive[component_id];
        if (!recursive) continue;

        recursive_component_count++;

        long batch_size =
            component_id < parallel_batch_sizes.len ? (long)parallel_batch_sizes[component_id] : 0;
        if (batch_size > 1) batched_component_count++;
        if (batch_size > max_batch_size) max_batch_size = batch_size;

        bool batch_seen = false;
        if (component_id < parallel_wave_ids.len && component_id < scc_plan.component_strata.len) {
            usz stratum = scc_plan.component_strata[component_id];
            usz wave = parallel_wave_ids[component_id];
            for (usz prev = 0; prev < component_id; prev++) {
                bool prev_recursive =
                    scc_plan.component_recursive.ptr != null &&
                    prev < scc_plan.component_recursive.len &&
                    scc_plan.component_recursive[prev];
                if (!prev_recursive) continue;
                if (prev >= parallel_wave_ids.len || prev >= scc_plan.component_strata.len) continue;
                if (scc_plan.component_strata[prev] != stratum) continue;
                if (parallel_wave_ids[prev] != wave) continue;
                batch_seen = true;
                break;
            }
        }
        if (!batch_seen) batch_count++;
    }

    if (recursive_component_count_out != null) *recursive_component_count_out = recursive_component_count;
    if (batched_component_count_out != null) *batched_component_count_out = batched_component_count;
    if (batch_count_out != null) *batch_count_out = batch_count;
    if (max_batch_size_out != null) *max_batch_size_out = max_batch_size;
}
