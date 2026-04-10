module lisp;

fn Value* deduce_build_parallel_component_batch_metadata(
    DeduceSccPlan* plan,
    usz[]* wave_ids_out,
    usz[]* batch_sizes_out,
    Interp* interp
) {
    if (wave_ids_out == null || batch_sizes_out == null || plan == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state",
            "deduce/analyze: invalid parallel batch state");
    }

    *wave_ids_out = {};
    *batch_sizes_out = {};
    if (plan.component_count == 0) return null;

    usz[] wave_ids = mem::new_array(usz, plan.component_count);
    if (wave_ids.ptr == null) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    usz[] batch_sizes = mem::new_array(usz, plan.component_count);
    if (batch_sizes.ptr == null) {
        mem::free(wave_ids.ptr);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }

    for (usz i = 0; i < wave_ids.len; i++) {
        wave_ids[i] = 0;
        batch_sizes[i] = 0;
    }

    for (usz pos = 0; pos < plan.component_order.len; pos++) {
        usz component_id = plan.component_order[pos];
        if (component_id >= plan.component_count || component_id >= plan.component_strata.len) continue;

        usz stratum = plan.component_strata[component_id];
        usz wave = 0;
        foreach (edge : plan.edges) {
            if (edge.from < 0 || edge.to < 0) continue;
            if ((usz)edge.from >= plan.predicate_component.len || (usz)edge.to >= plan.predicate_component.len) continue;

            int from_component = plan.predicate_component[(usz)edge.from];
            int to_component = plan.predicate_component[(usz)edge.to];
            if (from_component < 0 || to_component < 0 || from_component == to_component) continue;
            if ((usz)from_component != component_id) continue;
            if ((usz)to_component >= plan.component_strata.len || plan.component_strata[(usz)to_component] != stratum) continue;

            usz candidate_wave = wave_ids[(usz)to_component] + 1;
            if (candidate_wave > wave) wave = candidate_wave;
        }
        wave_ids[component_id] = wave;
    }

    for (usz component_id = 0; component_id < plan.component_count; component_id++) {
        if (component_id >= plan.component_strata.len || component_id >= wave_ids.len) continue;
        usz count = 0;
        usz stratum = plan.component_strata[component_id];
        usz wave = wave_ids[component_id];
        for (usz other = 0; other < plan.component_count; other++) {
            if (other >= plan.component_strata.len || other >= wave_ids.len) continue;
            if (plan.component_strata[other] != stratum) continue;
            if (wave_ids[other] != wave) continue;
            count++;
        }
        batch_sizes[component_id] = count;
    }

    *wave_ids_out = wave_ids;
    *batch_sizes_out = batch_sizes;
    return null;
}

fn Value* deduce_parallel_component_batch_topology_payload(
    DeduceSccPlan* plan,
    usz[] wave_ids,
    usz[] batch_sizes,
    Interp* interp
) {
    if (plan == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid parallel batch state");
    }
    if (plan.component_count == 0) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz component_id = 0; component_id < plan.component_count; component_id++) {
        bool recursive =
            plan.component_recursive.ptr != null &&
            component_id < plan.component_recursive.len &&
            plan.component_recursive[component_id];
        if (!recursive) continue;

        bool batch_seen = false;
        if (component_id < wave_ids.len && component_id < plan.component_strata.len) {
            usz stratum = plan.component_strata[component_id];
            usz wave = wave_ids[component_id];
            for (usz prev = 0; prev < component_id; prev++) {
                bool prev_recursive =
                    plan.component_recursive.ptr != null &&
                    prev < plan.component_recursive.len &&
                    plan.component_recursive[prev];
                if (!prev_recursive) continue;
                if (prev >= wave_ids.len || prev >= plan.component_strata.len) continue;
                if (plan.component_strata[prev] != stratum) continue;
                if (wave_ids[prev] != wave) continue;
                batch_seen = true;
                break;
            }
        }
        if (batch_seen) continue;

        Value* entry = make_hashmap_checked(interp, 8, "deduce/analyze: out of memory");
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }

        Value* predicate_rev = make_nil(interp);
        for (usz predicate_id = 0; predicate_id < plan.predicates.len; predicate_id++) {
            if (predicate_id >= plan.predicate_component.len) continue;
            if (plan.predicate_component[predicate_id] != (int)component_id) continue;
            predicate_rev = make_cons(
                interp,
                make_symbol(interp, plan.predicates[predicate_id]),
                predicate_rev
            );
        }

        if (Value* err = explain_dict_set(entry, interp, "leader-component-id", make_int(interp, (long)component_id))) {
            (void)err;
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        if (Value* err = explain_dict_set(entry, interp, "stratum",
                make_int(interp, component_id < plan.component_strata.len ? (long)plan.component_strata[component_id] : 0))) {
            (void)err;
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        if (Value* err = explain_dict_set(entry, interp, "wave",
                make_int(interp, component_id < wave_ids.len ? (long)wave_ids[component_id] : 0))) {
            (void)err;
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        if (Value* err = explain_dict_set(entry, interp, "batch-size",
                make_int(interp, component_id < batch_sizes.len ? (long)batch_sizes[component_id] : 0))) {
            (void)err;
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        if (Value* err = explain_dict_set(entry, interp, "component-predicates", deduce_reverse_list(predicate_rev, interp))) {
            (void)err;
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        rev = make_cons(interp, entry, rev);
    }

    return deduce_reverse_list(rev, interp);
}
