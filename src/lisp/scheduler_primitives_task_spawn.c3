module lisp;

import std::core::mem;

// ============================================================
// (__raw-task-spawn (op arg...)) -> task-handle
//
// Minimal threaded task spawn API over sendable offload jobs.
// ============================================================

fn Value* scheduler_prepare_task_spawn(OffloadWork* work, usz* task_id_out, Interp* interp) {
    usz task_id = scheduler_alloc_thread_task();
    if (task_id == NO_TASK) {
        scheduler_release_shared(work.shared);
        return scheduler_raise(interp, "scheduler/task-spawn-task-table-full", "task-spawn: task table full");
    }

    work.fiber_id = NO_FIBER;
    work.task_id = task_id;
    if (!scheduler_enqueue_offload(*work)) {
        scheduler_release_shared(work.shared);
        scheduler_drop_thread_task(task_id);
        return scheduler_raise(
            interp,
            "scheduler/task-spawn-admission-full",
            "task-spawn: admission cap reached"
        );
    }

    *task_id_out = task_id;
    return null;
}

<*
@require interp != null
*>
fn Value* prim_task_spawn(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    scheduler_init();
    if (!g_scheduler.thread_task_mu_ready) {
        return scheduler_raise(
            interp,
            "scheduler/task-mutex-unavailable",
            "task-spawn: task mutex unavailable"
        );
    }
    OffloadWork work;
    Value* parse_err = scheduler_parse_offload_job(
        args,
        "scheduler/task-spawn-expected-job-list",
        "task-spawn: expected job list",
        false,
        &work,
        interp
    );
    if (parse_err != null) return parse_err;

    usz task_id = NO_TASK;
    Value* spawn_err = scheduler_prepare_task_spawn(&work, &task_id, interp);
    if (spawn_err != null) return spawn_err;

    uint generation = 0;
    if (!scheduler_get_thread_task_generation(task_id, &generation) || generation == 0) {
        scheduler_drop_thread_task(task_id);
        return scheduler_raise(
            interp,
            "scheduler/task-spawn-task-handle-invalid",
            "task-spawn: failed to create task handle"
        );
    }

    return scheduler_make_task_handle_value(task_id, generation, interp);
}

<*
@require interp != null
*>
fn Value* prim_task_spawn_batch(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    scheduler_init();
    if (!g_scheduler.thread_task_mu_ready) {
        return scheduler_raise(
            interp,
            "scheduler/task-mutex-unavailable",
            "task-spawn-batch: task mutex unavailable"
        );
    }
    if (args.len < 1) {
        return scheduler_raise(
            interp,
            "scheduler/task-spawn-batch-expected-job-list",
            "task-spawn-batch: expected job list"
        );
    }

    Value* jobs = args[0];
    if (jobs == null || is_nil(jobs)) {
        return make_nil(interp);
    }

    usz job_count = 0;
    Value* cur = jobs;
    while (is_cons(cur)) {
        job_count++;
        cur = cdr(cur);
    }
    if (!is_nil(cur)) {
        return scheduler_raise(
            interp,
            "scheduler/task-spawn-batch-expected-job-list",
            "task-spawn-batch: expected job list"
        );
    }
    if (job_count == 0) {
        return make_nil(interp);
    }

    OffloadWork[] works = mem::new_array(OffloadWork, job_count);
    if (works.ptr == null) {
        return scheduler_raise(
            interp,
            "scheduler/task-spawn-batch-out-of-memory",
            "task-spawn-batch: out of memory"
        );
    }
    defer mem::free(works.ptr);

    cur = jobs;
    for (usz i = 0; i < job_count; i++) {
        works[i] = {};
        Value*[1] wrapped;
        wrapped[0] = car(cur);
        Value* parse_err = scheduler_parse_offload_job(
            wrapped[..],
            "scheduler/task-spawn-batch-expected-job-list",
            "task-spawn-batch: expected job list",
            false,
            &works[i],
            interp
        );
        if (parse_err != null) return parse_err;
        cur = cdr(cur);
    }

    usz[] task_ids = mem::new_array(usz, job_count);
    uint[] generations = mem::new_array(uint, job_count);
    if (task_ids.ptr == null || generations.ptr == null) {
        if (task_ids.ptr != null) mem::free(task_ids.ptr);
        if (generations.ptr != null) mem::free(generations.ptr);
        return scheduler_raise(
            interp,
            "scheduler/task-spawn-batch-out-of-memory",
            "task-spawn-batch: out of memory"
        );
    }
    defer mem::free(task_ids.ptr);
    defer mem::free(generations.ptr);

    usz spawned_count = 0;
    for (usz i = 0; i < job_count; i++) {
        usz task_id = NO_TASK;
        Value* spawn_err = scheduler_prepare_task_spawn(&works[i], &task_id, interp);
        if (spawn_err != null) {
            for (usz j = 0; j < spawned_count; j++) {
                if (task_ids[j] != NO_TASK && scheduler_thread_task_exists(task_ids[j], generations[j])) {
                    scheduler_drop_thread_task(task_ids[j], generations[j]);
                }
            }
            return spawn_err;
        }

        uint generation = 0;
        if (!scheduler_get_thread_task_generation(task_id, &generation) || generation == 0) {
            scheduler_drop_thread_task(task_id);
            for (usz j = 0; j < spawned_count; j++) {
                if (task_ids[j] != NO_TASK && scheduler_thread_task_exists(task_ids[j], generations[j])) {
                    scheduler_drop_thread_task(task_ids[j], generations[j]);
                }
            }
            return scheduler_raise(
                interp,
                "scheduler/task-spawn-task-handle-invalid",
                "task-spawn-batch: failed to create task handle"
            );
        }

        task_ids[i] = task_id;
        generations[i] = generation;
        spawned_count++;
    }

    Value*[] handle_vals = mem::new_array(Value*, job_count);
    if (handle_vals.ptr == null) {
        for (usz j = 0; j < spawned_count; j++) {
            if (task_ids[j] != NO_TASK && scheduler_thread_task_exists(task_ids[j], generations[j])) {
                scheduler_drop_thread_task(task_ids[j], generations[j]);
            }
        }
        return scheduler_raise(
            interp,
            "scheduler/task-spawn-batch-out-of-memory",
            "task-spawn-batch: out of memory"
        );
    }
    defer mem::free(handle_vals.ptr);

    for (usz i = 0; i < job_count; i++) {
        Value* handle_v = scheduler_make_task_handle_value(task_ids[i], generations[i], interp);
        if (handle_v == null || handle_v.tag == ERROR) {
            if (task_ids[i] != NO_TASK && scheduler_thread_task_exists(task_ids[i], generations[i])) {
                scheduler_drop_thread_task(task_ids[i], generations[i]);
            }
            for (usz j = 0; j < spawned_count; j++) {
                if (task_ids[j] != NO_TASK && scheduler_thread_task_exists(task_ids[j], generations[j])) {
                    scheduler_drop_thread_task(task_ids[j], generations[j]);
                }
            }
            return handle_v;
        }
        handle_vals[i] = handle_v;
    }

    Value* results_rev = make_nil(interp);
    for (usz i = job_count; i > 0; i--) {
        results_rev = scheduler_prepend_result_or_error(
            handle_vals[i - 1],
            results_rev,
            "scheduler/task-spawn-batch-out-of-memory",
            "task-spawn-batch: out of memory",
            interp
        );
        if (results_rev != null && results_rev.tag == ERROR) {
            for (usz j = 0; j < spawned_count; j++) {
                if (task_ids[j] != NO_TASK && scheduler_thread_task_exists(task_ids[j], generations[j])) {
                    scheduler_drop_thread_task(task_ids[j], generations[j]);
                }
            }
            return results_rev;
        }
    }
    return results_rev;
}
