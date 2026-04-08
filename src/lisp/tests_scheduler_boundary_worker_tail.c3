module lisp;

import std::core::mem;
import std::io;

fn void run_scheduler_thread_task_nonexecuted_payload_release_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: if libuv after-work fires without executing work_cb (for
    // example cancelled while queued), queued shared payload is still released
    // and the task converges to one terminal completion.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    bool ok = true;
    char[] failed_phase = "";

    if (!scheduler_shared_registry_ensure_ready()) {
        ok = false;
        failed_phase = "registry-init";
    }

    if (ok) {
        scheduler_shared_registry_drain_retired(SHARED_RETIRED_QUEUE_CAPACITY);
    }

    usz task_id = NO_TASK;
    uint generation = 0;
    void* req = null;
    usz shared_slot = SHARED_REGISTRY_CAPACITY;
    uint shared_generation = 0;
    if (ok) {
        task_id = scheduler_alloc_thread_task();
        if (task_id == NO_TASK) {
            ok = false;
            failed_phase = "task-alloc";
        }
    }

    if (ok) {
        req = omni_uv_work_req_new();
        if (req == null) {
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_phase = "req-alloc";
        }
    }

    if (ok) {
        OffloadWork work = {};
        work.fiber_id = NO_FIBER;
        work.task_id = task_id;
        work.op = OFFLOAD_SLEEP_MS;
        work.int_arg = 5;
        work.shared = scheduler_publish_shared("cancel-before-work");
        if (work.shared == null) {
            omni_uv_work_req_free(req);
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_phase = "shared-publish";
        } else if (!scheduler_bind_thread_task_request(task_id, req, &generation) || generation == 0) {
            scheduler_release_shared(work.shared);
            omni_uv_work_req_free(req);
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_phase = "bind-request";
        } else {
            shared_slot = work.shared.slot;
            shared_generation = work.shared.generation;
            QueuedOffloadWork* queued = (QueuedOffloadWork*)mem::malloc(QueuedOffloadWork.sizeof);
            if (queued == null) {
                scheduler_release_shared(work.shared);
                scheduler_clear_thread_task_request(task_id, req, generation);
                omni_uv_work_req_free(req);
                scheduler_drop_thread_task(task_id, generation);
                ok = false;
                failed_phase = "queued-alloc";
            } else {
                *queued = {};
                queued.work = work;
                queued.task_generation = generation;
                queued.executed = false;
                queued.completion = null;
                omni_uv_req_set_data(req, queued);
                scheduler_uv_offload_after_cb(req, -1);
            }
        }
    }

    if (ok) {
        OffloadCompletion* completion = scheduler_take_thread_task_completion(task_id, generation);
        bool completion_ok = completion != null &&
                             completion.kind == OFFLOAD_RES_ERROR &&
                             !scheduler_thread_task_exists(task_id, generation);
        if (completion != null) scheduler_free_offload_completion(completion);

        SharedHandle probe = {};
        probe.kind = SHARED_HANDLE_BLOB;
        probe.slot = shared_slot;
        probe.generation = shared_generation;
        probe.payload = null;
        bool shared_released = scheduler_shared_registry_resolve_blob(&probe) == null;

        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        ok = completion_ok && shared_released && state_ok;
        if (!ok) {
            if (!completion_ok) {
                failed_phase = "completion";
            } else if (!shared_released) {
                failed_phase = "shared-release";
            } else {
                failed_phase = "boundary";
            }
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler non-executed worker cancel releases queued payload");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler non-executed worker cancel releases queued payload (%s)",
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_worker_prestart_cancel_single_shared_release_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: when task begin is rejected before execution, shared payload
    // ownership is released exactly once across worker + after-work callbacks.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    bool ok = true;
    char[] failed_phase = "";

    if (!scheduler_shared_registry_ensure_ready()) {
        ok = false;
        failed_phase = "registry-init";
    }
    if (ok) {
        scheduler_shared_registry_drain_retired(SHARED_RETIRED_QUEUE_CAPACITY);
    }

    usz task_id = NO_TASK;
    uint generation = 0;
    void* req = null;
    usz shared_slot = SHARED_REGISTRY_CAPACITY;
    uint shared_generation = 0;
    if (ok) {
        task_id = scheduler_alloc_thread_task();
        if (task_id == NO_TASK) {
            ok = false;
            failed_phase = "task-alloc";
        }
    }

    if (ok) {
        req = omni_uv_work_req_new();
        if (req == null) {
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_phase = "req-alloc";
        }
    }

    if (ok) {
        OffloadWork work = {};
        work.fiber_id = NO_FIBER;
        work.task_id = task_id;
        work.op = OFFLOAD_SLEEP_MS;
        work.int_arg = 5;
        work.shared = scheduler_publish_shared("prestart-cancel");
        if (work.shared == null) {
            omni_uv_work_req_free(req);
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_phase = "shared-publish";
        } else if (!scheduler_bind_thread_task_request(task_id, req, &generation) || generation == 0) {
            scheduler_release_shared(work.shared);
            omni_uv_work_req_free(req);
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_phase = "bind-request";
        } else {
            shared_slot = work.shared.slot;
            shared_generation = work.shared.generation;
            QueuedOffloadWork* queued = (QueuedOffloadWork*)mem::malloc(QueuedOffloadWork.sizeof);
            if (queued == null) {
                scheduler_release_shared(work.shared);
                scheduler_clear_thread_task_request(task_id, req, generation);
                omni_uv_work_req_free(req);
                scheduler_drop_thread_task(task_id, generation);
                ok = false;
                failed_phase = "queued-alloc";
            } else {
                *queued = {};
                queued.work = work;
                queued.task_generation = generation;
                queued.executed = false;
                queued.completion = null;
                omni_uv_req_set_data(req, queued);

                bool cancel_ok = scheduler_cancel_thread_task(task_id, null, generation);
                if (!cancel_ok) {
                    scheduler_release_shared(work.shared);
                    mem::free(queued);
                    scheduler_clear_thread_task_request(task_id, req, generation);
                    omni_uv_work_req_free(req);
                    scheduler_drop_thread_task(task_id, generation);
                    ok = false;
                    failed_phase = "cancel";
                } else {
                    scheduler_uv_offload_work_cb(req);
                    scheduler_uv_offload_after_cb(req, -1);
                }
            }
        }
    }

    if (ok) {
        OffloadCompletion* completion = scheduler_take_thread_task_completion(task_id, generation);
        bool completion_ok = completion != null &&
                             completion.kind == OFFLOAD_RES_ERROR &&
                             !scheduler_thread_task_exists(task_id, generation);
        if (completion != null) scheduler_free_offload_completion(completion);

        SharedHandle probe = {};
        probe.kind = SHARED_HANDLE_BLOB;
        probe.slot = shared_slot;
        probe.generation = shared_generation;
        probe.payload = null;
        bool shared_released = scheduler_shared_registry_resolve_blob(&probe) == null;

        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        ok = completion_ok && shared_released && state_ok;
        if (!ok) {
            if (!completion_ok) {
                failed_phase = "completion";
            } else if (!shared_released) {
                failed_phase = "shared-release";
            } else {
                failed_phase = "boundary";
            }
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler prestart-cancel shared release remains single-owner");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler prestart-cancel shared release remains single-owner (%s)",
            (ZString)failed_phase
        );
        (*fail)++;
    }
}
