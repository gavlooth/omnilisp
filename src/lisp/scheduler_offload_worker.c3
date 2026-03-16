module lisp;

struct QueuedOffloadWork {
    OffloadWork work;
    uint task_generation;
    OffloadCompletion* completion;
    bool executed;
    QueuedOffloadWork* next_free;
}

fn OffloadCompletion* scheduler_make_offload_alloc_failure_completion() {
    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) return null;
    *completion = {};
    scheduler_set_offload_error(completion, "offload: worker completion alloc failed");
    return completion;
}

fn QueuedOffloadWork* scheduler_offload_take_queued_work() {
    OffloadAdmission* admission = &g_scheduler.offload_admission;
    if (!admission.initialized) return null;

    QueuedOffloadWork* queued = null;
    admission.mu.lock();
    if (admission.queued_free_head != null) {
        queued = (QueuedOffloadWork*)admission.queued_free_head;
        admission.queued_free_head = queued.next_free;
        if (admission.queued_free_count > 0) admission.queued_free_count--;
    }
    admission.mu.unlock();

    if (queued != null) {
        *queued = {};
        return queued;
    }
    return (QueuedOffloadWork*)mem::malloc(QueuedOffloadWork.sizeof);
}

fn void scheduler_offload_recycle_queued_work(QueuedOffloadWork* queued) {
    if (queued == null) return;

    OffloadAdmission* admission = &g_scheduler.offload_admission;
    if (!admission.initialized) {
        mem::free(queued);
        return;
    }

    bool recycled = false;
    admission.mu.lock();
    if (admission.queued_free_count < admission.max_in_flight) {
        queued.next_free = (QueuedOffloadWork*)admission.queued_free_head;
        admission.queued_free_head = queued;
        admission.queued_free_count++;
        recycled = true;
    }
    admission.mu.unlock();

    if (!recycled) mem::free(queued);
}

fn void scheduler_offload_admission_release_slot() {
    OffloadAdmission* admission = &g_scheduler.offload_admission;
    if (!admission.initialized) return;
    admission.mu.lock();
    if (admission.in_flight > 0) admission.in_flight--;
    admission.mu.unlock();
}

fn void scheduler_uv_offload_work_cb(void* req) {
    QueuedOffloadWork* queued = (QueuedOffloadWork*)omni_uv_req_get_data(req);
    if (queued == null) return;

    OffloadWork work = queued.work;
    if (work.task_id != NO_TASK) {
        if (!scheduler_try_begin_thread_task(work.task_id, queued.task_generation)) {
            return;
        }
    }

    queued.executed = true;
    queued.completion = scheduler_execute_offload(work);
}

fn void scheduler_uv_offload_after_cb(void* req, int status) {
    (void)status;
    scheduler_offload_admission_release_slot();

    QueuedOffloadWork* queued = (QueuedOffloadWork*)omni_uv_req_get_data(req);
    if (queued == null) {
        omni_uv_work_req_free(req);
        return;
    }

    OffloadWork work = queued.work;
    OffloadCompletion* completion = queued.completion;

    if (work.task_id != NO_TASK) {
        scheduler_clear_thread_task_request(work.task_id, req, queued.task_generation);
        if (queued.executed) {
            if (completion == null) {
                completion = scheduler_make_offload_alloc_failure_completion();
            }
            if (completion != null) {
                scheduler_complete_thread_task(work.task_id, completion, queued.task_generation);
            } else {
                // OOM hardening: avoid stranding RUNNING tasks if completion materialization fails.
                scheduler_drop_thread_task(work.task_id, queued.task_generation);
            }
        } else {
            // Cancellation before worker execution must still release queued payloads.
            // The task transition path owns completion signaling for pending entries.
            if (work.shared != null) {
                scheduler_release_shared(work.shared);
                work.shared = null;
                queued.work.shared = null;
            }
            if (completion != null) {
                scheduler_free_offload_completion(completion);
                completion = null;
            }
            bool already_done = false;
            (void)scheduler_cancel_thread_task(work.task_id, &already_done, queued.task_generation);
        }
    } else {
        if (completion == null) {
            completion = scheduler_make_offload_alloc_failure_completion();
        }
        if (completion != null && !scheduler_publish_offload_ready(work.fiber_id, completion, work.wake_epoch)) {
            scheduler_free_offload_completion(completion);
        }
    }

    queued.work.shared = null;
    queued.completion = null;
    queued.next_free = null;
    scheduler_offload_recycle_queued_work(queued);
    omni_uv_work_req_free(req);
}

fn void scheduler_init_offload_admission() {
    OffloadAdmission* admission = &g_scheduler.offload_admission;
    if (admission.initialized) return;

    *admission = {};
    if (catch err = admission.mu.init()) return;
    admission.in_flight = 0;
    admission.max_in_flight = OFFLOAD_ADMISSION_MAX_IN_FLIGHT;
    admission.queued_free_head = null;
    admission.queued_free_count = 0;
    admission.initialized = true;
}

fn bool scheduler_enqueue_offload(OffloadWork work) {
    OffloadAdmission* admission = &g_scheduler.offload_admission;
    if (!admission.initialized || !g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) return false;

    void* req = omni_uv_work_req_new();
    if (req == null) return false;
    QueuedOffloadWork* queued = scheduler_offload_take_queued_work();
    if (queued == null) {
        omni_uv_work_req_free(req);
        return false;
    }
    *queued = {};
    queued.work = work;
    queued.task_generation = 0;

    bool admitted = false;
    admission.mu.lock();
    if (admission.in_flight < admission.max_in_flight) {
        admission.in_flight++;
        admitted = true;
    }
    admission.mu.unlock();
    if (!admitted) {
        scheduler_offload_recycle_queued_work(queued);
        omni_uv_work_req_free(req);
        return false;
    }

    if (work.task_id != NO_TASK &&
        !scheduler_bind_thread_task_request(work.task_id, req, &queued.task_generation)) {
        scheduler_offload_admission_release_slot();
        scheduler_offload_recycle_queued_work(queued);
        omni_uv_work_req_free(req);
        return false;
    }

    omni_uv_req_set_data(req, queued);
    int status = omni_uv_queue_work_start(
        g_scheduler.uv_loop,
        req,
        &scheduler_uv_offload_work_cb,
        &scheduler_uv_offload_after_cb
    );
    if (status != 0) {
        if (work.task_id != NO_TASK) {
            scheduler_clear_thread_task_request(work.task_id, req, queued.task_generation);
        }
        scheduler_offload_admission_release_slot();
        scheduler_offload_recycle_queued_work(queued);
        omni_uv_work_req_free(req);
        return false;
    }
    return true;
}
