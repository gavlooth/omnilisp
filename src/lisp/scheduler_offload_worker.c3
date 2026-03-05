module lisp;

fn bool scheduler_offload_compress(bool gzip_mode, SharedBlob* input, OffloadCompletion* completion) {
    if (input == null || input.bytes == null) {
        scheduler_set_offload_error(completion, "offload: missing input");
        return false;
    }

    DeflateCompressor* compressor = libdeflate_alloc_compressor(6);
    if (compressor == null) {
        scheduler_set_offload_error(completion, "offload: compressor allocation failed");
        return false;
    }
    defer libdeflate_free_compressor(compressor);

    usz input_len = input.len;
    usz bound = gzip_mode
        ? libdeflate_gzip_compress_bound(compressor, input_len)
        : libdeflate_deflate_compress_bound(compressor, input_len);
    char* out = (char*)mem::malloc(bound + 1);
    if (out == null) {
        scheduler_set_offload_error(completion, "offload: out of memory");
        return false;
    }

    usz written = gzip_mode
        ? libdeflate_gzip_compress(compressor, input.bytes, input_len, out, bound)
        : libdeflate_deflate_compress(compressor, input.bytes, input_len, out, bound);
    if (written == 0) {
        mem::free(out);
        scheduler_set_offload_error(completion, "offload: compression failed");
        return false;
    }
    out[written] = 0;

    SharedBlob* result = shared_blob_new_owned(out, written);
    if (result == null) {
        scheduler_set_offload_error(completion, "offload: result allocation failed");
        return false;
    }

    completion.kind = OFFLOAD_RES_BYTES;
    completion.blob = result;
    return true;
}

fn bool scheduler_offload_accept_fd(long raw_fd, OffloadCompletion* completion) {
    if (raw_fd < 0) {
        scheduler_set_offload_error(completion, "offload accept-fd: invalid listener fd");
        return false;
    }
    int accepted_fd = tcp_accept_fd((int)raw_fd);
    if (accepted_fd < 0) {
        scheduler_set_offload_error(completion, "offload accept-fd: accept failed");
        return false;
    }
    completion.kind = OFFLOAD_RES_INT;
    completion.int_result = accepted_fd;
    return true;
}

fn OffloadCompletion* scheduler_execute_offload(OffloadWork work) {
    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) {
        shared_blob_release(work.blob);
        return null;
    }
    *completion = {};

    switch (work.op) {
        case OFFLOAD_SLEEP_MS:
            if (work.int_arg > 0) c_usleep((uint)(work.int_arg * 1000));
            completion.kind = OFFLOAD_RES_NIL;
        case OFFLOAD_GZIP:
            scheduler_offload_compress(true, work.blob, completion);
        case OFFLOAD_DEFLATE:
            scheduler_offload_compress(false, work.blob, completion);
        case OFFLOAD_ACCEPT_FD:
            scheduler_offload_accept_fd(work.int_arg, completion);
    }

    shared_blob_release(work.blob);
    return completion;
}

fn int scheduler_offload_worker_main(void* arg) {
    (void)arg;
    while (true) {
        OffloadWork work = {};

        g_scheduler.offload_queue.mu.lock();
        while (g_scheduler.offload_queue.count == 0 && g_scheduler.offload_queue.running) {
            g_scheduler.offload_queue.cv.wait(&g_scheduler.offload_queue.mu);
        }
        if (!g_scheduler.offload_queue.running) {
            g_scheduler.offload_queue.mu.unlock();
            return 0;
        }

        usz slot = g_scheduler.offload_queue.tail & OFFLOAD_QUEUE_MASK;
        work = g_scheduler.offload_queue.ring[slot];
        g_scheduler.offload_queue.tail++;
        g_scheduler.offload_queue.count--;
        g_scheduler.offload_queue.mu.unlock();

        if (work.task_id != NO_TASK) {
            if (!scheduler_try_begin_thread_task(work.task_id)) {
                shared_blob_release(work.blob);
                continue;
            }
        }

        OffloadCompletion* completion = scheduler_execute_offload(work);
        if (completion == null) {
            continue;
        }

        if (work.task_id != NO_TASK) {
            scheduler_complete_thread_task(work.task_id, completion);
            continue;
        }

        while (!wakeup_enqueue(WAKEUP_OFFLOAD_READY, work.fiber_id, 0, completion)) {
            c_usleep(1000);
        }
    }
}

fn void scheduler_init_offload_queue() {
    OffloadQueue* q = &g_scheduler.offload_queue;
    if (q.initialized) return;

    *q = {};
    if (catch err = q.mu.init()) return;
    if (catch err = q.cv.init()) {
        q.mu.destroy();
        return;
    }
    q.running = true;

    if (catch err = q.worker.create(&scheduler_offload_worker_main, null)) {
        q.cv.destroy();
        q.mu.destroy();
        *q = {};
        return;
    }

    q.initialized = true;
}

fn bool scheduler_enqueue_offload(OffloadWork work) {
    OffloadQueue* q = &g_scheduler.offload_queue;
    if (!q.initialized) return false;

    q.mu.lock();
    defer q.mu.unlock();

    if (q.count >= OFFLOAD_QUEUE_SIZE) return false;
    usz slot = q.head & OFFLOAD_QUEUE_MASK;
    q.ring[slot] = work;
    q.head++;
    q.count++;
    q.cv.signal();
    return true;
}
