module lisp;

int g_scheduler_force_transition_completion_alloc_fail_nth = 0;

fn OffloadCompletion* scheduler_make_transition_error_completion(char[] error_message) {
    if (g_scheduler_force_transition_completion_alloc_fail_nth > 0) {
        g_scheduler_force_transition_completion_alloc_fail_nth--;
        if (g_scheduler_force_transition_completion_alloc_fail_nth == 0) return null;
    }
    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) return null;
    *completion = {};
    scheduler_set_offload_error(completion, error_message);
    return completion;
}

fn void scheduler_prepare_cancel_completion_noalloc(OffloadCompletion* completion, char[] cancel_message) @inline {
    if (completion == null) return;
    if (completion.shared != null) {
        scheduler_release_shared(completion.shared);
        completion.shared = null;
    }
    if (completion.error_msg != null && completion.error_len >= cancel_message.len) {
        for (usz i = 0; i < cancel_message.len; i++) {
            completion.error_msg[i] = cancel_message[i];
        }
        completion.error_len = cancel_message.len;
        completion.kind = OFFLOAD_RES_ERROR;
        return;
    }
    if (completion.error_msg != null) mem::free(completion.error_msg);
    completion.error_msg = null;
    completion.error_len = 0;
    completion.kind = OFFLOAD_RES_ERROR;
    scheduler_set_offload_error(completion, cancel_message);
}

fn void scheduler_transition_take_waiter(
    usz* waiter_fiber_id_out,
    uint* waiter_wake_epoch_out,
    usz* waiter_fiber_id_slot,
    uint* waiter_wake_epoch_slot
) @inline {
    if (waiter_fiber_id_out != null) *waiter_fiber_id_out = NO_FIBER;
    if (waiter_wake_epoch_out != null) *waiter_wake_epoch_out = 0;
    if (waiter_fiber_id_slot == null || waiter_wake_epoch_slot == null) return;
    if (waiter_fiber_id_out != null) *waiter_fiber_id_out = *waiter_fiber_id_slot;
    if (waiter_wake_epoch_out != null) *waiter_wake_epoch_out = *waiter_wake_epoch_slot;
    *waiter_fiber_id_slot = NO_FIBER;
    *waiter_wake_epoch_slot = 0;
}

fn bool scheduler_transition_materialize_cancel_completion(
    OffloadCompletion** slot_completion,
    OffloadCompletion** incoming_completion,
    char[] cancel_message
) {
    if (slot_completion == null) return false;
    if (*slot_completion == null) {
        if (incoming_completion != null && *incoming_completion != null) {
            scheduler_prepare_cancel_completion_noalloc(*incoming_completion, cancel_message);
            *slot_completion = *incoming_completion;
            *incoming_completion = null;
        } else {
            *slot_completion = scheduler_make_transition_error_completion(cancel_message);
        }
        return *slot_completion != null;
    }

    if (incoming_completion != null && *incoming_completion != null) {
        scheduler_free_offload_completion(*incoming_completion);
        *incoming_completion = null;
    }
    return true;
}

fn void scheduler_transition_handoff_completion(
    OffloadCompletion** slot_completion,
    OffloadCompletion* completion,
    usz* waiter_fiber_id_slot,
    uint* waiter_wake_epoch_slot,
    usz* waiter_fiber_id_out,
    uint* waiter_wake_epoch_out
) {
    if (slot_completion != null && *slot_completion != null) {
        scheduler_free_offload_completion(*slot_completion);
    }
    if (slot_completion != null) *slot_completion = completion;
    scheduler_transition_take_waiter(
        waiter_fiber_id_out,
        waiter_wake_epoch_out,
        waiter_fiber_id_slot,
        waiter_wake_epoch_slot
    );
}

fn void scheduler_publish_task_cancel_locked(usz waiter_fiber_id, uint waiter_wake_epoch) @inline {
    scheduler_signal_thread_task_waiters_locked();
    (void)scheduler_signal_task_waiter_fiber(waiter_fiber_id, waiter_wake_epoch, true);
}

fn void scheduler_publish_os_thread_cancel_locked(usz waiter_fiber_id, uint waiter_wake_epoch) @inline {
    scheduler_signal_thread_task_waiters_locked();
    (void)scheduler_signal_os_thread_waiter_fiber(waiter_fiber_id, waiter_wake_epoch, true);
}

fn void scheduler_publish_task_cancel_unlocked(usz waiter_fiber_id, uint waiter_wake_epoch) @inline {
    scheduler_signal_thread_task_waiters();
    (void)scheduler_signal_task_waiter_fiber(waiter_fiber_id, waiter_wake_epoch);
}

fn void scheduler_publish_os_thread_cancel_unlocked(usz waiter_fiber_id, uint waiter_wake_epoch) @inline {
    scheduler_signal_thread_task_waiters();
    (void)scheduler_signal_os_thread_waiter_fiber(waiter_fiber_id, waiter_wake_epoch);
}

fn void scheduler_publish_task_completion_unlocked(usz waiter_fiber_id, uint waiter_wake_epoch) @inline {
    scheduler_signal_thread_task_waiters();
    (void)scheduler_signal_task_waiter_fiber(waiter_fiber_id, waiter_wake_epoch);
}

fn void scheduler_publish_os_thread_completion_unlocked(usz waiter_fiber_id, uint waiter_wake_epoch) @inline {
    scheduler_signal_thread_task_waiters();
    (void)scheduler_signal_os_thread_waiter_fiber(waiter_fiber_id, waiter_wake_epoch);
}
