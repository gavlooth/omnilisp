module lisp;

fn long scheduler_publish_sendable_int(long value) @inline {
    return value;
}

fn long scheduler_domain_publish_sendable_int(SchedulerDomain* domain, long value) @inline {
    if (!scheduler_domain_is_local(domain)) return value;
    scheduler_domain_safe_point(domain);
    return scheduler_publish_sendable_int(value);
}

fn Value* scheduler_consume_sendable_int(long value, Interp* interp) @inline {
    return make_int(interp, value);
}

fn Value* scheduler_domain_consume_sendable_int(SchedulerDomain* domain, long value, Interp* interp) @inline {
    if (!scheduler_domain_is_local(domain)) {
        return scheduler_raise(interp, "scheduler/domain-invalid-sendable-consume", "scheduler: invalid domain sendable consume");
    }
    scheduler_domain_safe_point(domain);
    return scheduler_consume_sendable_int(value, interp);
}

fn Value* scheduler_project_shared_to_local_value(SharedHandle** handle_ref, Interp* interp) {
    if (handle_ref == null || *handle_ref == null) {
        return make_string(interp, "");
    }

    SharedHandle* handle = *handle_ref;
    switch (handle.kind) {
        case SHARED_HANDLE_BLOB:
            char* owned = null;
            usz len = 0;
            if (!scheduler_take_shared_bytes_owned(handle, &owned, &len)) {
                scheduler_release_shared(handle);
                *handle_ref = null;
                return make_string(interp, "");
            }

            scheduler_release_shared(handle);
            *handle_ref = null;
            return make_string_owned(interp, owned, len);
    }
}

fn Value* scheduler_domain_project_shared_to_local_value(SchedulerDomain* domain, SharedHandle** handle_ref, Interp* interp) {
    if (!scheduler_domain_is_local(domain)) {
        return scheduler_raise(interp, "scheduler/domain-invalid-shared-project", "scheduler: invalid domain shared projection");
    }
    scheduler_domain_safe_point(domain);
    return scheduler_project_shared_to_local_value(handle_ref, interp);
}
