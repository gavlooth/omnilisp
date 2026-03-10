module main;

import std::core::mem;
import std::io;
import lisp;

fn void scope_region_run_is_in_scope_tests(usz* passed, usz* failed) {
    // Test 14: is_in_scope
    ScopeRegion* s = scope_create(null);
    void* p1 = s.alloc(24);
    void* p2 = s.alloc(24);
    ScopeRegion* other = scope_create(null);
    void* p3 = other.alloc(24);

    if (is_in_scope(p1, s)) { (*passed)++; } else { io::printn("FAIL: is_in_scope p1 in s"); (*failed)++; }
    if (is_in_scope(p2, s)) { (*passed)++; } else { io::printn("FAIL: is_in_scope p2 in s"); (*failed)++; }
    if (!is_in_scope(p3, s)) { (*passed)++; } else { io::printn("FAIL: is_in_scope p3 not in s"); (*failed)++; }
    if (is_in_scope(p3, other)) { (*passed)++; } else { io::printn("FAIL: is_in_scope p3 in other"); (*failed)++; }
    if (!is_in_scope(null, s)) { (*passed)++; } else { io::printn("FAIL: is_in_scope null"); (*failed)++; }

    scope_release(other);
    scope_release(s);
}

fn void scope_region_run_splice_escape_tests(usz* passed, usz* failed) {
    // Test 15: scope_splice_escapes
    ScopeRegion* parent = scope_create(null);
    ScopeRegion* child = scope_create(parent);

    void* escape_ptr = child.alloc_escape(24);
    if (escape_ptr != null) { (*passed)++; } else { io::printn("FAIL: splice escape alloc"); (*failed)++; }

    int* temp_counter = (int*)mem::malloc(int.sizeof);
    int* escape_counter = (int*)mem::malloc(int.sizeof);
    *temp_counter = 0;
    *escape_counter = 0;

    scope_register_dtor(child, (void*)temp_counter, fn void(void* ptr) {
        int* c = (int*)ptr;
        *c = *c + 1;
    });
    scope_register_dtor_escape(child, (void*)escape_counter, fn void(void* ptr) {
        int* c = (int*)ptr;
        *c = *c + 1;
    });

    lisp::BoundaryScopeTransferResult ready = lisp::boundary_check_scope_transfer(parent, child);
    if (ready.allowed && ready.reason == lisp::BoundaryScopeTransferReason.BOUNDARY_SCOPE_TRANSFER_OK) {
        (*passed)++;
    } else {
        io::printfn("FAIL: splice ready check reason=%s", lisp::boundary_scope_transfer_reason_name(ready.reason));
        (*failed)++;
    }

    scope_splice_escapes(parent, child);
    if (*temp_counter == 1) { (*passed)++; } else { io::printfn("FAIL: splice temp dtor=%d", *temp_counter); (*failed)++; }
    if (*escape_counter == 0) { (*passed)++; } else { io::printfn("FAIL: splice escape dtor early=%d", *escape_counter); (*failed)++; }
    if (is_in_scope(escape_ptr, parent)) { (*passed)++; } else { io::printn("FAIL: splice escape ptr in parent"); (*failed)++; }

    scope_release(parent);
    if (*escape_counter == 1) { (*passed)++; } else { io::printfn("FAIL: splice escape dtor late=%d", *escape_counter); (*failed)++; }

    mem::free(temp_counter);
    mem::free(escape_counter);

    // Test 15a: multiple splice operations preserve order, ownership and tail validity.
    {
        ScopeRegion* multi_parent = scope_create(null);
        ScopeRegion* multi_child_a = scope_create(multi_parent);
        ScopeRegion* multi_child_b = scope_create(multi_parent);

        int* order_state = (int*)mem::malloc(int.sizeof);
        *order_state = 0;

        void* multi_parent_escape = multi_parent.alloc_escape(16);
        void* multi_child_a_escape = multi_child_a.alloc_escape(16);
        void* multi_child_b_escape = multi_child_b.alloc_escape(16);
        if (multi_parent_escape == null || multi_child_a_escape == null || multi_child_b_escape == null) {
            io::printn("FAIL: splice order setup alloc");
            (*failed)++;
            if (multi_child_b != null) scope_release(multi_child_b);
            if (multi_child_a != null) scope_release(multi_child_a);
            if (multi_parent != null) scope_release(multi_parent);
            mem::free(order_state);
            return;
        }

        scope_register_dtor_escape(multi_parent, (void*)order_state, fn void(void* ptr) {
            int* s = (int*)ptr;
            *s = (*s * 10) + 1;
        });
        scope_register_dtor_escape(multi_child_a, (void*)order_state, fn void(void* ptr) {
            int* s = (int*)ptr;
            *s = (*s * 10) + 2;
        });
        scope_register_dtor_escape(multi_child_b, (void*)order_state, fn void(void* ptr) {
            int* s = (int*)ptr;
            *s = (*s * 10) + 3;
        });

        scope_splice_escapes(multi_parent, multi_child_a);
        bool after_first_ok = scope_escape_chunk_tail_consistent(multi_parent) &&
                              scope_escape_dtor_tail_consistent(multi_parent);

        scope_splice_escapes(multi_parent, multi_child_b);
        bool after_second_ok = scope_escape_chunk_tail_consistent(multi_parent) &&
                               scope_escape_dtor_tail_consistent(multi_parent) &&
                               is_in_scope(multi_child_a_escape, multi_parent) &&
                               is_in_scope(multi_child_b_escape, multi_parent) &&
                               multi_parent.escape_chunks_tail != null &&
                               multi_parent.escape_dtors_tail != null &&
                               multi_parent.escape_chunks_tail.next == null &&
                               multi_parent.escape_dtors_tail.next == null;

        scope_release(multi_parent);
        bool order_ok = *order_state == 321;
        if (after_first_ok && after_second_ok && order_ok) {
            (*passed)++;
        } else {
            io::printfn(
                "FAIL: splice order/tail first=%s second=%s order=%d",
                after_first_ok ? "yes" : "no",
                after_second_ok ? "yes" : "no",
                *order_state
            );
            (*failed)++;
        }
        mem::free(order_state);
    }

    // Test 15a2: empty/non-empty concat combinations keep head/tail valid.
    {
        int* combo_counter = (int*)mem::malloc(int.sizeof);
        *combo_counter = 0;
        bool combo_ok = true;

        // Case A: parent empty + child non-empty.
        ScopeRegion* parent_a = scope_create(null);
        ScopeRegion* child_a = scope_create(parent_a);
        void* child_a_escape = child_a.alloc_escape(16);
        scope_register_dtor_escape(child_a, (void*)combo_counter, fn void(void* ptr) {
            int* c = (int*)ptr;
            *c = *c + 1;
        });
        scope_splice_escapes(parent_a, child_a);
        combo_ok = combo_ok &&
                   child_a_escape != null &&
                   is_in_scope(child_a_escape, parent_a) &&
                   parent_a.escape_chunks != null &&
                   parent_a.escape_chunks_tail != null &&
                   parent_a.escape_bump != null &&
                   parent_a.escape_limit != null &&
                   parent_a.escape_bump <= parent_a.escape_limit &&
                   parent_a.escape_chunks_tail.next == null &&
                   parent_a.escape_dtors != null &&
                   parent_a.escape_dtors_tail != null &&
                   parent_a.escape_dtors_tail.next == null;
        scope_release(parent_a);
        combo_ok = combo_ok && *combo_counter == 1;

        // Case B: parent non-empty + child empty.
        ScopeRegion* parent_b = scope_create(null);
        ScopeRegion* child_b = scope_create(parent_b);
        parent_b.alloc_escape(16);
        scope_register_dtor_escape(parent_b, (void*)combo_counter, fn void(void* ptr) {
            int* c = (int*)ptr;
            *c = *c + 1;
        });
        ScopeChunk* old_chunks_head = parent_b.escape_chunks;
        ScopeChunk* old_chunks_tail = parent_b.escape_chunks_tail;
        ScopeDtor* old_dtors_head = parent_b.escape_dtors;
        ScopeDtor* old_dtors_tail = parent_b.escape_dtors_tail;
        scope_splice_escapes(parent_b, child_b);
        combo_ok = combo_ok &&
                   parent_b.escape_chunks == old_chunks_head &&
                   parent_b.escape_chunks_tail == old_chunks_tail &&
                   parent_b.escape_dtors == old_dtors_head &&
                   parent_b.escape_dtors_tail == old_dtors_tail &&
                   scope_escape_chunk_tail_consistent(parent_b) &&
                   scope_escape_dtor_tail_consistent(parent_b);
        scope_release(parent_b);
        combo_ok = combo_ok && *combo_counter == 2;

        // Case C: parent empty + child empty.
        ScopeRegion* parent_c = scope_create(null);
        ScopeRegion* child_c = scope_create(parent_c);
        scope_splice_escapes(parent_c, child_c);
        combo_ok = combo_ok &&
                   parent_c.escape_chunks == null &&
                   parent_c.escape_chunks_tail == null &&
                   parent_c.escape_dtors == null &&
                   parent_c.escape_dtors_tail == null &&
                   scope_escape_chunk_tail_consistent(parent_c) &&
                   scope_escape_dtor_tail_consistent(parent_c);
        scope_release(parent_c);

        if (combo_ok) {
            (*passed)++;
        } else {
            io::printn("FAIL: splice empty/non-empty concat invariants");
            (*failed)++;
        }
        mem::free(combo_counter);
    }

    // Test 15b: explicit legality reasons reject sibling/foreign/refcount>1 transfers.
    ScopeRegion* reject_parent = scope_create(null);
    ScopeRegion* reject_child = scope_create(reject_parent);
    ScopeRegion* reject_sibling = scope_create(reject_parent);
    ScopeRegion* reject_foreign = scope_create(null);
    if (reject_parent == null || reject_child == null || reject_sibling == null || reject_foreign == null) {
        if (reject_foreign != null) scope_release(reject_foreign);
        if (reject_sibling != null) scope_release(reject_sibling);
        if (reject_child != null) scope_release(reject_child);
        if (reject_parent != null) scope_release(reject_parent);
        io::printn("FAIL: splice rejection setup");
        (*failed)++;
        return;
    }

    void* reject_escape = reject_child.alloc_escape(16);
    lisp::BoundaryScopeTransferResult sibling_result =
        lisp::boundary_try_scope_splice_escapes_result(reject_sibling, reject_child);
    lisp::BoundaryScopeTransferResult foreign_result =
        lisp::boundary_try_scope_splice_escapes_result(reject_foreign, reject_child);

    void* saved_child_owner = reject_child.owner_thread_token;
    reject_child.owner_thread_token = null;
    lisp::BoundaryScopeTransferResult owner_result =
        lisp::boundary_try_scope_splice_escapes_result(reject_parent, reject_child);
    reject_child.owner_thread_token = saved_child_owner;

    scope_retain(reject_child);
    lisp::BoundaryScopeTransferResult retained_result =
        lisp::boundary_try_scope_splice_escapes_result(reject_parent, reject_child);

    bool sibling_ok = !sibling_result.allowed &&
                      sibling_result.reason == lisp::BoundaryScopeTransferReason.BOUNDARY_SCOPE_TRANSFER_NOT_IMMEDIATE_CHILD;
    bool foreign_ok = !foreign_result.allowed &&
                      foreign_result.reason == lisp::BoundaryScopeTransferReason.BOUNDARY_SCOPE_TRANSFER_NOT_IMMEDIATE_CHILD;
    bool owner_ok = !owner_result.allowed &&
                    owner_result.reason == lisp::BoundaryScopeTransferReason.BOUNDARY_SCOPE_TRANSFER_OWNER_THREAD_MISMATCH;
    bool retained_ok = !retained_result.allowed &&
                       retained_result.reason == lisp::BoundaryScopeTransferReason.BOUNDARY_SCOPE_TRANSFER_CHILD_REFCOUNT_NOT_ONE;
    bool state_ok = reject_escape != null &&
                    reject_child.parent == reject_parent &&
                    reject_child.refcount == 2 &&
                    is_in_scope(reject_escape, reject_child) &&
                    !is_in_scope(reject_escape, reject_parent) &&
                    !is_in_scope(reject_escape, reject_sibling) &&
                    !is_in_scope(reject_escape, reject_foreign);

    scope_release(reject_child); // Drop extra retain before teardown.
    scope_release(reject_sibling);
    scope_release(reject_child);
    scope_release(reject_foreign);
    scope_release(reject_parent);

    if (sibling_ok && foreign_ok && owner_ok && retained_ok && state_ok) {
        (*passed)++;
    } else {
        io::printfn(
            "FAIL: splice rejection reasons sibling=%s foreign=%s owner=%s retained=%s state=%s (%s/%s/%s/%s)",
            sibling_ok ? "yes" : "no",
            foreign_ok ? "yes" : "no",
            owner_ok ? "yes" : "no",
            retained_ok ? "yes" : "no",
            state_ok ? "yes" : "no",
            lisp::boundary_scope_transfer_reason_name(sibling_result.reason),
            lisp::boundary_scope_transfer_reason_name(foreign_result.reason),
            lisp::boundary_scope_transfer_reason_name(owner_result.reason),
            lisp::boundary_scope_transfer_reason_name(retained_result.reason)
        );
        (*failed)++;
    }
}
