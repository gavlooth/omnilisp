module lisp;

import std::core::mem;

fn bool deduce_db_ensure_rule_signature_capacity(DeduceDb* db, usz needed) {
    if (db == null) return false;
    if (needed <= db.rule_signature_capacity) return true;

    usz new_cap = db.rule_signature_capacity > 0 ? db.rule_signature_capacity : 8;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }
    if (new_cap > usz.max / DeduceRuleSignature.sizeof) return false;

    DeduceRuleSignature* next =
        (DeduceRuleSignature*)mem::realloc(db.rule_signatures, DeduceRuleSignature.sizeof * new_cap);
    if (next == null) return false;
    db.rule_signatures = next;
    db.rule_signature_capacity = new_cap;
    return true;
}

fn void deduce_rule_signature_release_allocations(DeduceRuleSignature* sig) {
    if (sig == null) return;
    if (sig.body_predicates != null) {
        mem::free(sig.body_predicates);
        sig.body_predicates = null;
    }
    if (sig.body_negated != null) {
        mem::free(sig.body_negated);
        sig.body_negated = null;
    }
    if (sig.body_bound_masks != null) {
        mem::free(sig.body_bound_masks);
        sig.body_bound_masks = null;
    }
    if (sig.body_selected_indexes != null) {
        mem::free(sig.body_selected_indexes);
        sig.body_selected_indexes = null;
    }
    if (sig.body_filter_pushdown != null) {
        mem::free(sig.body_filter_pushdown);
        sig.body_filter_pushdown = null;
    }
    if (sig.body_projection_masks != null) {
        mem::free(sig.body_projection_masks);
        sig.body_projection_masks = null;
    }
    if (sig.head_term_kinds != null) {
        mem::free(sig.head_term_kinds);
        sig.head_term_kinds = null;
    }
    if (sig.head_term_aggregate_ops != null) {
        mem::free(sig.head_term_aggregate_ops);
        sig.head_term_aggregate_ops = null;
    }
    if (sig.head_term_is_variable != null) {
        mem::free(sig.head_term_is_variable);
        sig.head_term_is_variable = null;
    }
    if (sig.head_term_var_ids != null) {
        mem::free(sig.head_term_var_ids);
        sig.head_term_var_ids = null;
    }
    if (sig.head_term_literals != null) {
        mem::free(sig.head_term_literals);
        sig.head_term_literals = null;
    }
    if (sig.body_term_offsets != null) {
        mem::free(sig.body_term_offsets);
        sig.body_term_offsets = null;
    }
    if (sig.body_term_counts != null) {
        mem::free(sig.body_term_counts);
        sig.body_term_counts = null;
    }
    if (sig.body_term_is_variable != null) {
        mem::free(sig.body_term_is_variable);
        sig.body_term_is_variable = null;
    }
    if (sig.body_term_var_ids != null) {
        mem::free(sig.body_term_var_ids);
        sig.body_term_var_ids = null;
    }
    if (sig.body_term_literals != null) {
        mem::free(sig.body_term_literals);
        sig.body_term_literals = null;
    }
    sig.head_term_count = 0;
    sig.aggregate_projection_count = 0;
    sig.body_term_total_count = 0;
    sig.max_var_id = 0;
}
