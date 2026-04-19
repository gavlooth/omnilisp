module lisp;

import std::core::mem;

// Keep common nested equality cases on-stack first to avoid avoidable heap churn.
const usz EQUAL_STACK_INLINE_CAPACITY = 1024;
const usz EQUAL_SEEN_INLINE_CAPACITY = 1024;

struct EqualityWorkspaceStats {
    usz stack_heap_growths;
    usz seen_heap_growths;
    usz stack_heap_bytes;
    usz seen_heap_bytes;
}

struct EqualPair {
    Value* left;
    Value* right;
}

fn void equality_workspace_reset_stats(EqualityWorkspaceStats* workspace_stats) {
    if (workspace_stats == null) return;
    workspace_stats.stack_heap_growths = 0;
    workspace_stats.seen_heap_growths = 0;
    workspace_stats.stack_heap_bytes = 0;
    workspace_stats.seen_heap_bytes = 0;
}

fn bool equality_time_point_equal(Value* left, Value* right) {
    if (left.time_point_val.year != right.time_point_val.year) return false;
    if (left.time_point_val.month != right.time_point_val.month) return false;
    if (left.time_point_val.day != right.time_point_val.day) return false;
    if (left.time_point_val.hour != right.time_point_val.hour) return false;
    if (left.time_point_val.minute != right.time_point_val.minute) return false;
    if (left.time_point_val.second != right.time_point_val.second) return false;
    if (left.time_point_val.microsecond != right.time_point_val.microsecond) return false;
    if (left.time_point_val.tz_offset != right.time_point_val.tz_offset) return false;
    return left.time_point_val.kind == right.time_point_val.kind;
}

fn bool equality_pair_seen(EqualPair* seen, usz seen_count, Value* left, Value* right) {
    for (usz i = 0; i < seen_count; i++) {
        if (seen[i].left == left && seen[i].right == right) return true;
    }
    return false;
}

fn bool equality_ensure_pair_capacity(EqualPair** pairs_ptr, usz* cap_ptr, usz count, usz needed,
    bool* heap_ptr, EqualityWorkspaceStats* workspace_stats, bool seen_pairs) {
    if (needed <= *cap_ptr) return true;

    usz new_cap = *cap_ptr;
    while (new_cap < needed) new_cap *= 2;

    EqualPair* old_pairs = *pairs_ptr;
    EqualPair* new_pairs = (EqualPair*)mem::malloc(EqualPair.sizeof * new_cap);
    if (new_pairs == null) return false;

    for (usz i = 0; i < count; i++) new_pairs[i] = old_pairs[i];
    if (*heap_ptr) mem::free(old_pairs);

    *pairs_ptr = new_pairs;
    *heap_ptr = true;
    *cap_ptr = new_cap;

    if (workspace_stats != null) {
        if (seen_pairs) {
            workspace_stats.seen_heap_growths++;
            workspace_stats.seen_heap_bytes += EqualPair.sizeof * new_cap;
        } else {
            workspace_stats.stack_heap_growths++;
            workspace_stats.stack_heap_bytes += EqualPair.sizeof * new_cap;
        }
    }
    return true;
}

fn bool equality_push_cons_pair(EqualPair** stack_ptr, usz* stack_cap_ptr, usz* stack_count_ptr,
    bool* stack_heap_ptr, EqualPair** seen_ptr, usz* seen_cap_ptr, usz* seen_count_ptr,
    bool* seen_heap_ptr, Value* left, Value* right, EqualityWorkspaceStats* workspace_stats) {
    if (equality_pair_seen(*seen_ptr, *seen_count_ptr, left, right)) return true;

    if (!equality_ensure_pair_capacity(seen_ptr, seen_cap_ptr, *seen_count_ptr, *seen_count_ptr + 1,
        seen_heap_ptr, workspace_stats, true)) return false;
    (*seen_ptr)[(*seen_count_ptr)++] = { .left = left, .right = right };

    if (!equality_ensure_pair_capacity(stack_ptr, stack_cap_ptr, *stack_count_ptr, *stack_count_ptr + 2,
        stack_heap_ptr, workspace_stats, false)) return false;

    (*stack_ptr)[(*stack_count_ptr)++] = { .left = left.cons_val.cdr, .right = right.cons_val.cdr };
    (*stack_ptr)[(*stack_count_ptr)++] = { .left = left.cons_val.car, .right = right.cons_val.car };
    return true;
}

fn bool equality_push_array_pairs(EqualPair** stack_ptr, usz* stack_cap_ptr, usz* stack_count_ptr,
    bool* stack_heap_ptr, EqualPair** seen_ptr, usz* seen_cap_ptr, usz* seen_count_ptr,
    bool* seen_heap_ptr, Value* left, Value* right, EqualityWorkspaceStats* workspace_stats) {
    if (left.array_val.length != right.array_val.length) return false;
    if (equality_pair_seen(*seen_ptr, *seen_count_ptr, left, right)) return true;

    if (!equality_ensure_pair_capacity(seen_ptr, seen_cap_ptr, *seen_count_ptr, *seen_count_ptr + 1,
        seen_heap_ptr, workspace_stats, true)) return false;
    (*seen_ptr)[(*seen_count_ptr)++] = { .left = left, .right = right };

    usz len = left.array_val.length;
    if (!equality_ensure_pair_capacity(stack_ptr, stack_cap_ptr, *stack_count_ptr, *stack_count_ptr + len,
        stack_heap_ptr, workspace_stats, false)) return false;

    for (usz i = len; i > 0; i--) {
        usz idx = i - 1;
        (*stack_ptr)[(*stack_count_ptr)++] = {
            .left = left.array_val.items[idx],
            .right = right.array_val.items[idx]
        };
    }
    return true;
}
