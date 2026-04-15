module lisp;
import std::core::mem;

fn bool values_equal_with_workspace_stats(Value* a, Value* b, EqualityWorkspaceStats* workspace_stats = null) {
    equality_workspace_reset_stats(workspace_stats);

    if (a == null && b == null) return true;
    if (a == null || b == null) return false;

    // Fast path for non-recursive values.
    if ((a.tag == BIG_INTEGER && (b.tag == BIG_INTEGER || b.tag == INT)) ||
        (a.tag == INT && b.tag == BIG_INTEGER)) {
        return big_integer_values_equal(a, b);
    }
    if (a.tag == BIG_COMPLEX || b.tag == BIG_COMPLEX) {
        return is_numeric_value(a) && is_numeric_value(b) && big_complex_values_equal(a, b);
    }
    if (a.tag == BIG_FLOAT || b.tag == BIG_FLOAT) {
        return is_numeric_value(a) && is_numeric_value(b) && big_float_values_equal(a, b);
    }
    if (is_number(a) && is_number(b)) {
        return to_double(a) == to_double(b);
    }
    if (a.tag != b.tag) return false;

    switch (a.tag) {
        case NIL:
        case VOID:
            return true;
        case INT:
            return a.int_val == b.int_val;
        case DOUBLE:
            return a.double_val == b.double_val;
        case STRING:
            if (a.str_len != b.str_len) return false;
            for (usz i = 0; i < a.str_len; i++) {
                if (a.str_chars[i] != b.str_chars[i]) return false;
            }
            return true;
        case SYMBOL:
            return a.sym_val == b.sym_val;
        case TIME_POINT:
            return equality_time_point_equal(a, b);
        case HASHMAP:
        case SET:
            return a == b;
        case CONS:
        case ARRAY:
            break;
        default:
            return a == b;
    }

    EqualPair[EQUAL_STACK_INLINE_CAPACITY] stack_inline = {};
    EqualPair[EQUAL_SEEN_INLINE_CAPACITY] seen_inline = {};

    EqualPair* stack = &stack_inline[0];
    EqualPair* seen = &seen_inline[0];
    bool stack_heap = false;
    bool seen_heap = false;

    usz stack_cap = EQUAL_STACK_INLINE_CAPACITY;
    usz stack_count = 0;
    usz seen_cap = EQUAL_SEEN_INLINE_CAPACITY;
    usz seen_count = 0;

    stack[stack_count++] = { .left = a, .right = b };
    bool ok = true;

    while (stack_count > 0 && ok) {
        EqualPair pair = stack[stack_count - 1];
        stack_count--;
        Value* left = pair.left;
        Value* right = pair.right;

        if (left == null && right == null) continue;
        if (left == null || right == null) {
            ok = false;
            break;
        }

        if ((left.tag == BIG_INTEGER && (right.tag == BIG_INTEGER || right.tag == INT)) ||
            (left.tag == INT && right.tag == BIG_INTEGER)) {
            if (!big_integer_values_equal(left, right)) {
                ok = false;
                break;
            }
            continue;
        }
        if (left.tag == BIG_COMPLEX || right.tag == BIG_COMPLEX) {
            if (!is_numeric_value(left) || !is_numeric_value(right) || !big_complex_values_equal(left, right)) {
                ok = false;
                break;
            }
            continue;
        }
        if (left.tag == BIG_FLOAT || right.tag == BIG_FLOAT) {
            if (!is_numeric_value(left) || !is_numeric_value(right) || !big_float_values_equal(left, right)) {
                ok = false;
                break;
            }
            continue;
        }
        if (is_number(left) && is_number(right)) {
            if (to_double(left) != to_double(right)) {
                ok = false;
                break;
            }
            continue;
        }
        if (left.tag != right.tag) {
            ok = false;
            break;
        }

        switch (left.tag) {
            case NIL:
            case VOID:
                break;
            case INT:
                if (left.int_val != right.int_val) ok = false;
                break;
            case DOUBLE:
                if (left.double_val != right.double_val) ok = false;
                break;
            case BIG_INTEGER:
                if (!big_integer_values_equal(left, right)) ok = false;
                break;
            case BIG_FLOAT:
                if (!big_float_values_equal(left, right)) ok = false;
                break;
            case BIG_COMPLEX:
                if (!big_complex_values_equal(left, right)) ok = false;
                break;
            case STRING:
                if (left.str_len != right.str_len) {
                    ok = false;
                    break;
                }
                for (usz i = 0; i < left.str_len; i++) {
                    if (left.str_chars[i] != right.str_chars[i]) {
                        ok = false;
                        break;
                    }
                }
                break;
            case SYMBOL:
                if (left.sym_val != right.sym_val) ok = false;
                break;
            case TIME_POINT: {
                if (!equality_time_point_equal(left, right)) ok = false;
                break;
            }
            case CONS: {
                ok = equality_push_cons_pair(&stack, &stack_cap, &stack_count, &stack_heap,
                    &seen, &seen_cap, &seen_count, &seen_heap, left, right, workspace_stats);
                break;
            }
            case HASHMAP:
            case SET:
                if (left != right) ok = false;
                break;
            case ARRAY: {
                ok = equality_push_array_pairs(&stack, &stack_cap, &stack_count, &stack_heap,
                    &seen, &seen_cap, &seen_count, &seen_heap, left, right, workspace_stats);
                break;
            }
            default:
                if (left != right) ok = false;
                break;
        }
    }

    if (seen_heap) mem::free(seen);
    if (stack_heap) mem::free(stack);
    return ok;
}

fn bool values_equal(Value* a, Value* b) {
    return values_equal_with_workspace_stats(a, b);
}
