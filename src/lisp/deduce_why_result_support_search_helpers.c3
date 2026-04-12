module lisp;

import std::core::mem;

struct DeduceWhyResultVisitedSubject {
    SymbolId                       predicate;
    Value*[]                       tuple;
    DeduceWhyResultVisitedSubject* parent;
}

fn bool deduce_why_result_visited_contains(
    DeduceWhyResultVisitedSubject* visited,
    SymbolId predicate,
    Value*[] tuple
) {
    DeduceWhyResultVisitedSubject* cur = visited;
    while (cur != null) {
        if (cur.predicate == predicate && cur.tuple.len == tuple.len) {
            bool same = true;
            foreach (i, val : tuple) {
                if (deduce_compare_value(cur.tuple[i], val) != 0) {
                    same = false;
                    break;
                }
            }
            if (same) return true;
        }
        cur = cur.parent;
    }
    return false;
}

fn bool deduce_why_result_clone_bindings(
    bool[] bound,
    Value*[] bound_values,
    bool[]* bound_out,
    Value*[]* bound_values_out
) {
    if (bound_out != null) *bound_out = {};
    if (bound_values_out != null) *bound_values_out = {};
    if (bound.len != bound_values.len || bound_out == null || bound_values_out == null) return false;

    bool[] next_bound = mem::new_array(bool, bound.len);
    if (bound.len > 0 && next_bound.ptr == null) return false;
    Value*[] next_values = mem::new_array(Value*, bound_values.len);
    if (bound_values.len > 0 && next_values.ptr == null) {
        if (next_bound.ptr != null) mem::free(next_bound.ptr);
        return false;
    }

    if (bound.len > 0) mem::copy(next_bound.ptr, bound.ptr, bool.sizeof * bound.len);
    if (bound_values.len > 0) mem::copy(next_values.ptr, bound_values.ptr, Value*.sizeof * bound_values.len);
    *bound_out = next_bound;
    *bound_values_out = next_values;
    return true;
}

fn Value* deduce_why_result_concat_support(Value* front, Value* tail, Interp* interp) {
    Value* rev = make_nil(interp);
    Value* cur = front;
    while (is_cons(cur)) {
        rev = make_cons(interp, car(cur), rev);
        if (rev != null && rev.tag == ERROR) return rev;
        cur = cdr(cur);
    }
    Value* out = tail == null ? make_nil(interp) : tail;
    while (is_cons(rev)) {
        out = make_cons(interp, car(rev), out);
        if (out != null && out.tag == ERROR) return out;
        rev = cdr(rev);
    }
    return out;
}
