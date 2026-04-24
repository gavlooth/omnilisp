module lisp;

enum BoundaryValueOwnershipPolicy : char {
    BOUNDARY_VALUE_IMMEDIATE,
    BOUNDARY_VALUE_HEAP_CLONE,
    BOUNDARY_VALUE_GRAPH_CONTAINER,
    BOUNDARY_VALUE_CLOSURE_GRAPH,
    BOUNDARY_VALUE_SHARED_RUNTIME_WRAPPER,
    BOUNDARY_VALUE_OPAQUE_FOREIGN,
    BOUNDARY_VALUE_INTERPRETER_STABLE,
    BOUNDARY_VALUE_UNSUPPORTED_BOUNDARY,
}

enum BoundaryValueEdgePolicy : char {
    BOUNDARY_VALUE_EDGE_NONE,
    BOUNDARY_VALUE_EDGE_VALUE,
    BOUNDARY_VALUE_EDGE_ENV,
}

fn BoundaryValueOwnershipPolicy boundary_value_ownership_policy(ValueTag tag) @inline {
    switch (tag) {
        case NIL:
        case VOID:
        case INT:
        case DOUBLE:
        case FLOAT32:
        case COMPLEX128:
        case COMPLEX64:
        case SYMBOL:
        case TIME_POINT:
            return BOUNDARY_VALUE_IMMEDIATE;

        case STRING:
        case ERROR:
        case BIG_INTEGER:
        case BIG_FLOAT:
        case BIG_COMPLEX:
            return BOUNDARY_VALUE_HEAP_CLONE;

        case CONS:
        case ARRAY:
        case HASHMAP:
        case SET:
            return BOUNDARY_VALUE_GRAPH_CONTAINER;

        case CLOSURE:
            return BOUNDARY_VALUE_CLOSURE_GRAPH;

        case PARTIAL_PRIM:
        case CONTINUATION:
        case INSTANCE:
        case ITERATOR:
        case COROUTINE:
        case TENSOR:
            return BOUNDARY_VALUE_SHARED_RUNTIME_WRAPPER;

        case FFI_HANDLE:
            return BOUNDARY_VALUE_OPAQUE_FOREIGN;

        case PRIMITIVE:
        case TYPE_INFO:
        case MODULE:
        case METHOD_TABLE:
            return BOUNDARY_VALUE_INTERPRETER_STABLE;
    }
}

fn BoundaryValueEdgePolicy boundary_value_edge_policy(ValueTag tag) @inline {
    switch (tag) {
        case CONS:
        case ARRAY:
        case HASHMAP:
        case SET:
        case PARTIAL_PRIM:
        case INSTANCE:
        case METHOD_TABLE:
        case ITERATOR:
        case TENSOR:
            return BOUNDARY_VALUE_EDGE_VALUE;

        case CLOSURE:
        case MODULE:
            return BOUNDARY_VALUE_EDGE_ENV;

        case NIL:
        case VOID:
        case INT:
        case DOUBLE:
        case FLOAT32:
        case COMPLEX128:
        case COMPLEX64:
        case SYMBOL:
        case TIME_POINT:
        case STRING:
        case ERROR:
        case BIG_INTEGER:
        case BIG_FLOAT:
        case BIG_COMPLEX:
        case CONTINUATION:
        case PRIMITIVE:
        case FFI_HANDLE:
        case TYPE_INFO:
        case COROUTINE:
            return BOUNDARY_VALUE_EDGE_NONE;
    }
}

fn bool boundary_value_tag_has_omni_owned_edges(ValueTag tag) @inline {
    return boundary_value_edge_policy(tag) != BOUNDARY_VALUE_EDGE_NONE;
}

fn bool boundary_value_tag_is_opaque_foreign(ValueTag tag) @inline {
    return boundary_value_ownership_policy(tag) == BOUNDARY_VALUE_OPAQUE_FOREIGN;
}

fn bool boundary_value_tag_requires_materialized_payload_clone(ValueTag tag) @inline {
    return boundary_value_ownership_policy(tag) == BOUNDARY_VALUE_HEAP_CLONE;
}

fn bool boundary_value_tag_is_stable_materialization_candidate(ValueTag tag) @inline {
    switch (boundary_value_ownership_policy(tag)) {
        case BOUNDARY_VALUE_HEAP_CLONE:
        case BOUNDARY_VALUE_GRAPH_CONTAINER:
        case BOUNDARY_VALUE_CLOSURE_GRAPH:
            return tag != ERROR;

        case BOUNDARY_VALUE_IMMEDIATE:
            return tag == TIME_POINT;

        case BOUNDARY_VALUE_SHARED_RUNTIME_WRAPPER:
        case BOUNDARY_VALUE_OPAQUE_FOREIGN:
        case BOUNDARY_VALUE_INTERPRETER_STABLE:
        case BOUNDARY_VALUE_UNSUPPORTED_BOUNDARY:
            return false;
    }
}
