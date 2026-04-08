module lisp;

import std::io;

struct BoundaryRuntimePolicy {
    bool initialized;
    bool strict_mode;
    bool scope_chain_scan_bypass;
    usz scope_chain_scan_budget;
    bool graph_audit;
    usz graph_audit_rate;
    usz graph_audit_max_roots;
    bool verbose_telemetry;
    bool trace_runtime;
    bool benchmark_trace_runtime;
}

BoundaryRuntimePolicy g_boundary_runtime_policy;
bool g_boundary_policy_invalid_warned;
bool g_boundary_policy_snapshot_emitted;

fn BoundaryRuntimePolicy* boundary_runtime_policy_get() @inline {
    if (!g_boundary_runtime_policy.initialized) {
        boundary_policy_load_from_env();
    }
    return &g_boundary_runtime_policy;
}

fn bool boundary_policy_scope_chain_scan_bypass_enabled() @inline {
    BoundaryRuntimePolicy* policy = boundary_runtime_policy_get();
    return policy.scope_chain_scan_bypass;
}

fn usz boundary_policy_scope_chain_scan_budget() @inline {
    BoundaryRuntimePolicy* policy = boundary_runtime_policy_get();
    return policy.scope_chain_scan_budget;
}

fn bool boundary_policy_graph_audit_enabled() @inline {
    BoundaryRuntimePolicy* policy = boundary_runtime_policy_get();
    return policy.graph_audit;
}

fn usz boundary_policy_graph_audit_rate() @inline {
    BoundaryRuntimePolicy* policy = boundary_runtime_policy_get();
    return policy.graph_audit_rate;
}

fn usz boundary_policy_graph_audit_max_roots() @inline {
    BoundaryRuntimePolicy* policy = boundary_runtime_policy_get();
    return policy.graph_audit_max_roots;
}

fn bool boundary_policy_verbose_telemetry_enabled() @inline {
    BoundaryRuntimePolicy* policy = boundary_runtime_policy_get();
    return policy.verbose_telemetry;
}

fn bool boundary_policy_trace_runtime_enabled() @inline {
    BoundaryRuntimePolicy* policy = boundary_runtime_policy_get();
    return policy.trace_runtime;
}

fn bool boundary_policy_benchmark_trace_enabled() @inline {
    BoundaryRuntimePolicy* policy = boundary_runtime_policy_get();
    return policy.benchmark_trace_runtime;
}

fn void boundary_policy_emit_snapshot_once(char[] label = "runtime") @noinline {
    if (g_boundary_policy_snapshot_emitted) return;
    BoundaryRuntimePolicy* policy = boundary_runtime_policy_get();
    io::eprintfn(
        "[boundary][policy][%s] strict=%d scan_bypass=%d scan_budget=%d graph_audit=%d graph_audit_rate=%d graph_audit_max_roots=%d verbose=%d trace=%d benchmark_trace=%d",
        (ZString)label,
        policy.strict_mode ? 1 : 0,
        policy.scope_chain_scan_bypass ? 1 : 0,
        (long)policy.scope_chain_scan_budget,
        policy.graph_audit ? 1 : 0,
        (long)policy.graph_audit_rate,
        (long)policy.graph_audit_max_roots,
        policy.verbose_telemetry ? 1 : 0,
        policy.trace_runtime ? 1 : 0,
        policy.benchmark_trace_runtime ? 1 : 0
    );
    g_boundary_policy_snapshot_emitted = true;
}
