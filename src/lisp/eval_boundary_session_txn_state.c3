module lisp;

import main;
import std::io;

struct BoundaryInterpState {
    main::ScopeRegion* current_scope;
    main::ScopeRegion* releasing_scope;
}

struct BoundarySession {
    main::ScopeRegion* current_scope;
    main::ScopeRegion* releasing_scope;
    bool active;
}

struct BoundaryScopeSwap {
    main::ScopeRegion* saved_scope;
    bool active;
}

struct BoundaryReleasingScopeSwap {
    main::ScopeRegion* saved_releasing_scope;
    bool active;
}

enum BoundaryTxnState : char {
    BOUNDARY_TXN_INIT,
    BOUNDARY_TXN_OPEN,
    BOUNDARY_TXN_COMMITTED,
    BOUNDARY_TXN_ABORTED,
    BOUNDARY_TXN_CLOSED,
}

const usz BOUNDARY_TXN_STATE_COUNT =
    (usz)BoundaryTxnState.BOUNDARY_TXN_CLOSED + 1;

enum BoundaryTxnAction : char {
    BOUNDARY_TXN_ACTION_COMMIT,
    BOUNDARY_TXN_ACTION_ABORT,
    BOUNDARY_TXN_ACTION_CLOSE,
}

const usz BOUNDARY_TXN_ACTION_COUNT =
    (usz)BoundaryTxnAction.BOUNDARY_TXN_ACTION_CLOSE + 1;

struct BoundaryTxn {
    BoundarySession session;
    BoundaryTxnState state;
}

const char[][] BOUNDARY_TXN_STATE_NAMES = {
    "init",
    "open",
    "committed",
    "aborted",
    "closed",
};

$assert(BOUNDARY_TXN_STATE_NAMES.len == BOUNDARY_TXN_STATE_COUNT);

const char[][] BOUNDARY_TXN_ACTION_NAMES = {
    "commit",
    "abort",
    "close",
};

$assert(BOUNDARY_TXN_ACTION_NAMES.len == BOUNDARY_TXN_ACTION_COUNT);

fn void boundary_assert_releasing_scope_swap_state(BoundaryReleasingScopeSwap* swap) @inline {
    assert(swap != null, "boundary releasing-scope swap: null swap");
}

<*
@require interp != null : "boundary_scope_swap_begin: null interp"
@require target_scope != null : "boundary_scope_swap_begin: null target_scope"
@ensure return.active : "boundary_scope_swap_begin: returned inactive swap"
@ensure return.saved_scope != null : "boundary_scope_swap_begin: missing saved scope"
*>
fn BoundaryScopeSwap boundary_scope_swap_begin(Interp* interp, main::ScopeRegion* target_scope) @inline {
    boundary_assert_interp_scope_chain(interp);
    assert(target_scope != null, "boundary scope swap: null target_scope");
    BoundaryScopeSwap swap = {
        .saved_scope = interp.current_scope,
        .active = true,
    };
    interp.current_scope = target_scope;
    boundary_assert_interp_scope_chain(interp);
    return swap;
}

<*
@require interp != null : "boundary_scope_swap_end: null interp"
@require swap != null : "boundary_scope_swap_end: null swap"
@require swap.active : "boundary_scope_swap_end: swap must be active"
@require swap.saved_scope != null : "boundary_scope_swap_end: missing saved scope"
@ensure !swap.active : "boundary_scope_swap_end: swap must be inactive after restore"
@ensure interp.current_scope == swap.saved_scope : "boundary_scope_swap_end: current_scope restore mismatch"
*>
fn void boundary_scope_swap_end(Interp* interp, BoundaryScopeSwap* swap) @inline {
    boundary_assert_scope_swap_state(swap);
    assert(swap.active, "boundary scope swap: end on inactive swap");
    interp.current_scope = swap.saved_scope;
    swap.active = false;
    boundary_assert_interp_scope_chain(interp);
}

<*
@require interp != null : "boundary_releasing_scope_swap_begin: null interp"
@require releasing_scope != null : "boundary_releasing_scope_swap_begin: null releasing_scope"
@ensure return.active : "boundary_releasing_scope_swap_begin: returned inactive swap"
*>
fn BoundaryReleasingScopeSwap boundary_releasing_scope_swap_begin(
    Interp* interp,
    main::ScopeRegion* releasing_scope
) @inline {
    boundary_assert_interp_scope_chain(interp);
    BoundaryReleasingScopeSwap swap = {
        .saved_releasing_scope = interp.releasing_scope,
        .active = true,
    };
    interp.releasing_scope = releasing_scope;
    return swap;
}

<*
@require interp != null : "boundary_releasing_scope_swap_end: null interp"
@require swap != null : "boundary_releasing_scope_swap_end: null swap"
@require swap.active : "boundary_releasing_scope_swap_end: swap must be active"
@ensure !swap.active : "boundary_releasing_scope_swap_end: swap must be inactive after restore"
@ensure interp.releasing_scope == swap.saved_releasing_scope : "boundary_releasing_scope_swap_end: releasing_scope restore mismatch"
*>
fn void boundary_releasing_scope_swap_end(
    Interp* interp,
    BoundaryReleasingScopeSwap* swap
) @inline {
    boundary_assert_releasing_scope_swap_state(swap);
    assert(swap.active, "boundary releasing-scope swap: end on inactive swap");
    interp.releasing_scope = swap.saved_releasing_scope;
    swap.active = false;
    boundary_assert_interp_scope_chain(interp);
}

fn void boundary_assert_txn_state(BoundaryTxn* txn) @inline {
    assert(txn != null, "boundary txn: null txn");
}

fn char[] boundary_txn_state_name(BoundaryTxnState state) @inline {
    usz idx = (usz)state;
    if (idx < BOUNDARY_TXN_STATE_NAMES.len) return BOUNDARY_TXN_STATE_NAMES[idx];
    return "unknown";
}

fn char[] boundary_txn_action_name(BoundaryTxnAction action) @inline {
    usz idx = (usz)action;
    if (idx < BOUNDARY_TXN_ACTION_NAMES.len) return BOUNDARY_TXN_ACTION_NAMES[idx];
    return "unknown";
}

fn bool boundary_txn_transition_allowed(BoundaryTxnState state, BoundaryTxnAction action) @inline {
    switch (action) {
        case BOUNDARY_TXN_ACTION_COMMIT:
            return state == BOUNDARY_TXN_OPEN;
        case BOUNDARY_TXN_ACTION_ABORT:
            return state == BOUNDARY_TXN_OPEN;
        case BOUNDARY_TXN_ACTION_CLOSE:
            return state == BOUNDARY_TXN_OPEN ||
                   state == BOUNDARY_TXN_COMMITTED ||
                   state == BOUNDARY_TXN_ABORTED;
    }
}

fn void boundary_txn_invalid_transition(BoundaryTxnState state, BoundaryTxnAction action) @noinline {
    usz state_index = (usz)state;
    usz action_index = (usz)action;
    assert(state_index < BOUNDARY_TXN_STATE_NAMES.len, "boundary txn: invalid state index");
    assert(action_index < BOUNDARY_TXN_ACTION_NAMES.len, "boundary txn: invalid action index");
    // Keep failure reporting cold and side-effect free on success paths.
    // Non-throwing try-ops (`boundary_txn_try_*`) are the protocol guard
    // surface used by misuse tests and callers that need explicit branching.
    io::eprintfn(
        "[boundary][txn] invalid transition state=%s action=%s",
        (ZString)boundary_txn_state_name(state),
        (ZString)boundary_txn_action_name(action)
    );
    unreachable("boundary txn protocol violation");
}
