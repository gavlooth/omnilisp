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

<*
@require interp != null : "boundary_txn_begin: null interp"
@ensure return.state == BOUNDARY_TXN_OPEN : "boundary_txn_begin: txn not opened"
@ensure return.session.active : "boundary_txn_begin: session should be active"
*>
fn BoundaryTxn boundary_txn_begin(Interp* interp) @inline {
    BoundaryTxn txn = {};
    txn.session = boundary_session_begin(interp);
    txn.state = BOUNDARY_TXN_OPEN;
    return txn;
}

fn bool boundary_txn_try_commit(BoundaryTxn* txn) @inline {
    if (txn == null) return false;
    if (!boundary_txn_transition_allowed(txn.state, BOUNDARY_TXN_ACTION_COMMIT)) return false;
    txn.state = BOUNDARY_TXN_COMMITTED;
    return true;
}

<*
@require txn != null : "boundary_txn_commit: null txn"
@require txn.state == BOUNDARY_TXN_OPEN : "boundary_txn_commit: txn must be open"
@ensure txn.state == BOUNDARY_TXN_COMMITTED : "boundary_txn_commit: txn not committed"
*>
fn void boundary_txn_commit(BoundaryTxn* txn) @inline {
    boundary_assert_txn_state(txn);
    if (boundary_txn_try_commit(txn)) return;
    boundary_txn_invalid_transition(txn.state, BOUNDARY_TXN_ACTION_COMMIT);
}

fn bool boundary_txn_try_abort(BoundaryTxn* txn) @inline {
    if (txn == null) return false;
    if (!boundary_txn_transition_allowed(txn.state, BOUNDARY_TXN_ACTION_ABORT)) return false;
    txn.state = BOUNDARY_TXN_ABORTED;
    return true;
}

<*
@require txn != null : "boundary_txn_abort: null txn"
@require txn.state == BOUNDARY_TXN_OPEN : "boundary_txn_abort: txn must be open"
@ensure txn.state == BOUNDARY_TXN_ABORTED : "boundary_txn_abort: txn not aborted"
*>
fn void boundary_txn_abort(BoundaryTxn* txn) @inline {
    boundary_assert_txn_state(txn);
    if (boundary_txn_try_abort(txn)) return;
    boundary_txn_invalid_transition(txn.state, BOUNDARY_TXN_ACTION_ABORT);
}

fn bool boundary_txn_try_close(Interp* interp, BoundaryTxn* txn) @inline {
    if (interp == null || txn == null) return false;
    if (!boundary_txn_transition_allowed(txn.state, BOUNDARY_TXN_ACTION_CLOSE)) return false;
    boundary_session_end(interp, &txn.session);
    txn.state = BOUNDARY_TXN_CLOSED;
    return true;
}

<*
@require interp != null : "boundary_txn_close: null interp"
@require txn != null : "boundary_txn_close: null txn"
@require txn.state != BOUNDARY_TXN_INIT : "boundary_txn_close: close from init"
@require txn.state != BOUNDARY_TXN_CLOSED : "boundary_txn_close: double-close"
@ensure txn.state == BOUNDARY_TXN_CLOSED : "boundary_txn_close: txn should be closed"
@ensure !txn.session.active : "boundary_txn_close: session should be inactive"
*>
fn void boundary_txn_close(Interp* interp, BoundaryTxn* txn) @inline {
    boundary_assert_txn_state(txn);
    if (boundary_txn_try_close(interp, txn)) return;
    boundary_txn_invalid_transition(txn.state, BOUNDARY_TXN_ACTION_CLOSE);
}

<*
@require interp != null : "boundary_save_interp_state: null interp"
*>
fn BoundaryInterpState boundary_save_interp_state(Interp* interp) @inline {
    boundary_assert_interp_scope_chain(interp);
    return {
        .current_scope = interp.current_scope,
        .releasing_scope = interp.releasing_scope,
    };
}

<*
@require interp != null : "boundary_restore_interp_state: null interp"
@ensure interp.current_scope == saved.current_scope : "boundary_restore_interp_state: current_scope restore mismatch"
@ensure interp.releasing_scope == saved.releasing_scope : "boundary_restore_interp_state: releasing_scope restore mismatch"
*>
fn void boundary_restore_interp_state(Interp* interp, BoundaryInterpState saved) @inline {
    boundary_assert_saved_state(saved);
    interp.current_scope = saved.current_scope;
    interp.releasing_scope = saved.releasing_scope;
    boundary_assert_interp_scope_chain(interp);
}

<*
@require interp != null : "boundary_session_begin: null interp"
*>
fn BoundarySession boundary_session_begin(Interp* interp) @inline {
    boundary_assert_interp_scope_chain(interp);
    return {
        .current_scope = interp.current_scope,
        .releasing_scope = interp.releasing_scope,
        .active = true,
    };
}

<*
@require interp != null : "boundary_session_end: null interp"
@require session != null : "boundary_session_end: null session"
@require session.active : "boundary_session_end: session must be active"
@ensure !session.active : "boundary_session_end: session should be inactive after restore"
@ensure interp.current_scope == session.current_scope : "boundary_session_end: current_scope restore mismatch"
@ensure interp.releasing_scope == session.releasing_scope : "boundary_session_end: releasing_scope restore mismatch"
*>
fn void boundary_session_end(Interp* interp, BoundarySession* session) @inline {
    boundary_assert_session_state(session);
    assert(session.active, "boundary session: end on inactive session");
    interp.current_scope = session.current_scope;
    interp.releasing_scope = session.releasing_scope;
    session.active = false;
    assert(interp.current_scope == session.current_scope, "boundary session: current_scope restore mismatch");
    assert(interp.releasing_scope == session.releasing_scope, "boundary session: releasing_scope restore mismatch");
    boundary_assert_interp_scope_chain(interp);
}
