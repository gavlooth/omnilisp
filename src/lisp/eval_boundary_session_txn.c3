module lisp;

import main;
import std::io;

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
