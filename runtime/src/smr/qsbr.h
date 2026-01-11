#ifndef OMNI_QSBR_H
#define OMNI_QSBR_H

/*
 * qsbr.h - Quiescent State Based Reclamation (QSBR) stubs
 *
 * The runtime currently calls `qsbr_quiescent()` at a few safe points
 * (e.g. channel send/recv) as a placeholder for a future QSBR/SMR
 * implementation.
 *
 * IMPORTANT:
 * - This is NOT a heap GC and must never scan the heap.
 * - This header provides a minimal, tool-friendly stub so the runtime
 *   builds in configurations where QSBR is not yet implemented.
 *
 * Future work (Issue 4):
 * - Replace this header-only stub with a real QSBR implementation that
 *   is limited to internal runtime DS nodes (not user heap).
 */

static inline void qsbr_quiescent(void)
{
	/*
	 * A real QSBR implementation would report a quiescent state for the
	 * current thread, allowing reclamation of retired nodes once all
	 * threads have passed through a quiescent point.
	 *
	 * For now, provide a compiler barrier so call sites remain meaningful.
	 */
	__atomic_thread_fence(__ATOMIC_SEQ_CST);
}

#endif /* OMNI_QSBR_H */

