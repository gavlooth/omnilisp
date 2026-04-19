/*
 * arena_reset - Reset arena, release all physical pages to OS
 */
void arena_reset(Arena* a)
{
	for (VMemChunk* c = a->begin; c; c = c->next) {
		vmem_chunk_reset(c);
	}
	a->end = a->begin;
}

/*
 * arena_rewind - Rewind to a previous snapshot
 */
void arena_rewind(Arena* a, Arena_Mark m)
{
	if (!m.chunk) {
		arena_reset(a);
		return;
	}

	m.chunk->offset = m.offset;

	/* Reset all chunks after the snapshot */
	for (VMemChunk* c = m.chunk->next; c; c = c->next) {
		c->offset = 0;
	}

	a->end = m.chunk;
}

/*
 * arena_free - Release all chunks and their VA space
 */
void arena_free(Arena* a)
{
	VMemChunk* c = a->begin;
	while (c) {
		VMemChunk* next = c->next;
		vmem_chunk_free(c);
		c = next;
	}
	a->begin = NULL;
	a->end = NULL;
}

/*
 * arena_trim - Free unused chunks after current position
 */
void arena_trim(Arena* a)
{
	if (!a->end)
		return;

	VMemChunk* c = a->end->next;
	a->end->next = NULL;

	while (c) {
		VMemChunk* next = c->next;
		vmem_chunk_free(c);
		c = next;
	}
}

/*
 * arena_promote - Move all chunks from src to end of dest
 */
void arena_promote(Arena* dest, Arena* src)
{
	if (!src->begin)
		return;

	if (dest->end) {
		dest->end->next = src->begin;
		dest->end = src->end;
	} else {
		dest->begin = src->begin;
		dest->end = src->end;
	}

	src->begin = NULL;
	src->end = NULL;
}

/*
 * arena_detach_blocks - O(1) splice: remove chunk range from arena
 *
 * Critical for region merging in OmniLisp.
 */
void arena_detach_blocks(Arena* a, VMemChunk* start, VMemChunk* end)
{
	if (!a || !start || !end)
		return;

	/* Find predecessor of start */
	if (a->begin == start) {
		a->begin = end->next;
	} else {
		VMemChunk* prev = a->begin;
		while (prev && prev->next != start)
			prev = prev->next;
		if (prev)
			prev->next = end->next;
	}

	/* Update end pointer if needed */
	if (a->end == end) {
		if (!a->begin) {
			a->end = NULL;
		} else {
			VMemChunk* c = a->begin;
			while (c->next)
				c = c->next;
			a->end = c;
		}
	}

	/* Isolate the detached range */
	end->next = NULL;
}

/*
 * arena_attach_blocks - O(1) splice: append chunk range to arena
 *
 * Critical for region merging in OmniLisp.
 */
void arena_attach_blocks(Arena* a, VMemChunk* start, VMemChunk* end)
{
	if (!a || !start || !end)
		return;

	if (a->end) {
		a->end->next = start;
	} else {
		a->begin = start;
	}
	a->end = end;
}
