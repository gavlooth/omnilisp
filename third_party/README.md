# Third-Party Dependencies

This directory contains vendored C libraries used by the OmniLisp runtime.

## Libraries

### arena (Alexey Kutepov)
- **URL**: https://github.com/tsoding/arena
- **License**: MIT
- **Description**: Vendored arena allocator headers
- **Files**: `arena/arena.h`, `arena/arena_config.h`, `arena/vmem_arena.h`
- **Notes**:
  - `arena/arena.h` is upstream vendor code and still contains upstream TODO comments.
  - Those TODOs are not tracked as Omni runtime backlog items.
  - Omni-specific allocator selection should include `arena/arena_config.h`, which dispatches between the vendored upstream allocator and the Omni `vmem_arena` implementation.

## Usage

```c
#include "arena/arena_config.h"

Arena tmp_arena = {0};
void* ptr = arena_alloc(&tmp_arena, size);
// ... use ptr ...
arena_free(&tmp_arena);  // Free all at once
```

Used in OmniLisp for:
- Transmigration temporary allocations (forwarding tables, work queues)
- Short-lived data structures during region operations
