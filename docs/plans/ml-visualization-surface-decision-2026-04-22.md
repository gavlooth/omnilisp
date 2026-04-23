# ML Visualization Surface Decision Note

Date: 2026-04-22
Item: `AUDIT-2026-ML-VISUALIZATION-GAP`
Status: Decision recorded, implementation deferred to post-audit backlog

## Current State

No ML-facing visualization primitives exist. The Omni runtime has:

- Generic UI node constructors in `examples/libraries/ftxui/ui_nodes.omni:55`:
  - `(graph series)` creates a `'graph` node with a `series` property.
  - This is a declarative node, not an ML-specific primitive.
- FTXUI backend dispatch through `ui.ftxui` for terminal rendering.
- No image display, dataset preview, training curves, confusion matrices, feature maps, or export surface.

## Decision: Canonical ML Visualization Primitives

Omni does not need a full matplotlib-style plotting stack at this stage. The pre-alpha visualization surface should be minimal, backend-agnostic at the declarative layer, and fail-closed when backends are unavailable.

### Proposed Primitives

| Primitive | Signature | Purpose | Backend |
|-----------|-----------|---------|---------|
| `ml/plot` | `(ml/plot data [options])` | Line/scatter plot from tensor or list data | FTXUI terminal graph, future image backend |
| `ml/loss-curve` | `(ml/loss-curve losses [options])` | Training loss curve from list/vector of scalars | FTXUI terminal graph |
| `ml/confusion-matrix` | `(ml/confusion-matrix predictions targets [options])` | Confusion matrix display | Terminal text table, future image backend |
| `ml/tensor-summary` | `(ml/tensor-summary tensor [options])` | Shape, dtype, placement, min/max/mean summary | Terminal text |
| `ml/export-image` | `(ml/export-image tensor path [options])` | Export tensor as image file | CPU-only, fail-closed for non-image dtypes |

### Rejected Primitives

- `ml/feature-map` — too domain-specific for the current surface; defer until CNN surface is concrete.
- `ml/animate-training` — requires animation/time-loop infrastructure not yet in `ui.runtime`.
- `ml/interactive-plot` — requires mouse/event capture beyond current FTXUI contract.

### Backend Contract

1. **Declarative layer**: All visualization primitives return ordinary Omni dictionaries or UI nodes, not opaque backend handles.
2. **FTXUI terminal**: The default backend for `ml/plot`, `ml/loss-curve`, and `ml/confusion-matrix` is the existing FTXUI `graph` / `table` node lowering.
3. **No hidden CPU fallback**: If a primitive requires image rasterization and no image backend is available, it fails closed with `ui/backend-unsupported`, not silently returning text.
4. **Data contracts**:
   - `ml/plot` accepts rank-1 or rank-2 tensors, or lists of `[x y]` pairs.
   - `ml/loss-curve` accepts rank-1 tensors or lists of scalars.
   - `ml/confusion-matrix` accepts two rank-1 tensors of equal length.
   - `ml/tensor-summary` accepts any tensor.
   - `ml/export-image` accepts rank-2 or rank-3 tensors with dtype representable as grayscale/RGB.
5. **Options dictionary**: All primitives accept an optional `options` dict with:
   - `'title` — string
   - `'width` / `'height` — positive integer (backend may ignore if terminal-size constrained)
   - `'backend` — symbol (`'ftxui`, `'image`, etc.)

## Scope Boundaries

- **In scope for immediate backlog**: Decision note, primitive names, option contracts.
- **Out of scope until explicit milestone**: Image rasterization backend, pixel buffer ABI, file-format encoders (PNG, JPEG), interactive plot events.
- **Dependency on existing work**: `ml/plot` and `ml/loss-curve` can ship once the FTXUI `graph` node lowering is verified for arbitrary series data.

## Negative Constraints

- Do not add a matplotlib or gnuplot FFI dependency.
- Do not introduce a dedicated `Image` or `PixelBuffer` Omni value type for this surface alone.
- Do not make visualization primitives auto-display; they return declarative data that must be passed to a UI backend or renderer.

## Next Steps

1. Create backlog item `ML-VIZ-001` for `ml/plot` / `ml/loss-curve` terminal implementation.
2. Create backlog item `ML-VIZ-002` for `ml/tensor-summary`.
3. Create backlog item `ML-VIZ-003` for `ml/confusion-matrix`.
4. Create backlog item `ML-VIZ-004` for `ml/export-image` (deferred until image format backend).
