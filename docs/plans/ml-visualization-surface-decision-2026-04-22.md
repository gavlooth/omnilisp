# ML Visualization Surface Decision Note

Date: 2026-04-22
Item: `AUDIT-2026-ML-VISUALIZATION-GAP`
Status: Terminal plotting, typed plot families, tensor summary,
confusion-matrix, and PPM image export slices implemented

## Current State

As of 2026-04-23, the first ML-facing visualization primitives exist:

- `ml/plot(data [options])` returns an ordinary FTXUI `graph` node for finite
  scalar series, x/y pair series, rank-1 CPU Float64/Float32 tensors, or rank-2
  `[N 2]` CPU Float64/Float32 tensors. It also accepts Array/List multi-series
  overlays and records `props.series-list` plus a graph-compatible primary
  `props.series`.
- `ml/loss-curve(losses [options])` returns an ordinary FTXUI `graph` node for
  finite scalar loss series or rank-1 CPU Float64/Float32 tensors.
- Both primitives expose the existing `props.series` contract used by
  `ui.graph`; `ml/plot` also records `props.x` when x/y data is supplied.
- Graph-family primitives accept optional `title`, `width`, `height`,
  `backend`, and non-interactive data-window `zoom` options. The only shipped
  graph backend is `'ftxui`; unsupported backends fail closed with
  `ml/backend-unsupported`.
- `ml/scatter(points [options])`, `ml/bar-chart(values [options])`,
  `ml/histogram(values [options])`, `ml/roc-curve(points [options])`, and
  `ml/pr-curve(points [options])` return graph dictionaries with typed
  `props.plot-kind` metadata and native FTXUI canvas rendering.
- `ml/heatmap(matrix [options])` returns ordinary finite matrix metadata/data.
  It lowers to a colored FTXUI canvas heatmap and does not require image
  display or PNG/JPEG support.
- `ml/tensor-summary(tensor [options])` returns ordinary tensor summary data for
  any Tensor. It includes dtype, device, payload, layout, shape, strides,
  rank, element count, byte length, and `stats`. Direct CPU Float64/Float32
  concrete tensors report finite min/max/mean stats; unsupported stat surfaces
  report `stats.status 'unavailable` and a symbolic `stats.reason` without
  hidden device-to-CPU fallback.
- `ml/confusion-matrix(predictions targets [options])` returns ordinary
  confusion-matrix data for equal-length integer class-id Array/List labels or
  rank-1 concrete CPU Float64/Float32 Tensor labels. Rows are target classes,
  columns are predicted classes, and unsupported backends or label domains fail
  closed.
- `ml/export-image(tensor path [options])` writes a dependency-free PPM image
  file for rank-2 grayscale or rank-3 HWC CPU Float64/Float32 tensors with 1 or
  3 channels. Values must be finite, non-negative, and fit either normalized
  unit range or byte range. It returns ordinary export metadata and does not
  auto-display.

The Omni runtime also has:

- Generic UI node constructors in `examples/libraries/ftxui/ui_nodes.omni:55`:
  - `(graph series)` creates a `'graph` node with a `series` property.
  - This is a declarative node, not an ML-specific primitive.
- FTXUI backend dispatch through `ui.ftxui` for terminal rendering.
- No image display, dataset preview, feature maps, PNG/JPEG encoders, or
  interactive visualization surface beyond the shipped graph-node, summary,
  confusion-matrix, and PPM export data.

## Decision: Canonical ML Visualization Primitives

Omni does not need a full matplotlib-style plotting stack at this stage. The pre-alpha visualization surface should be minimal, backend-agnostic at the declarative layer, and fail-closed when backends are unavailable.

### Canonical Primitives

| Primitive | Signature | Purpose | Backend |
|-----------|-----------|---------|---------|
| `ml/plot` | `(ml/plot data [options])` | Line/x-y/multi-series plot from tensor or list data | FTXUI canvas rendering shipped for typed plot nodes |
| `ml/loss-curve` | `(ml/loss-curve losses [options])` | Training loss curve from list/vector of scalars | FTXUI terminal graph shipped |
| `ml/scatter` | `(ml/scatter points [options])` | Scatter points with marker metadata | FTXUI canvas rendering shipped |
| `ml/bar-chart` | `(ml/bar-chart values [options])` | Bar chart values with optional labels | FTXUI canvas rendering shipped |
| `ml/histogram` | `(ml/histogram values [options])` | Histogram counts and bin edges | FTXUI canvas rendering shipped |
| `ml/heatmap` | `(ml/heatmap matrix [options])` | Finite matrix heatmap data | Colored FTXUI canvas rendering shipped |
| `ml/roc-curve` | `(ml/roc-curve points [options])` | Named ROC curve from FPR/TPR points | FTXUI canvas rendering shipped |
| `ml/pr-curve` | `(ml/pr-curve points [options])` | Named precision-recall curve | FTXUI canvas rendering shipped |
| `ml/confusion-matrix` | `(ml/confusion-matrix predictions targets [options])` | Confusion matrix display | Ordinary target-row/predicted-column data shipped |
| `ml/tensor-summary` | `(ml/tensor-summary tensor [options])` | Shape, dtype, placement, min/max/mean summary | Ordinary summary data shipped |
| `ml/export-image` | `(ml/export-image tensor path [options])` | Export tensor as image file | Dependency-free PPM writer shipped |

### Rejected Primitives

- `ml/feature-map` — too domain-specific for the current surface; defer until CNN surface is concrete.
- `ml/animate-training` — requires animation/time-loop infrastructure not yet in `ui.runtime`.
- `ml/interactive-plot` — requires mouse/event capture beyond current FTXUI contract.

### Backend Contract

1. **Declarative layer**: All visualization primitives return ordinary Omni dictionaries or UI nodes, not opaque backend handles.
2. **FTXUI terminal**: Untyped `ui.graph` still uses the existing FTXUI graph
   node lowering. Typed ML graph-family primitives lower through the FTXUI
   canvas plot shim so multi-series overlays, scatter markers, bar charts,
   histograms, ROC/PR curves, and heatmaps render natively in the terminal.
   `ml/confusion-matrix` ships as ordinary FTXUI-compatible table data.
3. **No hidden CPU fallback**: If a primitive requires image rasterization and no image backend is available, it fails closed with `ui/backend-unsupported`, not silently returning text.
   The shipped `ml/export-image` path is an in-repo PPM writer, not
   ImageMagick, gnuplot, or a shell-out backend.
4. **Data contracts**:
   - `ml/plot` accepts rank-1 CPU Float64/Float32 tensors, rank-2 `[N 2]`
     CPU Float64/Float32 tensors, numeric Array/List series, or Array/List
     series of `[x y]` numeric pairs. Array/List inputs containing multiple
     scalar series produce `props.plot-kind 'multi-series`.
   - `ml/loss-curve` accepts rank-1 CPU Float64/Float32 tensors or numeric
     Array/List scalar series.
   - `ml/scatter`, `ml/roc-curve`, and `ml/pr-curve` accept x/y pair data or
     rank-2 `[N 2]` CPU Float64/Float32 tensors.
   - `ml/bar-chart` and `ml/histogram` accept scalar numeric Array/List series
     or rank-1 CPU Float64/Float32 tensors.
   - `ml/heatmap` accepts finite rectangular numeric Array/List matrix data or
     rank-2 CPU Float64/Float32 tensors.
   - `ml/confusion-matrix` accepts equal-length integer class-id Array/List
     labels or rank-1 concrete CPU Float64/Float32 Tensor labels. Rows are
     target classes and columns are predicted classes.
   - `ml/tensor-summary` accepts any tensor.
   - `ml/export-image` accepts rank-2 grayscale or rank-3 HWC CPU Float64/Float32
     tensors with 1 or 3 channels. The default range mode is `'auto`: values
     `0..1` are scaled as unit range, values above `1` up to `255` are written
     as byte range. Explicit `'range` may be `'auto`, `'unit`, or `'byte`.
     Explicit `'format` may only be `'ppm`; explicit `'backend` may be `'image`
     or `'ppm`.
5. **Options dictionary**: All primitives accept an optional `options` dict with:
   - `'title` — string
   - `'width` / `'height` — positive integer (backend may ignore if terminal-size constrained)
   - `'backend` — symbol (`'ftxui`, `'image`, etc.)
   - `'zoom` — optional dictionary with `x-min`, `x-max`, `y-min`, and `y-max`
     numeric fields for non-interactive data-window metadata. Graph primitives
     apply `x-min`/`x-max` to the renderable `props.series` where the input has
     a direct x or index domain.
   - Plot-specific metadata such as `marker`, `legend`, `labels`, or `bins`
     where supported.

## Scope Boundaries

- **Shipped**: `ml/plot`, `ml/loss-curve`, `ml/scatter`, `ml/bar-chart`,
  `ml/histogram`, `ml/roc-curve`, and `ml/pr-curve` graph primitives with
  native FTXUI canvas rendering for typed plot metadata; `ml/heatmap` ordinary
  matrix data with colored FTXUI canvas rendering; `ml/tensor-summary`
  ordinary summary data; `ml/confusion-matrix` ordinary target-row/predicted-
  column data; `ml/export-image` dependency-free PPM file export.
- **Out of scope until explicit milestone**: PNG/JPEG encoders, image display,
  pixel buffer ABI, interactive plot events.
- **Remaining backlog**: none for this ML visualization surface decision.

## Negative Constraints

- Do not add a matplotlib or gnuplot FFI dependency.
- Do not add ImageMagick or shell-out tooling for the shipped export path.
- Do not introduce a dedicated `Image` or `PixelBuffer` Omni value type for this surface alone.
- Do not make visualization primitives auto-display; they return declarative data that must be passed to a UI backend or renderer.

## Next Steps

1. `ML-VIZ-001`: closed by the 2026-04-23 `ml/plot` / `ml/loss-curve`
   terminal graph-node implementation.
2. `ML-VIZ-002`: closed by the 2026-04-23 `ml/tensor-summary`
   implementation.
3. `ML-VIZ-003`: closed by the 2026-04-23 `ml/confusion-matrix`
   implementation.
4. `ML-VIZ-004`: closed by the 2026-04-23 `ml/export-image` PPM writer
   implementation.
5. `ML-VIZ-005`: closed by the 2026-04-23 typed plot-family expansion.
6. `ML-VIZ-006`: closed by the 2026-04-23 native FTXUI canvas rendering
   extension for typed ML plots.
