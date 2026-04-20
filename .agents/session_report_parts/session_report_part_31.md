# 2026-04-20 16:25 CEST - Omni Neural DataSpec Decision

- Objective attempted:
  - Make the data-oriented neural-network design official, name it, and record
    an implementation plan that keeps Vulkan first-class for inference and
    training.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - `ML-VK-070` model/layer library and serialization.
- Documentation changes made:
  - Added `docs/plans/omni-neural-dataspec-plan-2026-04-20.md`.
  - Linked the plan from `docs/plans/README.md`.
  - Updated the Vulkan ML roadmap `ML-VK-070` section to point to the accepted
    Omni Neural DataSpec design.
  - Expanded `docs/todo_parts/todo_part_14.md` with TODO-backed
    `ML-VK-070-001` through `ML-VK-070-005` slices.
  - Updated `memory/changelog_parts/changelog_part_35.md`.
- Key decision:
  - Official name: **Omni Neural DataSpec**.
  - `nn/*` layer/model APIs operate on validated data specs, explicit
    parameter trees, explicit state trees, and transparent model bundles.
  - `ml/*` remains the primitive Tensor math namespace.
  - Vulkan inference and training must use explicit Tensor placement,
    fail-closed unsupported paths, and truthful capability bits.
- Validation:
  - Documentation/static validation only for this decision slice.
- Unresolved issues:
  - Runtime `nn/*` implementation has not started.
  - `ML-VK-030` still needs max/average pooling before the initial ConvNet
    inference MVP can be complete.
  - Training depends on `ML-VK-050` autograd and `ML-VK-060` optimizers.
- Next actions:
  - Implement `ML-VK-030` pooling.
  - Then start `ML-VK-070-001` schema normalization and `nn/validate`.
- Signature: Codex GPT-5.4
