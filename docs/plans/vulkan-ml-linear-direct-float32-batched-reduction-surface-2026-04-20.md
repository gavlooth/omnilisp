# Vulkan ML batched reduction surface decision

Date: 2026-04-20
Status: accepted decision

## Context

The Vulkan ML roadmap keeps the broad `ml-linear` backend capability false.
A narrow partial capability, `ml-linear-direct-float32`, is allowed for direct
concrete Vulkan `Float32` `ml/linear` only. `ML-VK-010-004` needs a public
batched-reduction surface decision before reducer coverage is implemented.

## Candidate public names

- `ml/linear/reduce`
- `ml/linear/batched-reduce`
- `ml/linear/reduce-batch`
- `ml/linear/row-reduce`

## Decision

Use `ml/linear/batched-reduce` as the public surface for the first concrete
Vulkan `Float32` batched reduction lane.

Reasoning:

- it names the operation family directly without implying a general-purpose
  reduction API beyond this lane;
- it keeps the batched aspect visible, which matches the current ML backlog
  boundary;
- it fits the existing `ml/linear` public naming style without inventing a
  backend-flavored API.

## Decision impact

- keep `ml-linear` false for Vulkan;
- keep `ml-linear-direct-float32` as the only narrow direct-concrete Vulkan
  `Float32` capability name for the current lane;
- implement reducer coverage only after the public batched-reduction surface is
  frozen and reflected in the roadmap.
