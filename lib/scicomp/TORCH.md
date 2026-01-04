# SciComp.Torch - LibTorch Deep Learning Bindings

GPU-accelerated tensor operations and neural networks via LibTorch (PyTorch C++ frontend).

## Table of Contents

1. [Overview](#overview)
2. [Installation](#installation)
3. [Quick Start](#quick-start)
4. [Tensor Creation](#tensor-creation)
5. [Tensor Operations](#tensor-operations)
6. [Shape Manipulation](#shape-manipulation)
7. [Autograd](#autograd)
8. [Neural Networks](#neural-networks)
9. [Loss Functions](#loss-functions)
10. [Optimizers](#optimizers)
11. [Device Management](#device-management)
12. [Serialization](#serialization)
13. [FFI Reference](#ffi-reference)

---

## Overview

SciComp.Torch provides OmniLisp bindings to LibTorch, enabling:

- **GPU-accelerated tensor computation** via CUDA/MPS
- **Automatic differentiation** for gradient-based optimization
- **Neural network layers** (Linear, Conv2d, LSTM, etc.)
- **Optimizers** (SGD, Adam, AdamW, RMSprop)
- **Loss functions** (MSE, CrossEntropy, BCE, etc.)

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     OmniLisp Code                           │
│         (randn '(3 4)), (t+ a b), (backward! loss)          │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                  torch.omni (FFI Bindings)                  │
│    High-level wrappers + Low-level extern declarations      │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                 libomnitorch.so (C API)                     │
│         libtorch_c_api.cpp - C wrapper for LibTorch         │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    LibTorch (C++ API)                       │
│              PyTorch's C++ frontend library                 │
└─────────────────────────────────────────────────────────────┘
```

---

## Installation

### Prerequisites

1. **LibTorch** - Download from [pytorch.org](https://pytorch.org/get-started/locally/)
2. **C++ compiler** with C++17 support (GCC 9+, Clang 10+)

### Building the C API

```bash
cd lib/scicomp/native
make
```

This produces `libomnitorch.so` (Linux) or `libomnitorch.dylib` (macOS).

### Environment Setup

```bash
export LD_LIBRARY_PATH=/path/to/libtorch/lib:$LD_LIBRARY_PATH
export LIBRARY_PATH=/path/to/libtorch/lib:$LIBRARY_PATH
```

---

## Quick Start

```scheme
(import SciComp.Torch)

; Create tensors
(define x (randn '(3 4)))    ; 3x4 random normal tensor
(define y (randn '(4 5)))    ; 4x5 random normal tensor

; Matrix multiplication
(define z (t@ x y))          ; Result: 3x5 tensor

; Element-wise operations
(define a (ones '(2 3)))
(define b (t+ a (t*s a 2.0))) ; a + 2*a = 3*a

; Neural network training
(define model (nn-linear 784 10))
(define opt (adam (parameters model) 0.001))

(define (train-step input target)
  (optimizer-zero-grad! opt)
  (let* ((output (forward model input))
         (loss (cross-entropy-loss output target)))
    (backward! loss)
    (step! opt)
    loss))
```

---

## Tensor Creation

### Basic Constructors

| Function | Description | Example |
|----------|-------------|---------|
| `(zeros shape [dtype])` | All zeros | `(zeros '(3 4))` |
| `(ones shape [dtype])` | All ones | `(ones '(2 2) Float64)` |
| `(empty shape [dtype device])` | Uninitialized | `(empty '(5 5) Float32 CUDA)` |
| `(eye n [dtype])` | Identity matrix | `(eye 4)` |
| `(rand shape)` | Uniform [0, 1) | `(rand '(10 10))` |
| `(randn shape)` | Normal (0, 1) | `(randn '(3 4))` |
| `(arange start end [step dtype])` | Range | `(arange 0 10 0.5)` |
| `(linspace start end steps [dtype])` | Linear spacing | `(linspace 0 1 100)` |
| `(clone t)` | Deep copy | `(clone x)` |

### Data Types

| Constant | Description |
|----------|-------------|
| `Float16` | 16-bit float |
| `Float32` | 32-bit float (default) |
| `Float64` | 64-bit double |
| `BFloat16` | Brain float 16 |
| `Int8` | 8-bit signed integer |
| `Int16` | 16-bit signed integer |
| `Int32` | 32-bit signed integer |
| `Int64` | 64-bit signed integer |
| `UInt8` | 8-bit unsigned integer |
| `Bool` | Boolean |

### Device Constants

| Constant | Description |
|----------|-------------|
| `CPU` | CPU device |
| `CUDA` | NVIDIA GPU |
| `MPS` | Apple Metal (M1/M2) |
| `XPU` | Intel GPU |

---

## Tensor Operations

### Arithmetic Operations

```scheme
; Element-wise binary operations
(t+ a b)        ; Addition
(t- a b)        ; Subtraction
(t* a b)        ; Multiplication
(t/ a b)        ; Division

; Scalar operations
(t+s t 5.0)     ; Add scalar
(t*s t 2.0)     ; Multiply by scalar
(t-pow t 2.0)   ; Power

; Matrix operations
(t@ a b)        ; Matrix multiplication (general)
(mm a b)        ; 2D matrix multiply
(bmm a b)       ; Batched matrix multiply
(mv mat vec)    ; Matrix-vector multiply
(dot a b)       ; Dot product
```

### Reduction Operations

```scheme
(t-sum t)       ; Sum all elements
(t-mean t)      ; Mean of all elements
(t-std t)       ; Standard deviation
(t-var t)       ; Variance
(t-max t)       ; Maximum value
(t-min t)       ; Minimum value
(t-argmax t)    ; Index of maximum
(t-argmin t)    ; Index of minimum
(t-norm t)      ; L2 norm

; Dimension-wise reductions
(t-sum-dim t dim [keepdim])   ; Sum along dimension
(t-mean-dim t dim [keepdim])  ; Mean along dimension
(t-cumsum t dim)              ; Cumulative sum
```

### Unary Operations

```scheme
; Basic math
(t-neg t)       ; Negation
(t-abs t)       ; Absolute value
(t-sqrt t)      ; Square root
(t-exp t)       ; Exponential
(t-log t)       ; Natural logarithm

; Trigonometric
(t-sin t)       ; Sine
(t-cos t)       ; Cosine
(t-tanh t)      ; Hyperbolic tangent

; Activations
(t-sigmoid t)   ; Sigmoid
(t-relu t)      ; ReLU
(t-gelu t)      ; GELU
(t-silu t)      ; SiLU/Swish
(t-softmax t dim)     ; Softmax
(t-log-softmax t dim) ; Log softmax

; Clamping
(t-clamp t min max)   ; Clamp values to range
```

### Tensor Properties

```scheme
(shape t)           ; Get shape as list: '(3 4 5)
(size t dim)        ; Size of specific dimension
(numel t)           ; Total number of elements
(item t)            ; Extract scalar from 0-d tensor
(t-get-1d t i)      ; Get element from 1D tensor
(t-get-2d t i j)    ; Get element from 2D tensor
(requires-grad? t)  ; Check if gradient tracking enabled
```

---

## Shape Manipulation

```scheme
(reshape t new-shape)      ; Reshape tensor
(view t new-shape)         ; View with new shape (must be contiguous)
(transpose t dim0 dim1)    ; Swap two dimensions
(squeeze t)                ; Remove size-1 dimensions
(unsqueeze t dim)          ; Add dimension at position
(flatten t)                ; Flatten to 1D
(t-contiguous t)           ; Make contiguous in memory
(t-T t)                    ; 2D transpose shorthand
```

### Examples

```scheme
(define x (randn '(2 3 4)))

; Reshape
(reshape x '(6 4))         ; -> (6, 4)
(reshape x '(2 12))        ; -> (2, 12)

; Transpose
(transpose x 0 2)          ; -> (4, 3, 2)

; Squeeze/Unsqueeze
(define y (randn '(1 3 1 4)))
(shape (squeeze y))        ; -> (3, 4)
(shape (unsqueeze y 0))    ; -> (1, 1, 3, 1, 4)
```

---

## Autograd

OmniLisp Torch supports automatic differentiation for gradient computation.

### Gradient Tracking

```scheme
; Enable gradient tracking
(requires-grad! t)

; Disable gradient tracking
(no-grad! t)

; Detach from computation graph
(detach t)

; Check gradient status
(requires-grad? t)
```

### Backward Pass

```scheme
; Compute gradients
(backward! loss)

; Access gradient
(grad t)

; Zero gradients (before next backward pass)
(zero-grad! t)
```

### No-Grad Context

```scheme
; Disable gradient computation temporarily
(with-no-grad
  (let ((output (forward model input)))
    ; No gradients computed here
    output))

; Inference mode (even more optimized)
(with-inference-mode
  (let ((predictions (forward model test-data)))
    predictions))
```

### Example: Manual Gradient Descent

```scheme
; Simple gradient descent
(define x (requires-grad! (randn '(10))))
(define target (randn '(10)))

(let loop ((i 0))
  (when (< i 100)
    ; Forward pass
    (let ((loss (t-sum (t-pow (t- x target) 2.0))))
      ; Backward pass
      (backward! loss)
      ; Update (manual SGD)
      (let ((grad-val (grad x)))
        ; x = x - lr * grad
        ; (In practice, use optimizers instead)
        (loop (+ i 1))))))
```

---

## Neural Networks

### Layer Types

#### Linear Layer

```scheme
(nn-linear in-features out-features [bias])
; bias: #t (default) or #f

(define fc (nn-linear 784 256))
(define output (forward fc input))  ; input: (batch, 784) -> (batch, 256)
```

#### Convolutional Layers

```scheme
(nn-conv1d in-ch out-ch kernel [stride padding])
(nn-conv2d in-ch out-ch kernel [stride padding])

(define conv (nn-conv2d 3 64 3 1 1))  ; 3->64 channels, 3x3 kernel
(define output (forward conv input))   ; input: (N, 3, H, W)
```

#### Normalization

```scheme
(nn-batch-norm1d num-features)
(nn-batch-norm2d num-features)

(define bn (nn-batch-norm2d 64))
```

#### Pooling

```scheme
(nn-max-pool1d kernel [stride])
(nn-max-pool2d kernel [stride])

(define pool (nn-max-pool2d 2))  ; 2x2 max pooling
```

#### Recurrent

```scheme
(nn-lstm input-size hidden-size num-layers)
(nn-gru input-size hidden-size num-layers)
(nn-rnn input-size hidden-size num-layers)

(define lstm (nn-lstm 128 256 2))  ; 2-layer LSTM
```

#### Other

```scheme
(nn-dropout p)           ; Dropout with probability p
(nn-embedding num vocab-size embedding-dim)
```

### Module Operations

```scheme
; Forward pass
(forward module input)

; Training/Eval mode
(train-mode! module)
(eval-mode! module)

; Get parameters for optimizer
(parameters module)  ; Returns list of parameter tensors
```

### Example: Simple Classifier

```scheme
(define (make-classifier input-dim hidden-dim output-dim)
  (let ((fc1 (nn-linear input-dim hidden-dim))
        (fc2 (nn-linear hidden-dim output-dim)))
    (lambda (x)
      (let* ((h (forward fc1 x))
             (h (t-relu h))
             (out (forward fc2 h)))
        out))))

(define model (make-classifier 784 256 10))
(define output (model input))
```

---

## Loss Functions

| Function | Description | Use Case |
|----------|-------------|----------|
| `(mse-loss input target)` | Mean Squared Error | Regression |
| `(l1-loss input target)` | Mean Absolute Error | Robust regression |
| `(cross-entropy-loss input target)` | Cross Entropy | Classification |
| `(binary-cross-entropy input target)` | Binary Cross Entropy | Binary classification |

### Examples

```scheme
; Regression
(define loss (mse-loss predictions targets))

; Classification (logits, class indices)
(define loss (cross-entropy-loss logits labels))

; Binary classification (probabilities after sigmoid)
(define probs (t-sigmoid logits))
(define loss (binary-cross-entropy probs targets))
```

---

## Optimizers

### Creating Optimizers

```scheme
; SGD with momentum
(sgd params lr [momentum weight-decay])
(define opt (sgd (parameters model) 0.01 0.9))

; Adam
(adam params lr [beta1 beta2 eps])
(define opt (adam (parameters model) 0.001))

; AdamW (Adam with decoupled weight decay)
(adamw params lr [beta1 beta2 eps weight-decay])
(define opt (adamw (parameters model) 0.001 0.9 0.999 1e-8 0.01))

; RMSprop
(rmsprop params lr [alpha eps momentum])
(define opt (rmsprop (parameters model) 0.01))
```

### Optimizer Operations

```scheme
; Zero gradients before backward pass
(optimizer-zero-grad! opt)

; Update parameters after backward pass
(step! opt)

; Learning rate scheduling
(set-lr! opt new-lr)
(get-lr opt)
```

### Training Loop Example

```scheme
(define (train model optimizer train-data epochs)
  (train-mode! model)
  (let loop ((epoch 0))
    (when (< epoch epochs)
      (for-each
        (lambda (batch)
          (let ((input (car batch))
                (target (cdr batch)))
            ; Zero gradients
            (optimizer-zero-grad! optimizer)
            ; Forward pass
            (let* ((output (forward model input))
                   (loss (cross-entropy-loss output target)))
              ; Backward pass
              (backward! loss)
              ; Update weights
              (step! optimizer))))
        train-data)
      (loop (+ epoch 1)))))
```

---

## Device Management

### Device Availability

```scheme
(cuda-available?)      ; Check CUDA availability
(mps-available?)       ; Check MPS availability (Apple Silicon)
(cuda-device-count)    ; Number of CUDA devices
```

### Moving Tensors

```scheme
(to-cuda t)           ; Move to CUDA
(to-cpu t)            ; Move to CPU
(to-mps t)            ; Move to MPS
(to-dtype t dtype)    ; Convert data type
```

### CUDA Operations

```scheme
(cuda-synchronize)              ; Wait for CUDA operations
(cuda-empty-cache)              ; Free cached memory
(cuda-memory-allocated [device]) ; Memory in use (bytes)
(cuda-memory-reserved [device])  ; Memory reserved (bytes)
```

### Example: GPU Training

```scheme
(when (cuda-available?)
  (let ((model (to-cuda (nn-linear 784 10)))
        (input (to-cuda (randn '(32 784))))
        (target (to-cuda (randn '(32 10)))))
    (let* ((output (forward model input))
           (loss (mse-loss output target)))
      (backward! loss))))
```

---

## Serialization

### Saving and Loading Tensors

```scheme
; Save tensor to file
(save-tensor! tensor "weights.pt")

; Load tensor from file
(define loaded (load-tensor "weights.pt"))
```

### Saving and Loading Models

```scheme
; Save model (TorchScript)
(save-module! model "model.pt")

; Load model
(define loaded-model (load-module "model.pt"))
```

---

## FFI Reference

### Type System

The FFI uses OmniLisp's type annotation syntax:

```scheme
; Type annotation on binding
[param {Type}]

; Type construction
(Option (Handle Tensor))
(CPtr CInt64)
```

### FFI Declaration Syntax

```scheme
(define (extern library/function-name [:null-on-error])
  [param1 {Type1}]
  [param2 {Type2}]
  ReturnType)
```

### Handle Types

| Type | Description |
|------|-------------|
| `(Handle Tensor)` | Tensor handle (ref-counted) |
| `(Handle Module)` | Neural network module handle |
| `(Handle Optimizer)` | Optimizer handle |

### Options

- `:null-on-error` - Return `None` on error instead of crashing
- `^:consumed` - Parameter ownership is transferred (freed by callee)

### Example FFI Declaration

```scheme
(define (extern torch/torch_add :null-on-error)
  [a {(Handle Tensor)}]
  [b {(Handle Tensor)}]
  (Option (Handle Tensor)))
```

---

## Random Number Generation

```scheme
(manual-seed seed)           ; Set CPU RNG seed
(cuda-manual-seed seed)      ; Set current CUDA device seed
(cuda-manual-seed-all seed)  ; Set all CUDA devices' seeds
```

### Reproducibility

```scheme
; For reproducible results
(manual-seed 42)
(when (cuda-available?)
  (cuda-manual-seed-all 42))

(define x (randn '(3 4)))  ; Same every time
```

---

## Utility Functions

```scheme
(torch-version)     ; Get LibTorch version string
(tensor-print t)    ; Print tensor to stdout (debugging)
```

---

## Complete Training Example

```scheme
(import SciComp.Torch)

; Set seed for reproducibility
(manual-seed 42)

; Create model
(define model (nn-linear 784 10))

; Create optimizer
(define optimizer (adam (parameters model) 0.001))

; Training function
(define (train-epoch model optimizer data)
  (train-mode! model)
  (let ((total-loss 0.0)
        (num-batches 0))
    (for-each
      (lambda (batch)
        (let ((input (car batch))
              (target (cdr batch)))
          (optimizer-zero-grad! optimizer)
          (let* ((output (forward model input))
                 (loss (cross-entropy-loss output target)))
            (backward! loss)
            (step! optimizer)
            (set! total-loss (+ total-loss (item loss)))
            (set! num-batches (+ num-batches 1)))))
      data)
    (/ total-loss num-batches)))

; Evaluation function
(define (evaluate model data)
  (eval-mode! model)
  (with-no-grad
    (let ((correct 0)
          (total 0))
      (for-each
        (lambda (batch)
          (let* ((input (car batch))
                 (target (cdr batch))
                 (output (forward model input))
                 (pred (t-argmax output)))
            ; Count correct predictions
            ; ...
            ))
        data)
      (/ correct total))))

; Run training
(let loop ((epoch 0))
  (when (< epoch 10)
    (let ((loss (train-epoch model optimizer train-data)))
      (display "Epoch ") (display epoch)
      (display " Loss: ") (display loss)
      (newline))
    (loop (+ epoch 1))))

; Save trained model
(save-module! model "trained_model.pt")
```

---

## API Coverage

### Tensor Operations (~150 functions)

- Creation: empty, zeros, ones, full, eye, arange, linspace, logspace, rand, randn, randint
- Arithmetic: add, sub, mul, div, matmul, mm, bmm, mv, dot, outer, fmod, remainder, pow
- Reductions: sum, prod, mean, std, var, max, min, argmax, argmin, norm, cumsum, cumprod, all, any
- Unary: neg, abs, sqrt, exp, log, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh, sigmoid, relu, gelu, silu, softmax, log_softmax, sign, ceil, floor, round, trunc, frac, reciprocal, rsqrt, square, exp2, expm1, log2, log10, log1p, relu6, leaky_relu, elu, selu, mish, softplus, softsign
- Shape: reshape, view, transpose, permute, squeeze, unsqueeze, flatten, contiguous, t, cat, stack, hstack, vstack, dstack, split, chunk, expand, repeat
- Comparison: eq, ne, lt, le, gt, ge, where, isnan, isinf, isfinite
- Indexing: index_select, gather, slice, narrow, masked_select, take, select, index_put_, scatter_, masked_fill_
- Linear Algebra: inverse, pinverse, det, logdet, trace, diag, diagonal, tril, triu, cholesky, solve, lstsq, triangular_solve

### Neural Network Layers (~20 types)

- Linear, Bilinear
- Conv1d, Conv2d
- BatchNorm1d, BatchNorm2d, LayerNorm
- Dropout, Dropout2d
- MaxPool1d, MaxPool2d, AvgPool1d, AvgPool2d
- AdaptiveAvgPool1d, AdaptiveAvgPool2d
- LSTM, GRU, RNN
- Embedding

### Loss Functions (9 types)

- MSE, L1, Smooth L1, Huber
- Cross Entropy, NLL, Binary Cross Entropy, BCE with Logits, KL Divergence

### Optimizers (4 types)

- SGD, Adam, AdamW, RMSprop
