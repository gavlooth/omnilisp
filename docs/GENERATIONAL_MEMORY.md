# Generational Memory: IPGE vs Tagged Handles

This document explains two approaches for detecting use-after-free at runtime,
when ASAP compile-time analysis cannot prove safety.

---

## The Problem

ASAP handles ~95% of memory management at compile time. But some cases escape
static analysis:

- FFI boundaries (external C libraries)
- Complex callback patterns
- Closures with unclear lifetimes
- Dynamic data structures

For these cases, we need **runtime detection** of stale references.

---

## CRITICAL: The Fundamental Soundness Requirement

> **You cannot validate a freed object's generation by reading from freed memory.**

Both IPGE and tagged handles read `obj->generation` or `slot->generation` during
validation. If that memory has been returned to `malloc`/`free`, reading it is
**undefined behavior** in C:

```c
Obj* obj = malloc(sizeof(Obj));
obj->generation = 42;
BorrowRef ref = borrow_create(obj);

free(obj);  // Memory returned to allocator

// UNDEFINED BEHAVIOR - reading freed memory!
if (borrow_is_valid(&ref)) {  // reads obj->generation
    // May crash, may read garbage, may "work" by luck
}
```

### The Three Sound Approaches

To make generational validation sound, you must ensure the generation field
**remains readable** after the object is logically freed:

#### 1. Stable Slot Pool (Recommended)

Objects live in pre-allocated slots. On "free", mark slot as available but
don't return memory to malloc. Generation stays readable.

```
┌─────────────────────────────────────────────────────┐
│                    Slot Pool                         │
├─────────┬─────────┬─────────┬─────────┬─────────────┤
│ Slot 0  │ Slot 1  │ Slot 2  │ Slot 3  │    ...      │
│ gen=5   │ gen=12  │ gen=3   │ gen=8   │             │
│ FREE    │ IN_USE  │ FREE    │ IN_USE  │             │
└─────────┴─────────┴─────────┴─────────┴─────────────┘
         ↑                     ↑
    Readable even          Readable even
    when free              when free
```

#### 2. Quarantine Allocator

Freed objects go to a quarantine queue. Only returned to malloc after N more
allocations or T time. Generation readable during quarantine.

```
free(obj) → [Quarantine Queue] → ... wait ... → actual free()
                ↑
          Still readable here
```

#### 3. Indirection Table

Handle contains index into a stable table. Table entry holds generation and
pointer. Table never freed.

```
Handle = { index: 42 }
            ↓
┌─────────────────────────────────────┐
│         Indirection Table            │
├───────┬────────────┬────────────────┤
│ Index │ Generation │ Pointer        │
├───────┼────────────┼────────────────┤
│  42   │    17      │ 0x7fff1234 or NULL │
└───────┴────────────┴────────────────┘
            ↑              ↑
      Always readable   Set to NULL on free
```

**Without one of these, IPGE and tagged handles are fundamentally unsound.**

---

## Approach 1: IPGE (In-Place Generational Evolution)

### Concept

Every object carries a **generation counter**. When freed and reallocated,
the generation changes. References remember the generation at creation time.
On access, compare remembered vs current generation.

**Requires: Stable slot pool or quarantine.**

### Memory Layout

```
┌─────────────────────────────────────────┐
│              Slot (stable)               │
├──────────────┬──────────────────────────┤
│  generation  │  flags  │    payload     │
│   (16 bit)   │  (FREE/ │                │
│              │  IN_USE)│                │
└──────────────┴──────────────────────────┘

┌─────────────────────────────────────────┐
│              BorrowRef                   │
├──────────────┬──────────────┬───────────┤
│   target     │ remembered   │  source   │
│   (Slot*)    │    _gen      │   _desc   │
└──────────────┴──────────────┴───────────┘
```

### Implementation (with Stable Slot Pool)

```c
/*
 * IPGE - In-Place Generational Evolution
 *
 * REQUIREMENT: Slots must remain allocated (pool-based).
 * Do NOT use with raw malloc/free.
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* Configuration: 16-bit (compact) or 64-bit (robust) */
#ifndef IPGE_ROBUST_MODE
#define IPGE_ROBUST_MODE 0
#endif

#if IPGE_ROBUST_MODE
    typedef uint64_t Generation;
    #define IPGE_MULTIPLIER 0x5851f42d4c957f2dULL
    #define IPGE_INCREMENT  0x14057b7ef767814fULL
#else
    typedef uint16_t Generation;
    /*
     * For full period LCG with modulus 2^16, Hull-Dobell theorem requires:
     *   - c is odd (coprime to 2^16)
     *   - a ≡ 1 (mod 4)
     *
     * 0xA001 = 40961, 40961 mod 4 = 1 ✓
     * 0x9E37 = 40503, odd ✓
     */
    #define IPGE_MULTIPLIER 0xA001
    #define IPGE_INCREMENT  0x9E37
#endif

/* Slot flags */
#define SLOT_FREE   0
#define SLOT_IN_USE 1

/* Slot in stable pool */
typedef struct Slot {
    Generation generation;
    uint16_t flags;
    /* Payload follows - size depends on pool configuration */
} Slot;

/* Borrowed reference */
typedef struct BorrowRef {
    Slot* target;
    Generation remembered_gen;
    const char* source_desc;  /* Debug: "loop iterator", "callback arg" */
} BorrowRef;

/*
 * LCG Evolution
 *
 * Linear Congruential Generator.
 *
 * For 16-bit with a=0xA001, c=0x9E37:
 *   Full period of 65,536 values (Hull-Dobell satisfied)
 *
 * For 64-bit with a=0x5851f42d4c957f2d, c=0x14057b7ef767814f:
 *   Full period of 2^64 values
 *
 * NOTE: Full period means every value visited once before repeat.
 * It does NOT prevent ABA - after 65,536 slot reuses (16-bit),
 * a stale handle WILL become valid again.
 */
static inline Generation ipge_evolve(Generation gen) {
    gen = (gen * IPGE_MULTIPLIER) + IPGE_INCREMENT;
    if (gen == 0) gen = 1;  /* Reserve 0 for "never valid" */
    return gen;
}

/*
 * ============== Slot Pool ==============
 *
 * Critical: Slots are never returned to malloc.
 * This makes generation reads always safe.
 */

#define POOL_SIZE 4096

typedef struct SlotPool {
    Slot* slots;           /* Contiguous array of slots */
    size_t slot_size;      /* Size of each slot (header + payload) */
    size_t capacity;       /* Number of slots */
    uint32_t* freelist;    /* Indices of free slots */
    size_t freelist_top;   /* Stack pointer for freelist */
} SlotPool;

/* Per-process random seed for initial generations
 *
 * NOTE: __attribute__((constructor)) works on GCC/Clang.
 * On MSVC, use a different mechanism:
 *   - CRT initializer: #pragma section(".CRT$XCU", read)
 *   - Or call ipge_init_seed() explicitly at startup
 */
static uint64_t g_ipge_seed;

#if defined(__GNUC__) || defined(__clang__)
__attribute__((constructor))
#endif
static void ipge_init_seed(void) {
    g_ipge_seed = (uint64_t)time(NULL);
    g_ipge_seed ^= (uint64_t)(uintptr_t)&g_ipge_seed;
    g_ipge_seed *= 0x5851f42d4c957f2dULL;
}

static inline SlotPool* pool_create(size_t payload_size, size_t capacity) {
    SlotPool* pool = malloc(sizeof(SlotPool));
    pool->slot_size = sizeof(Slot) + payload_size;
    pool->capacity = capacity;
    pool->slots = calloc(capacity, pool->slot_size);
    pool->freelist = malloc(capacity * sizeof(uint32_t));
    pool->freelist_top = capacity;

    /* Initialize all slots with randomized generations
     * Mix per-process seed with slot index for unpredictability.
     * This prevents handle-guessing across process restarts. */
    for (size_t i = 0; i < capacity; i++) {
        Slot* s = (Slot*)((char*)pool->slots + i * pool->slot_size);
        uint64_t mixed = g_ipge_seed ^ (i * 0x9e3779b97f4a7c15ULL);
        s->generation = (Generation)ipge_evolve((Generation)mixed);
        s->flags = SLOT_FREE;
        pool->freelist[i] = (uint32_t)i;
    }
    return pool;
}

static inline Slot* pool_alloc(SlotPool* pool) {
    if (pool->freelist_top == 0) return NULL;  /* Pool exhausted */

    uint32_t idx = pool->freelist[--pool->freelist_top];
    Slot* s = (Slot*)((char*)pool->slots + idx * pool->slot_size);

    /* Evolve generation on allocation */
    s->generation = ipge_evolve(s->generation);
    s->flags = SLOT_IN_USE;
    return s;
}

static inline void pool_free(SlotPool* pool, Slot* s) {
    if (!s || s->flags == SLOT_FREE) return;

    /* Evolve generation on free - invalidates existing refs */
    s->generation = ipge_evolve(s->generation);
    s->flags = SLOT_FREE;

    /* Return to freelist (slot memory stays allocated!) */
    size_t idx = ((char*)s - (char*)pool->slots) / pool->slot_size;
    pool->freelist[pool->freelist_top++] = (uint32_t)idx;
}

/* Get payload pointer from slot */
static inline void* slot_payload(Slot* s) {
    return (void*)(s + 1);
}

/*
 * ============== Borrow Operations ==============
 */

static inline BorrowRef borrow_create(Slot* s, const char* desc) {
    return (BorrowRef){
        .target = s,
        .remembered_gen = s->generation,
        .source_desc = desc
    };
}

/*
 * Validation is SAFE because:
 *   - Slot memory is never freed (pool-based)
 *   - Reading s->generation is always defined behavior
 *   - Even for freed slots, generation is readable
 */
static inline bool borrow_is_valid(BorrowRef* ref) {
    if (!ref->target) return false;
    Slot* s = ref->target;

    /* Safe read - slot memory always allocated */
    if (s->generation != ref->remembered_gen) return false;
    if (s->flags != SLOT_IN_USE) return false;  /* Extra safety */

    return true;
}

static inline void* borrow_get(BorrowRef* ref) {
    if (!borrow_is_valid(ref)) return NULL;
    return slot_payload(ref->target);
}
```

### Performance Analysis

**Create reference:**
```
1. Read s->generation          : 1 load (~3-4 cycles)
2. Store in ref                : 1 store (~1 cycle)
Total: ~4-5 cycles
```

**Validate reference:**
```
1. Read s->generation          : 1 load (~3-4 cycles)
2. Compare                     : 1 compare (~1 cycle)
3. Read s->flags (optional)    : 1 load (~0-4 cycles, likely same cache line)
4. Compare                     : 1 compare (~1 cycle)
Total: ~5-10 cycles
```

**Invalidate (pool_free):**
```
1. LCG computation             : ~4-6 cycles
2. Store generation            : 1 store (~1 cycle)
3. Freelist push               : ~3-4 cycles
Total: ~10 cycles
```

### Collision/ABA Risk

**This is about ABA, not one-time probability.**

With 16-bit generation:
- Full period = 65,536 values
- After 65,536 alloc/free cycles **on the same slot**, generation wraps
- A stale handle created 65,536 slot-reuses ago **will become valid again**
- This is **guaranteed**, not probabilistic

With 64-bit generation:
- Full period = 2^64 values
- Wrap-around after 18 quintillion reuses
- Practically never wraps in any realistic program

**Mitigation:** Use 64-bit generation, or accept 16-bit with understanding
that extremely long-lived stale handles can resurrect.

---

## Approach 2: Tagged Handles (new_genref)

### Concept

The handle (pointer) itself carries a **validation tag** in unused bits.
The tag is computed from pointer + generation + secret using a mixing function.
No separate reference structure needed.

**Requires: Stable slots (same as IPGE).**

### Memory Layout

```
┌─────────────────────────────────────────┐
│              Slot (stable)               │
├──────────────┬──────────────────────────┤
│  generation  │  flags  │    payload     │
│   (32 bit)   │         │                │
└──────────────┴──────────────────────────┘

Handle (single word):
┌────────────────────────────────────────┐
│  tag (8 bits)  │  pointer (56 bits)    │   <- AArch64
└────────────────────────────────────────┘

┌────────────────────────────────────────┐
│  pointer (60 bits)  │  tag (4 bits)    │   <- Portable
└────────────────────────────────────────┘
```

### Implementation (with Stable Slot Pool)

```c
/*
 * Tagged Handles - Portable Generational References
 *
 * REQUIREMENT: Slots must remain allocated (pool-based).
 * Do NOT use with raw malloc/free.
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>

/* Slot in stable pool */
typedef struct Slot {
    uint32_t generation;
    uint32_t flags;
    /* payload follows */
} Slot;

/* Handle is just a tagged pointer */
typedef uintptr_t Handle;

#define HANDLE_INVALID 0

/* Global secret (initialized once at startup)
 *
 * NOTE: __attribute__((constructor)) works on GCC/Clang.
 * On MSVC, call handle_init_secret() explicitly at startup.
 */
static uint64_t g_handle_secret;

#if defined(__GNUC__) || defined(__clang__)
__attribute__((constructor))
#endif
static void handle_init_secret(void) {
    g_handle_secret = (uint64_t)time(NULL);
    g_handle_secret ^= (uint64_t)(uintptr_t)&g_handle_secret;
    g_handle_secret *= 0x5851f42d4c957f2dULL;
}

/*
 * Mixing Function (splitmix64 finalizer)
 *
 * This is a BIJECTIVE function (reversible), not a hash.
 *
 * Purpose: Avalanche effect - small input changes cause large output changes.
 * This spreads bits so sequential (ptr, gen) pairs give scattered tags.
 *
 * NOT cryptographically secure:
 *   - Reversible (attacker can compute input from output if they know function)
 *   - Secret helps but is not a MAC/HMAC
 *   - 8-bit output can be brute-forced in 256 attempts
 *
 * Adequate for: Catching accidental bugs, not adversarial attacks.
 */
static inline uint64_t mix64(uint64_t x) {
    x ^= x >> 30;
    x *= 0xbf58476d1ce4e5b9ULL;
    x ^= x >> 27;
    x *= 0x94d049bb133111ebULL;
    x ^= x >> 31;
    return x;
}

/*
 * Compute validation tag
 */
static inline uint8_t compute_tag(void* ptr, uint32_t gen) {
    uint64_t x = (uint64_t)(uintptr_t)ptr;
    x ^= (uint64_t)gen * 0x9e3779b97f4a7c15ULL;
    x ^= g_handle_secret;
    return (uint8_t)(mix64(x) >> 56);
}

/*
 * Platform-specific encoding
 */

#if defined(__aarch64__)
#define HANDLE_USE_TOP_BYTE 1
#define TAG_SHIFT 56
#define TAG_MASK  0xFF00000000000000ULL
#define PTR_MASK  0x00FFFFFFFFFFFFFFULL
#define TAG_BITS  8
#else
#define HANDLE_USE_TOP_BYTE 0
#define TAG_BITS  4
#define TAG_MASK  0xFULL
#define PTR_MASK  (~TAG_MASK)
#endif

static inline Handle handle_encode(Slot* s, uint8_t tag) {
#if HANDLE_USE_TOP_BYTE
    return ((uintptr_t)s & PTR_MASK) | ((uintptr_t)tag << TAG_SHIFT);
#else
    return ((uintptr_t)s & PTR_MASK) | ((uintptr_t)(tag & TAG_MASK));
#endif
}

static inline Slot* handle_decode_ptr(Handle h) {
    return (Slot*)(h & PTR_MASK);
}

static inline uint8_t handle_decode_tag(Handle h) {
#if HANDLE_USE_TOP_BYTE
    return (uint8_t)(h >> TAG_SHIFT);
#else
    return (uint8_t)(h & TAG_MASK);
#endif
}

/*
 * Handle Operations
 */

static inline Handle handle_create(Slot* s) {
    uint8_t tag = compute_tag(s, s->generation);
    return handle_encode(s, tag);
}

/*
 * Validation reads slot->generation.
 * SAFE because slot memory is never freed (pool-based).
 */
static inline bool handle_is_valid(Handle h) {
    if (h == HANDLE_INVALID) return false;

    Slot* s = handle_decode_ptr(h);
    uint8_t stored_tag = handle_decode_tag(h);
    uint8_t expected_tag = compute_tag(s, s->generation);

#if HANDLE_USE_TOP_BYTE
    return stored_tag == expected_tag && s->flags != 0;
#else
    return (stored_tag & TAG_MASK) == (expected_tag & TAG_MASK) && s->flags != 0;
#endif
}

static inline void* handle_get(Handle h) {
    if (!handle_is_valid(h)) return NULL;
    Slot* s = handle_decode_ptr(h);
    return (void*)(s + 1);
}
```

### Performance Analysis

**Create handle:**
```
1. Read s->generation          : 1 load (~3-4 cycles)
2. XOR, multiply, XOR          : ~3 cycles
3. mix64 (6 ALU ops)           : ~6-8 cycles
4. Shift to extract tag        : 1 cycle
5. Encode in pointer           : ~2 cycles
Total: ~16-20 cycles
```

**Validate handle:**
```
1. Decode pointer              : 1 AND (~1 cycle)
2. Read s->generation          : 1 load (~3-4 cycles)
3. Recompute tag               : ~10-12 cycles
4. Decode stored tag           : 1 op (~1 cycle)
5. Compare + flags check       : ~2 cycles
Total: ~18-22 cycles
```

### Collision/ABA Risk (CRITICAL)

**This is NOT "1/256 probability". It's about tag repetition over reuse.**

The handle stores only TAG_BITS of validation information:
- 4 bits (portable): 16 possible tags
- 8 bits (AArch64): 256 possible tags

When a slot is freed and reallocated, it gets a new generation, which produces
a new tag. But there are only 2^TAG_BITS possible tags.

**Two different collision questions:**

**A) Will a SPECIFIC stale handle become valid again?**

For one old handle with tag T_old, each reuse has 1/2^k chance to match:

```
P(resurrection) ≈ 1 - (1 - 1/2^k)^N ≈ N / 2^k  (for small N)

8-bit tags (k=8):
  N = 17   → ~6.6% chance
  N = 100  → ~33% chance
  N = 256  → ~63% chance
  N = 512  → ~86% chance

4-bit tags (k=4):
  N = 5    → ~31% chance
  N = 16   → ~64% chance
  N = 32   → ~87% chance
```

**B) Will ANY stale handle collide (birthday problem)?**

If you have many stale handles floating around, collision probability
follows birthday paradox: ~50% after √(2^k) handles.

```
8-bit: ~50% collision after ~19 stale handles exist
4-bit: ~50% collision after ~5 stale handles exist
```

**Which matters?** Usually (A) - you care if a specific old handle resurrects.
But if your program leaks many stale handles, (B) becomes relevant.

**Bottom line:** Tagged handles are best-effort bug detection. They catch
most accidental stale-pointer bugs but are NOT robust against:
- Adversarial input (brute force ~256 attempts)
- Very long-lived stale handles
- Programs with many leaked handles

### Security Considerations

`mix64(secret ^ data)` is a **keyed mixing function**, not a cryptographic MAC:

- It spreads bits well (avalanche effect)
- It is NOT a proven PRF/MAC
- For non-adversarial bug-catching: fine
- For adversarial callers: the limiter is small tag width + retries, not the mixing

```
Attacker model              | Protection level
----------------------------|------------------
Accidental stale handle     | Good (catches ~99% of bugs)
Long-lived stale handle     | Poor (eventual collision)
Adversary with retries      | Poor (brute force 256 attempts)
Adversary knows secret      | None (can forge tags)
```

For adversarial FFI, consider:
- Indirection table with 32-bit generation
- Real MAC (SipHash) if you need cryptographic binding
- Rate limiting on handle validation failures

### Platform Considerations

**CRITICAL: You must mask tag bits before dereferencing the pointer.**

```c
// WRONG - may crash or trigger sanitizers
void* data = (void*)handle;

// CORRECT - mask off tag bits first
void* data = (void*)(handle & PTR_MASK);
```

**AArch64 (Top-Byte Ignore):**
- Hardware ignores top byte in address calculation
- Tagged pointers "just work" for dereference
- But sanitizers (ASan, MTE) may still flag unmasked pointers
- Recommendation: always mask anyway for portability

**x86-64 (Canonical Addressing):**
- Top 16 bits must be sign-extension of bit 47
- Top-byte tagging is **unsafe** - will fault on dereference
- Only low-bit tagging (4 bits) is safe on x86-64
- This is why we use `#if defined(__aarch64__)` for top-byte mode

---

## Comparison Summary

| Metric | IPGE (16-bit) | IPGE (64-bit) | Tagged (4-bit) | Tagged (8-bit) |
|--------|---------------|---------------|----------------|----------------|
| **Memory/slot** | 4 bytes | 12 bytes | 8 bytes | 8 bytes |
| **Memory/ref** | 24 bytes | 32 bytes | 8 bytes | 8 bytes |
| **Create cost** | ~5 cycles | ~5 cycles | ~18 cycles | ~18 cycles |
| **Validate cost** | ~8 cycles | ~8 cycles | ~20 cycles | ~20 cycles |
| **Full cycle (IPGE)** | 65,536 | 2^64 | N/A | N/A |
| **Expected repeat*** | N/A | N/A | ~16 | ~256 |
| **50% single-handle†** | 32,768 | 2^63 | ~5 reuses | ~178 reuses |
| **Debug info** | Yes | Yes | No | No |
| **Portability** | Excellent | Excellent | x86-64 safe | AArch64 only |

\* Tagged handles use truncated mixing output, not a permutation. Expect random-like
distribution with repeats scaling ~2^k on average (could repeat sooner by chance).

† "50% single-handle" = probability ONE specific stale handle resurrects after N reuses.

### ABA Risk Comparison (Single Stale Handle)

```
Slot reuses     IPGE-16    IPGE-64    Tag-4    Tag-8
─────────────────────────────────────────────────────
10              Safe       Safe       ~63%     ~4%
100             Safe       Safe       Certain  ~33%
500             Safe       Safe       Certain  ~86%
1,000           Safe       Safe       Certain  ~98%
65,536          ABA!       Safe       Certain  Certain
```

Note: Tag-N columns show probability of ONE specific stale handle resurrecting.
If many stale handles exist, use birthday-problem math instead.

---

## Approach 3: Cached-Tag Handles (Speed Optimized)

### The Problem with Approach 2

Tagged handles recompute `mix64()` on **every validation** (~12 ALU ops).
If you validate more often than you allocate/free, this is wasteful.

### The Solution: Cache the Tag

Compute the tag **once** when generation changes (alloc/free), store it in
the slot header. Validation becomes just loads + compares.

```
Validation cost comparison:
  Approach 2 (recompute): 1 load + 12 ALU + 1 compare = ~18-20 cycles
  Approach 3 (cached):    1 load + 1 compare          = ~5 cycles
```

### Memory Layout

```
┌─────────────────────────────────────────────────────┐
│                    Slot (8 bytes header)             │
├──────────────┬────────┬────────┬───────────────────┤
│  generation  │  tag8  │ flags  │  pad   │ payload  │
│   (32 bit)   │ (8bit) │ (8bit) │ (16bit)│          │
└──────────────┴────────┴────────┴───────────────────┘
                   ↑
            Cached! Computed once on alloc/free
```

### Implementation

```c
/*
 * Cached-Tag Handles - Best of Both Worlds
 *
 * - Single-word handles (like tagged handles)
 * - Fast validation (like IPGE)
 * - Tag computed once on alloc/free, not on every validate
 *
 * REQUIREMENT: Stable slot pool (same as other approaches)
 */

#include <stdint.h>
#include <stdbool.h>
#include <time.h>

typedef uintptr_t Handle;

#define SLOT_FREE   0u
#define SLOT_IN_USE 1u
#define HANDLE_INVALID 0

/* Slot header with CACHED tag */
typedef struct Slot {
    uint32_t generation;   /* Increments on reuse */
    uint8_t  tag8;         /* CACHED tag for current generation */
    uint8_t  flags;        /* FREE / IN_USE */
    uint16_t pad;          /* Pad to 8 bytes (optional) */
    /* payload follows... */
} Slot;

/* Global secret
 * NOTE: On MSVC, call cachedtag_init() explicitly at startup. */
static uint64_t g_cached_secret;

#if defined(__GNUC__) || defined(__clang__)
__attribute__((constructor))
#endif
static void cachedtag_init(void) {
    g_cached_secret = (uint64_t)time(NULL);
    g_cached_secret ^= (uint64_t)(uintptr_t)&g_cached_secret;
    g_cached_secret *= 0x5851f42d4c957f2dULL;
}

/* Mixing function - used ONLY on alloc/free now */
static inline uint64_t mix64(uint64_t x) {
    x ^= x >> 30;
    x *= 0xbf58476d1ce4e5b9ULL;
    x ^= x >> 27;
    x *= 0x94d049bb133111ebULL;
    x ^= x >> 31;
    return x;
}

static inline uint8_t compute_tag8(Slot* s) {
    uint64_t x = (uint64_t)(uintptr_t)s;
    x ^= (uint64_t)s->generation * 0x9e3779b97f4a7c15ULL;
    x ^= g_cached_secret;
    return (uint8_t)(mix64(x) >> 56);
}

/* Platform-specific pointer tagging */
#if defined(__aarch64__)
  #define HANDLE_TOPBYTE 1
  #define TAG_SHIFT 56
  #define TAG_MASK  ((uintptr_t)0xFFu << TAG_SHIFT)
  #define PTR_MASK  (~TAG_MASK)

  static inline Handle handle_make(Slot* s) {
      return ((uintptr_t)s & PTR_MASK) | ((uintptr_t)s->tag8 << TAG_SHIFT);
  }
  static inline Slot* handle_ptr(Handle h) {
      return (Slot*)(h & PTR_MASK);
  }
  static inline uint8_t handle_tag(Handle h) {
      return (uint8_t)(h >> TAG_SHIFT);
  }

#else
  /* Portable: low 4 bits, requires 16-byte alignment */
  #define HANDLE_TOPBYTE 0
  #define TAG_BITS 4
  #define TAG_MASK ((uintptr_t)((1u << TAG_BITS) - 1u))
  #define PTR_MASK (~TAG_MASK)

  static inline Handle handle_make(Slot* s) {
      return ((uintptr_t)s & PTR_MASK) | ((uintptr_t)(s->tag8 & TAG_MASK));
  }
  static inline Slot* handle_ptr(Handle h) {
      return (Slot*)(h & PTR_MASK);
  }
  static inline uint8_t handle_tag(Handle h) {
      return (uint8_t)(h & TAG_MASK);
  }
#endif

/*
 * Lifecycle: compute tag ONCE on generation change
 * This is the "slow path" - happens only on alloc/free
 */
static inline void slot_on_allocate(Slot* s) {
    s->generation++;
    if (s->generation == 0) s->generation = 1;
    s->tag8 = compute_tag8(s);  /* Compute and CACHE */
    s->flags = SLOT_IN_USE;
}

static inline void slot_on_free(Slot* s) {
    s->generation++;
    if (s->generation == 0) s->generation = 1;
    s->tag8 = compute_tag8(s);  /* Compute and CACHE */
    s->flags = SLOT_FREE;
}

/*
 * FAST validation: NO mixing, just loads + compares
 * This is the hot path - happens on every handle access
 */
static inline bool handle_is_valid(Handle h) {
    if (h == HANDLE_INVALID) return false;

    Slot* s = handle_ptr(h);
    if (!s) return false;

    /* Tag compare: just load + compare, no computation */
#if HANDLE_TOPBYTE
    if (handle_tag(h) != s->tag8) return false;
#else
    if ((handle_tag(h) & TAG_MASK) != (s->tag8 & TAG_MASK)) return false;
#endif

    /* Optional: also check flags for extra safety */
    if (s->flags != SLOT_IN_USE) return false;

    return true;
}

static inline void* handle_get(Handle h) {
    if (!handle_is_valid(h)) return NULL;
    return (void*)(handle_ptr(h) + 1);  /* Payload after header */
}
```

### Performance Analysis

**Create handle (just embed cached tag):**
```
1. Read s->tag8                : 1 load (~3 cycles)
2. Encode in pointer           : 1-2 ops (~2 cycles)
Total: ~5 cycles
```

**Validate handle (HOT PATH - now fast!):**
```
1. Mask pointer                : 1 AND (~1 cycle)
2. Load s->tag8                : 1 load (~3-4 cycles)
3. Compare with handle tag     : 1 compare (~1 cycle)
4. Load s->flags (same cache line): ~0 cycles (coalesced)
5. Compare                     : 1 compare (~1 cycle)
Total: ~6-8 cycles (vs ~18-20 before!)
```

**Allocate/Free (SLOW PATH - now pays the cost):**
```
1. Increment generation        : 1 add (~1 cycle)
2. compute_tag8 (mix64)        : ~12 cycles
3. Store tag8                  : 1 store (~1 cycle)
4. Store flags                 : 1 store (~1 cycle)
Total: ~15-18 cycles
```

### Why This Is The Best Trade-off

| Operation | Approach 2 (recompute) | Approach 3 (cached) |
|-----------|------------------------|---------------------|
| Validate  | ~18-20 cycles          | **~6-8 cycles**     |
| Alloc     | ~5 cycles              | ~15-18 cycles       |
| Free      | ~5 cycles              | ~15-18 cycles       |

If `validate >> alloc + free` (true for most programs), this is a big win.

**Example:** 1000 validates per alloc/free cycle:
- Approach 2: 1000 × 20 + 2 × 5 = 20,010 cycles
- Approach 3: 1000 × 7 + 2 × 17 = 7,034 cycles (**2.8× faster**)

---

## Approach 4: Indirection Table (Robust FFI)

For truly robust FFI handles that survive adversarial input:

```c
/*
 * Indirection Table Handles
 *
 * Handle is index + generation, not pointer.
 * Table never freed, survives all object lifecycles.
 * No pointer in handle = no pointer forgery.
 */

typedef struct TableEntry {
    uint32_t generation;
    void* ptr;              /* NULL if freed */
} TableEntry;

typedef struct HandleTable {
    TableEntry* entries;
    size_t capacity;
    uint32_t* freelist;
    size_t freelist_top;
} HandleTable;

typedef struct Handle {
    uint32_t index;
    uint32_t generation;
} Handle;

static inline Handle table_alloc(HandleTable* t, void* ptr) {
    uint32_t idx = t->freelist[--t->freelist_top];
    TableEntry* e = &t->entries[idx];
    e->generation++;
    e->ptr = ptr;
    return (Handle){ .index = idx, .generation = e->generation };
}

static inline void table_free(HandleTable* t, Handle h) {
    TableEntry* e = &t->entries[h.index];
    if (e->generation != h.generation) return;  /* Already freed */
    e->generation++;  /* Invalidate */
    e->ptr = NULL;
    t->freelist[t->freelist_top++] = h.index;
}

static inline void* table_get(HandleTable* t, Handle h) {
    TableEntry* e = &t->entries[h.index];
    if (e->generation != h.generation) return NULL;
    return e->ptr;
}
```

**Properties:**
- Handle is 8 bytes (index + generation), not a pointer
- 32-bit generation = 4 billion reuses before ABA
- Table survives object frees (always safe to read)
- No pointer in handle = can't forge valid pointers
- Slight indirection cost (~1 extra memory access)

---

## Recommendations

### For Internal Runtime (IPGE)

Use IPGE with 64-bit generation and stable slot pools:
- Fast validation (~8 cycles)
- Debug info (source_desc)
- No ABA in practice (2^64 reuses)
- Simple implementation

### For FFI (Tagged Handles or Indirection)

Choose based on threat model:

| Threat | Solution |
|--------|----------|
| Accidental bugs only | Tagged handles (8-bit) |
| Untrusted caller | Indirection table (32-bit gen) |
| Adversarial input | Indirection + rate limiting |

### Do NOT Use With malloc/free

Neither IPGE nor tagged handles work with raw malloc/free.
**You must use a pool allocator that keeps slot headers alive.**

---

## CPU Cycle Reference

| Operation | Cycles (approx) |
|-----------|-----------------|
| Integer ADD/XOR/AND | 1 |
| Integer MULTIPLY | 3-4 |
| Memory LOAD (L1 hit) | 3-4 |
| Memory LOAD (L2 hit) | 10-12 |
| Memory LOAD (L3 hit) | 30-40 |
| Memory LOAD (RAM) | 100-300 |

**Key insight:** Memory loads dominate. If the slot is in L1, validation
is ~8-20 cycles. If it's in RAM, validation is ~100-300 cycles regardless
of which approach you use.
