/**
 * Stack helpers for Omni Lisp stack engine.
 *
 * Provides:
 *   D1: FPU state save/restore (stmxcsr/ldmxcsr/fnstcw/fldcw)
 *   D2: Stack overflow detection via SIGSEGV + sigaltstack
 *
 * Called from C3 via extern declarations in stack_engine.c3.
 */

#define _GNU_SOURCE
#include <signal.h>
#include <setjmp.h>
#include <stdint.h>
#include <stddef.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* ============================================================
 * AddressSanitizer integration (fiber stack switching)
 * ============================================================ */

#if defined(__has_feature)
#  if __has_feature(address_sanitizer)
#    define OMNI_WITH_ASAN 1
#  endif
#endif
#if defined(__SANITIZE_ADDRESS__)
#  define OMNI_WITH_ASAN 1
#endif
#if !defined(OMNI_WITH_ASAN)
#  define OMNI_WITH_ASAN 0
#endif

#if defined(__clang__)
#  define OMNI_NO_ASAN __attribute__((no_sanitize("address")))
#elif defined(__GNUC__)
#  define OMNI_NO_ASAN __attribute__((no_sanitize_address))
#else
#  define OMNI_NO_ASAN
#endif

extern void __asan_init(void) __attribute__((weak));
extern void __sanitizer_start_switch_fiber(void **fake_stack_save,
                                           const void *bottom, size_t size)
    __attribute__((weak));
extern void __sanitizer_finish_switch_fiber(void *fake_stack_save,
                                            const void **bottom_old, size_t *size_old)
    __attribute__((weak));

int stack_asan_enabled(void) {
    if (OMNI_WITH_ASAN) return 1;
    if (__asan_init != NULL) return 1;
    if (__sanitizer_start_switch_fiber != NULL) return 1;
    if (__sanitizer_finish_switch_fiber != NULL) return 1;
    return 0;
}

void* stack_current_sp(void) {
    void* sp = NULL;
#if defined(__x86_64__)
    __asm__ volatile("mov %%rsp, %0" : "=r" (sp));
#elif defined(__aarch64__)
    __asm__ volatile("mov %0, sp" : "=r" (sp));
#else
    volatile unsigned char marker = 0;
    sp = (void*)&marker;
#endif
    return sp;
}

static __thread uintptr_t g_cached_thread_stack_lo = 0;
static __thread int g_cached_thread_stack_ready = 0;

static int stack_cache_current_thread_bounds(void) {
    pthread_attr_t attr;
    if (pthread_getattr_np(pthread_self(), &attr) != 0) return -1;

    void* base = NULL;
    size_t size = 0;
    size_t guard = 0;
    int rc = pthread_attr_getstack(&attr, &base, &size);
    if (rc == 0) rc = pthread_attr_getguardsize(&attr, &guard);
    pthread_attr_destroy(&attr);
    if (rc != 0 || base == NULL || size <= guard) return -1;

    g_cached_thread_stack_lo = (uintptr_t)base + guard;
    g_cached_thread_stack_ready = 1;
    return 0;
}

size_t stack_current_thread_available_bytes_from_sp(void* sp) {
    if (!g_cached_thread_stack_ready && stack_cache_current_thread_bounds() != 0) {
        return 0;
    }

    uintptr_t cur = (uintptr_t)sp;
    return cur > g_cached_thread_stack_lo ? cur - g_cached_thread_stack_lo : 0;
}

size_t stack_current_thread_available_bytes(void) {
    return stack_current_thread_available_bytes_from_sp(stack_current_sp());
}

/* ============================================================
 * SIGINT routing helpers (signal-safe REPL interrupt path)
 * ============================================================ */

static __thread volatile sig_atomic_t g_sigint_thread_pending = 0;
static __thread volatile sig_atomic_t* g_sigint_slot = NULL;
static __thread int g_sigint_handler_installed = 0;

static void omni_sigint_handler(int sig) {
    (void)sig;
    if (g_sigint_slot != NULL) {
        *g_sigint_slot = 1;
        return;
    }
    g_sigint_thread_pending = 1;
}

void omni_sigint_install_handler(void) {
    if (g_sigint_handler_installed) return;
    signal(SIGINT, omni_sigint_handler);
    g_sigint_handler_installed = 1;
}

void omni_sigint_bind_slot(volatile sig_atomic_t* slot) {
    g_sigint_slot = slot;
    if (g_sigint_slot != NULL && g_sigint_thread_pending) {
        *g_sigint_slot = 1;
        g_sigint_thread_pending = 0;
    }
}

void omni_sigint_unbind_slot(volatile sig_atomic_t* slot) {
    if (g_sigint_slot == slot) g_sigint_slot = NULL;
}

int omni_sigint_slot_pending(volatile sig_atomic_t* slot) {
    if (slot == NULL) return 0;
    if (*slot) return 1;
    if (g_sigint_thread_pending && g_sigint_slot == slot) {
        *slot = 1;
        g_sigint_thread_pending = 0;
        return 1;
    }
    return 0;
}

void omni_sigint_slot_clear(volatile sig_atomic_t* slot) {
    if (slot != NULL) *slot = 0;
    g_sigint_thread_pending = 0;
}

void omni_sigint_debug_mark_pending(void) {
    omni_sigint_handler(SIGINT);
}

/* ============================================================
 * D1: FPU State Save/Restore
 * ============================================================
 * C3 inline asm doesn't support stmxcsr/ldmxcsr/fnstcw/fldcw.
 */

void fpu_save(uint32_t* mxcsr, uint32_t* x87cw) {
#if defined(__x86_64__)
    uint32_t m;
    uint16_t c;
    __asm__ volatile("stmxcsr %0" : "=m" (m));
    __asm__ volatile("fnstcw %0" : "=m" (c));
    *mxcsr = m;
    *x87cw = (uint32_t)c;
#else
    if (mxcsr != NULL) *mxcsr = 0;
    if (x87cw != NULL) *x87cw = 0;
#endif
}

void fpu_restore(uint32_t mxcsr, uint32_t x87cw) {
#if defined(__x86_64__)
    uint16_t c = (uint16_t)x87cw;
    __asm__ volatile("ldmxcsr %0" : : "m" (mxcsr));
    __asm__ volatile("fldcw %0" : : "m" (c));
#else
    (void)mxcsr;
    (void)x87cw;
#endif
}

int stack_backend_has_aarch64(void) {
#if defined(__aarch64__)
    return 1;
#else
    return 0;
#endif
}

/* Shared C mirror of src/stack_engine_abi_switch.c3 StackContext layout.
 * Keep offsets in lock-step with the C3 declaration.
 */
struct omni_stack_context_layout {
    uint64_t rbx;
    uint64_t rbp;
    uint64_t r12;
    uint64_t r13;
    uint64_t r14;
    uint64_t r15;
    uint64_t rsp;
    uint64_t rip;
    uint32_t mxcsr;
    uint32_t x87cw;
    uint64_t x24;
    uint64_t x25;
    uint64_t x26;
    uint64_t x27;
    uint64_t x28;
    uint64_t lr;
    uint64_t x18;
    uint64_t q8_lo;
    uint64_t q8_hi;
    uint64_t q9_lo;
    uint64_t q9_hi;
    uint64_t q10_lo;
    uint64_t q10_hi;
    uint64_t q11_lo;
    uint64_t q11_hi;
    uint64_t q12_lo;
    uint64_t q12_hi;
    uint64_t q13_lo;
    uint64_t q13_hi;
    uint64_t q14_lo;
    uint64_t q14_hi;
    uint64_t q15_lo;
    uint64_t q15_hi;
    uint32_t fpcr;
    uint32_t fpsr;
};

_Static_assert(offsetof(struct omni_stack_context_layout, rbx) == 0, "StackContext.rbx offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, rbp) == 8, "StackContext.rbp offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, r12) == 16, "StackContext.r12 offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, r13) == 24, "StackContext.r13 offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, r14) == 32, "StackContext.r14 offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, r15) == 40, "StackContext.r15 offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, rsp) == 48, "StackContext.rsp offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, rip) == 56, "StackContext.rip offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, mxcsr) == 64, "StackContext.mxcsr offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, x87cw) == 68, "StackContext.x87cw offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, x24) == 72, "StackContext.x24 offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, x25) == 80, "StackContext.x25 offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, x26) == 88, "StackContext.x26 offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, x27) == 96, "StackContext.x27 offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, x28) == 104, "StackContext.x28 offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, lr) == 112, "StackContext.lr offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, x18) == 120, "StackContext.x18 offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, q8_lo) == 128, "StackContext.q8_lo offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, q8_hi) == 136, "StackContext.q8_hi offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, q9_lo) == 144, "StackContext.q9_lo offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, q9_hi) == 152, "StackContext.q9_hi offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, q10_lo) == 160, "StackContext.q10_lo offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, q10_hi) == 168, "StackContext.q10_hi offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, q11_lo) == 176, "StackContext.q11_lo offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, q11_hi) == 184, "StackContext.q11_hi offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, q12_lo) == 192, "StackContext.q12_lo offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, q12_hi) == 200, "StackContext.q12_hi offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, q13_lo) == 208, "StackContext.q13_lo offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, q13_hi) == 216, "StackContext.q13_hi offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, q14_lo) == 224, "StackContext.q14_lo offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, q14_hi) == 232, "StackContext.q14_hi offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, q15_lo) == 240, "StackContext.q15_lo offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, q15_hi) == 248, "StackContext.q15_hi offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, fpcr) == 256, "StackContext.fpcr offset mismatch");
_Static_assert(offsetof(struct omni_stack_context_layout, fpsr) == 260, "StackContext.fpsr offset mismatch");
_Static_assert(sizeof(struct omni_stack_context_layout) == 264, "StackContext size mismatch");

#if defined(__aarch64__)
void stack_seed_context_nonx86(void* ctx);
__asm__(
    ".text\n"
    ".align 2\n"
    ".global stack_seed_context_nonx86\n"
    ".type stack_seed_context_nonx86, %function\n"
    "stack_seed_context_nonx86:\n"
    "str x19, [x0, #0]\n"
    "str x29, [x0, #8]\n"
    "str x20, [x0, #16]\n"
    "str x21, [x0, #24]\n"
    "str x22, [x0, #32]\n"
    "str x23, [x0, #40]\n"
    "str x24, [x0, #72]\n"
    "str x25, [x0, #80]\n"
    "str x26, [x0, #88]\n"
    "str x27, [x0, #96]\n"
    "str x28, [x0, #104]\n"
    "str x18, [x0, #120]\n"
    "stp q8, q9, [x0, #128]\n"
    "stp q10, q11, [x0, #160]\n"
    "stp q12, q13, [x0, #192]\n"
    "stp q14, q15, [x0, #224]\n"
    "mrs x9, fpcr\n"
    "str w9, [x0, #256]\n"
    "mrs x9, fpsr\n"
    "str w9, [x0, #260]\n"
    "ret\n"
    ".size stack_seed_context_nonx86, .-stack_seed_context_nonx86\n"
);

void stack_context_switch_nonx86(void* old_ctx, void* new_ctx);
__asm__(
    ".text\n"
    ".align 2\n"
    ".global stack_context_switch_nonx86\n"
    ".type stack_context_switch_nonx86, %function\n"
    "stack_context_switch_nonx86:\n"
    "str x19, [x0, #0]\n"
    "str x29, [x0, #8]\n"
    "str x20, [x0, #16]\n"
    "str x21, [x0, #24]\n"
    "str x22, [x0, #32]\n"
    "str x23, [x0, #40]\n"
    "mov x9, sp\n"
    "str x9, [x0, #48]\n"
    "str x30, [x0, #56]\n"
    "str x24, [x0, #72]\n"
    "str x25, [x0, #80]\n"
    "str x26, [x0, #88]\n"
    "str x27, [x0, #96]\n"
    "str x28, [x0, #104]\n"
    "str x30, [x0, #112]\n"
    "str x18, [x0, #120]\n"
    "stp q8, q9, [x0, #128]\n"
    "stp q10, q11, [x0, #160]\n"
    "stp q12, q13, [x0, #192]\n"
    "stp q14, q15, [x0, #224]\n"
    "mrs x9, fpcr\n"
    "str w9, [x0, #256]\n"
    "mrs x9, fpsr\n"
    "str w9, [x0, #260]\n"
    "ldr x19, [x1, #0]\n"
    "ldr x29, [x1, #8]\n"
    "ldr x20, [x1, #16]\n"
    "ldr x21, [x1, #24]\n"
    "ldr x22, [x1, #32]\n"
    "ldr x23, [x1, #40]\n"
    "ldr x9, [x1, #48]\n"
    "mov sp, x9\n"
    "ldr x24, [x1, #72]\n"
    "ldr x25, [x1, #80]\n"
    "ldr x26, [x1, #88]\n"
    "ldr x27, [x1, #96]\n"
    "ldr x28, [x1, #104]\n"
    "ldr x30, [x1, #112]\n"
    "ldr x18, [x1, #120]\n"
    "ldp q8, q9, [x1, #128]\n"
    "ldp q10, q11, [x1, #160]\n"
    "ldp q12, q13, [x1, #192]\n"
    "ldp q14, q15, [x1, #224]\n"
    "ldr w9, [x1, #256]\n"
    "msr fpcr, x9\n"
    "ldr w9, [x1, #260]\n"
    "msr fpsr, x9\n"
    "ldr x9, [x1, #56]\n"
    "br x9\n"
    ".size stack_context_switch_nonx86, .-stack_context_switch_nonx86\n"
);
#else
void stack_seed_context_nonx86(void* ctx) {
    (void)ctx;
}

void stack_context_switch_nonx86(void* old_ctx, void* new_ctx) {
    (void)old_ctx;
    (void)new_ctx;
    fprintf(stderr, "stack engine context switching backend is unsupported on this target\n");
    abort();
}
#endif

/* ASAN-aware stack switch hooks (no-op without ASAN). */
void stack_asan_start_switch(void** fake_stack_save, void* stack_bottom, size_t stack_size) {
    if (__sanitizer_start_switch_fiber != NULL) {
        __sanitizer_start_switch_fiber(fake_stack_save, stack_bottom, stack_size);
        return;
    }
    (void)fake_stack_save;
    (void)stack_bottom;
    (void)stack_size;
}

void stack_asan_finish_switch(void* fake_stack_save) {
    if (__sanitizer_finish_switch_fiber != NULL) {
        const void* old_bottom = NULL;
        size_t old_size = 0;
        __sanitizer_finish_switch_fiber(fake_stack_save, &old_bottom, &old_size);
        return;
    }
    (void)fake_stack_save;
}

/* Raw stack copy for continuation clone.
 * ASAN redzones exist inside suspended stacks; cloning must copy raw bytes.
 */
OMNI_NO_ASAN
void stack_raw_copy(void* dst, const void* src, size_t size) {
    unsigned char* d = (unsigned char*)dst;
    const unsigned char* s = (const unsigned char*)src;
    for (size_t i = 0; i < size; i++) {
        d[i] = s[i];
    }
}

/* ============================================================
 * D2: Stack Overflow Detection via SIGSEGV + sigaltstack
 * ============================================================
 *
 * When a coroutine overflows its stack and hits the guard page,
 * SIGSEGV fires on an alternate signal stack. The handler checks
 * if the fault address is in a known guard page. If so, it
 * siglongjmps to the recovery point set before the context switch.
 *
 * Recovery points form a stack to support nested coro switches.
 */

#define MAX_RECOVERY_DEPTH 64
#define INITIAL_GUARD_CAPACITY 16

struct guard_entry {
    void*  base;
    size_t size;
};

static __thread struct guard_entry* g_guards = NULL;
static __thread int g_guard_count = 0;
static __thread int g_guard_capacity = 0;
static __thread sigjmp_buf g_recovery_stack[MAX_RECOVERY_DEPTH];
static __thread int g_recovery_depth = 0;
static __thread volatile sig_atomic_t g_guard_hit = 0;

static __thread void* g_sigstack = NULL;
static __thread int g_thread_initialized = 0;
static __thread int g_thread_init_refcount = 0;

static pthread_mutex_t g_guard_lock = PTHREAD_MUTEX_INITIALIZER;
static int g_handler_refcount = 0;

/* The context switch function defined in stack_engine.c3 (@naked, SysV ABI) */
extern void omni_context_switch(void* old_ctx, void* new_ctx);

static void sigsegv_handler(int sig, siginfo_t* info, void* ucontext) {
    (void)sig; (void)ucontext;

    if (g_recovery_depth <= 0)
        goto reraise;

    {
        uintptr_t addr = (uintptr_t)info->si_addr;
        for (int i = 0; i < g_guard_count; i++) {
            uintptr_t lo = (uintptr_t)g_guards[i].base;
            uintptr_t hi = lo + g_guards[i].size;
            if (addr >= lo && addr < hi) {
                g_guard_hit = 1;
                int depth = g_recovery_depth - 1;
                g_recovery_depth = depth;
                siglongjmp(g_recovery_stack[depth], 1);
                /* NOTREACHED */
            }
        }
    }

reraise:
    /* Not a known guard page — restore default handler and re-raise */
    {
        struct sigaction sa;
        memset(&sa, 0, sizeof(sa));
        sa.sa_handler = SIG_DFL;
        sigaction(SIGSEGV, &sa, NULL);
        raise(SIGSEGV);
    }
}

int stack_guard_init(void) {
    if (g_thread_init_refcount > 0) {
        g_thread_init_refcount++;
        return 0;
    }

    /* Allocate alternate signal stack */
    g_sigstack = malloc(SIGSTKSZ);
    if (!g_sigstack) return -1;

    stack_t ss;
    memset(&ss, 0, sizeof(ss));
    ss.ss_sp = g_sigstack;
    ss.ss_size = SIGSTKSZ;
    ss.ss_flags = 0;
    if (sigaltstack(&ss, NULL) < 0) {
        free(g_sigstack); g_sigstack = NULL;
        return -1;
    }

    if (pthread_mutex_lock(&g_guard_lock) != 0) {
        stack_t disable_ss;
        memset(&disable_ss, 0, sizeof(disable_ss));
        disable_ss.ss_flags = SS_DISABLE;
        sigaltstack(&disable_ss, NULL);
        free(g_sigstack); g_sigstack = NULL;
        return -1;
    }

    /* Install the process-wide SIGSEGV handler once. */
    if (g_handler_refcount == 0) {
        struct sigaction sa;
        memset(&sa, 0, sizeof(sa));
        sa.sa_sigaction = sigsegv_handler;
        sigemptyset(&sa.sa_mask);
        sa.sa_flags = SA_SIGINFO | SA_ONSTACK;
        if (sigaction(SIGSEGV, &sa, NULL) < 0) {
            pthread_mutex_unlock(&g_guard_lock);
            stack_t disable_ss;
            memset(&disable_ss, 0, sizeof(disable_ss));
            disable_ss.ss_flags = SS_DISABLE;
            sigaltstack(&disable_ss, NULL);
            free(g_sigstack); g_sigstack = NULL;
            return -1;
        }
    }

    g_handler_refcount++;
    pthread_mutex_unlock(&g_guard_lock);

    g_thread_initialized = 1;
    g_thread_init_refcount = 1;
    return 0;
}

void stack_guard_shutdown(void) {
    if (g_thread_init_refcount <= 0) return;
    g_thread_init_refcount--;
    if (g_thread_init_refcount > 0) return;
    if (!g_thread_initialized) return;

    if (pthread_mutex_lock(&g_guard_lock) == 0) {
        if (g_handler_refcount > 0) {
            g_handler_refcount--;
            if (g_handler_refcount == 0) {
                struct sigaction sa;
                memset(&sa, 0, sizeof(sa));
                sa.sa_handler = SIG_DFL;
                sigaction(SIGSEGV, &sa, NULL);
            }
        }
        pthread_mutex_unlock(&g_guard_lock);
    }

    stack_t ss;
    memset(&ss, 0, sizeof(ss));
    ss.ss_flags = SS_DISABLE;
    sigaltstack(&ss, NULL);

    free(g_sigstack); g_sigstack = NULL;
    free(g_guards); g_guards = NULL;
    g_guard_capacity = 0;
    g_guard_count = 0;
    g_recovery_depth = 0;
    g_thread_initialized = 0;
    g_thread_init_refcount = 0;
}

int stack_guard_register(void* base, size_t size) {
    if (base == NULL || size == 0) return -1;

    if (g_guard_count == g_guard_capacity) {
        int new_capacity = g_guard_capacity == 0 ? INITIAL_GUARD_CAPACITY : g_guard_capacity * 2;
        if (new_capacity < g_guard_capacity) return -1;
        struct guard_entry* grown = (struct guard_entry*)realloc(
            g_guards, (size_t)new_capacity * sizeof(*g_guards)
        );
        if (grown == NULL) return -1;
        g_guards = grown;
        g_guard_capacity = new_capacity;
    }

    g_guards[g_guard_count].base = base;
    g_guards[g_guard_count].size = size;
    g_guard_count++;
    return 0;
}

void stack_guard_unregister(void* base) {
    for (int i = 0; i < g_guard_count; i++) {
        if (g_guards[i].base == base) {
            g_guards[i] = g_guards[g_guard_count - 1];
            g_guard_count--;
            return;
        }
    }
}

/**
 * Protected context switch with stack overflow recovery.
 *
 * Wraps sigsetjmp + context switch so the sigsetjmp frame is live
 * when siglongjmp fires from the SIGSEGV handler.
 *
 * Returns 0 on normal switch-back, 1 on stack overflow recovery.
 */
int stack_guard_protected_switch(void* old_ctx, void* new_ctx) {
    if (g_recovery_depth >= MAX_RECOVERY_DEPTH) {
        /* Too deep — switch without protection */
        omni_context_switch(old_ctx, new_ctx);
        return 0;
    }

    g_guard_hit = 0;
    if (sigsetjmp(g_recovery_stack[g_recovery_depth], 1) != 0) {
        /* Stack overflow — siglongjmp'd back here */
        return 1;
    }
    g_recovery_depth++;

    omni_context_switch(old_ctx, new_ctx);

    g_recovery_depth--;
    return 0;
}
