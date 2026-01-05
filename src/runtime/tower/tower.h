/*
 * Tower of Interpreters
 *
 * Implementation of the "Collapsing Towers of Interpreters" pattern.
 * Based on Amin & Rompf, POPL 2018.
 *
 * The tower uses a 9-handler dispatch table:
 *   lit, var, lam, app, if, lft, run, em, clam
 *
 * Each handler can be customized per meta-level, enabling:
 * - Multi-stage programming (staging)
 * - Partial evaluation
 * - Tower collapse (fusing adjacent levels)
 */

#ifndef TOWER_H
#define TOWER_H

#include "../types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============== Handler Types ============== */

/* Forward declarations */
struct TowerEnv;
struct TowerMEnv;

/* Handler function signature: (expr, menv) -> value */
typedef Value* (*TowerHandler)(Value* expr, struct TowerMEnv* menv);

/* The 9 handlers that define interpreter semantics */
typedef struct TowerHandlers {
    TowerHandler h_lit;   /* Literal: numbers, strings */
    TowerHandler h_var;   /* Variable lookup */
    TowerHandler h_lam;   /* Lambda abstraction */
    TowerHandler h_app;   /* Function application */
    TowerHandler h_if;    /* Conditional */
    TowerHandler h_lft;   /* Lift: move value up one level */
    TowerHandler h_run;   /* Run: execute code at lower level */
    TowerHandler h_em;    /* EM (Escape to Meta): go up one level */
    TowerHandler h_clam;  /* Code lambda: lambda that produces code */
} TowerHandlers;

/* Meta-environment: handlers + lexical environment + parent level */
typedef struct TowerMEnv {
    TowerHandlers handlers;
    struct TowerEnv* env;       /* Current lexical environment */
    struct TowerMEnv* parent;   /* Parent meta-level (for EM) */
    int level;                  /* Tower level (0 = ground) */

    /* For staging: track generated code */
    Value* staged_code;
    int is_staging;
} TowerMEnv;

/* Lexical environment (binding stack) */
typedef struct TowerEnv {
    Value* bindings;            /* Association list: ((name . value) ...) */
    struct TowerEnv* parent;
} TowerEnv;

/* ============== Tower Construction ============== */

/* Create default handlers (standard interpreter semantics) */
TowerHandlers tower_default_handlers(void);

/* Create staging handlers (generate code instead of executing) */
TowerHandlers tower_staging_handlers(void);

/* Create a new meta-environment */
TowerMEnv* tower_menv_new(TowerHandlers handlers, TowerEnv* env, TowerMEnv* parent);

/* Free meta-environment */
void tower_menv_free(TowerMEnv* menv);

/* Create a new lexical environment */
TowerEnv* tower_env_new(TowerEnv* parent);

/* Free lexical environment */
void tower_env_free(TowerEnv* env);

/* ============== Environment Operations ============== */

/* Lookup variable in environment */
Value* tower_env_lookup(TowerEnv* env, Value* name);

/* Define variable in environment */
void tower_env_define(TowerEnv* env, Value* name, Value* value);

/* Set variable in environment (mutation) */
void tower_env_set(TowerEnv* env, Value* name, Value* value);

/* Extend environment with bindings */
TowerEnv* tower_env_extend(TowerEnv* env, Value* names, Value* values);

/* ============== Core Evaluation ============== */

/* Main tower evaluator */
Value* tower_eval(Value* expr, TowerMEnv* menv);

/* Apply function */
Value* tower_apply(Value* fn, Value* args, TowerMEnv* menv);

/* ============== Meta-Level Operations ============== */

/* Get current tower level */
int tower_level(TowerMEnv* menv);

/* Shift: evaluate expression at different level
 * (shift n expr) - evaluate expr at n levels up
 */
Value* tower_shift(int n, Value* expr, TowerMEnv* menv);

/* EM: escape to parent meta-level
 * (EM expr) - evaluate expr at parent level
 */
Value* tower_em(Value* expr, TowerMEnv* menv);

/* Lift: move a value up one level (for staging) */
Value* tower_lift(Value* value, TowerMEnv* menv);

/* Run: execute staged code at lower level */
Value* tower_run(Value* code, TowerMEnv* menv);

/* ============== Handler Scoping ============== */

/* with-handlers: temporarily install custom handlers
 * (with-handlers ((name handler) ...) body)
 */
Value* tower_with_handlers(Value* handler_bindings, Value* body, TowerMEnv* menv);

/* Get handler by name */
TowerHandler tower_get_handler(TowerMEnv* menv, const char* name);

/* Set handler by name */
void tower_set_handler(TowerMEnv* menv, const char* name, TowerHandler handler);

/* ============== Staging Support ============== */

/* Begin staging mode: generate code instead of executing */
TowerMEnv* tower_begin_staging(TowerMEnv* menv);

/* End staging mode: return generated code */
Value* tower_end_staging(TowerMEnv* menv);

/* Generate fresh variable name for staging */
Value* tower_gensym(TowerMEnv* menv);

/* ============== Convenience Functions ============== */

/* Evaluate string at ground level */
Value* tower_eval_string(const char* input);

/* Evaluate string at specific level */
Value* tower_eval_string_at_level(const char* input, int level);

/* Initialize global tower environment */
TowerMEnv* tower_init(void);

/* Cleanup tower resources */
void tower_cleanup(void);

#ifdef __cplusplus
}
#endif

#endif /* TOWER_H */
