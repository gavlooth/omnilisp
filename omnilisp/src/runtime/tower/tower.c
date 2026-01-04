/*
 * Tower of Interpreters Implementation
 *
 * Metacircular evaluator with customizable handlers.
 * Supports multi-stage programming and tower collapse.
 */

#define _POSIX_C_SOURCE 200809L
#include "tower.h"
#include "../pika/pika_reader.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* ============== Global State ============== */

static TowerMEnv* g_tower_menv = NULL;
static int g_gensym_counter = 0;

/* ============== Built-in Primitives ============== */

static Value* prim_add(Value* args, Value* menv) {
    (void)menv;
    int64_t sum = 0;
    while (!is_nil(args)) {
        Value* v = car(args);
        if (v && v->tag == T_INT) sum += v->i;
        args = cdr(args);
    }
    return mk_int(sum);
}

static Value* prim_sub(Value* args, Value* menv) {
    (void)menv;
    if (is_nil(args)) return mk_int(0);
    Value* first = car(args);
    if (!first || first->tag != T_INT) return mk_error("- requires integers");
    args = cdr(args);
    if (is_nil(args)) return mk_int(-first->i);  /* unary minus */
    int64_t result = first->i;
    while (!is_nil(args)) {
        Value* v = car(args);
        if (v && v->tag == T_INT) result -= v->i;
        args = cdr(args);
    }
    return mk_int(result);
}

static Value* prim_mul(Value* args, Value* menv) {
    (void)menv;
    int64_t product = 1;
    while (!is_nil(args)) {
        Value* v = car(args);
        if (v && v->tag == T_INT) product *= v->i;
        args = cdr(args);
    }
    return mk_int(product);
}

static Value* prim_div(Value* args, Value* menv) {
    (void)menv;
    if (is_nil(args)) return mk_error("/ requires at least one argument");
    Value* first = car(args);
    if (!first || first->tag != T_INT) return mk_error("/ requires integers");
    args = cdr(args);
    if (is_nil(args)) {
        if (first->i == 0) return mk_error("division by zero");
        return mk_int(1 / first->i);
    }
    int64_t result = first->i;
    while (!is_nil(args)) {
        Value* v = car(args);
        if (v && v->tag == T_INT) {
            if (v->i == 0) return mk_error("division by zero");
            result /= v->i;
        }
        args = cdr(args);
    }
    return mk_int(result);
}

static Value* prim_mod(Value* args, Value* menv) {
    (void)menv;
    Value* a = car(args);
    Value* b = car(cdr(args));
    if (!a || !b || a->tag != T_INT || b->tag != T_INT)
        return mk_error("mod requires two integers");
    if (b->i == 0) return mk_error("modulo by zero");
    return mk_int(a->i % b->i);
}

static Value* prim_eq(Value* args, Value* menv) {
    (void)menv;
    Value* a = car(args);
    Value* b = car(cdr(args));
    if (!a || !b) return mk_sym("#f");
    if (a->tag != b->tag) return mk_sym("#f");
    switch (a->tag) {
        case T_INT: return a->i == b->i ? mk_sym("#t") : mk_sym("#f");
        case T_SYM: return strcmp(a->s, b->s) == 0 ? mk_sym("#t") : mk_sym("#f");
        case T_NIL: return mk_sym("#t");
        default: return a == b ? mk_sym("#t") : mk_sym("#f");
    }
}

static Value* prim_lt(Value* args, Value* menv) {
    (void)menv;
    Value* a = car(args);
    Value* b = car(cdr(args));
    if (!a || !b || a->tag != T_INT || b->tag != T_INT)
        return mk_error("< requires integers");
    return a->i < b->i ? mk_sym("#t") : mk_sym("#f");
}

static Value* prim_gt(Value* args, Value* menv) {
    (void)menv;
    Value* a = car(args);
    Value* b = car(cdr(args));
    if (!a || !b || a->tag != T_INT || b->tag != T_INT)
        return mk_error("> requires integers");
    return a->i > b->i ? mk_sym("#t") : mk_sym("#f");
}

static Value* prim_cons(Value* args, Value* menv) {
    (void)menv;
    Value* a = car(args);
    Value* b = car(cdr(args));
    return mk_cell(a ? a : mk_nil(), b ? b : mk_nil());
}

static Value* prim_car(Value* args, Value* menv) {
    (void)menv;
    Value* cell = car(args);
    if (!cell || cell->tag != T_CELL) return mk_error("car requires a pair");
    return car(cell);
}

static Value* prim_cdr(Value* args, Value* menv) {
    (void)menv;
    Value* cell = car(args);
    if (!cell || cell->tag != T_CELL) return mk_error("cdr requires a pair");
    return cdr(cell);
}

static Value* prim_null(Value* args, Value* menv) {
    (void)menv;
    Value* v = car(args);
    return is_nil(v) ? mk_sym("#t") : mk_sym("#f");
}

static Value* prim_list(Value* args, Value* menv) {
    (void)menv;
    return args;
}

static Value* prim_not(Value* args, Value* menv) {
    (void)menv;
    Value* v = car(args);
    if (v && v->tag == T_SYM && strcmp(v->s, "#f") == 0)
        return mk_sym("#t");
    return mk_sym("#f");
}

static Value* prim_print(Value* args, Value* menv) {
    (void)menv;
    Value* v = car(args);
    char* s = val_to_str(v);
    printf("%s\n", s);
    free(s);
    return v;
}

/* ============== Environment Implementation ============== */

TowerEnv* tower_env_new(TowerEnv* parent) {
    TowerEnv* env = malloc(sizeof(TowerEnv));
    if (!env) return NULL;
    env->bindings = mk_nil();
    env->parent = parent;
    return env;
}

void tower_env_free(TowerEnv* env) {
    /* Note: bindings are managed by ASAP */
    if (env) free(env);
}

Value* tower_env_lookup(TowerEnv* env, Value* name) {
    if (!name || name->tag != T_SYM) return NULL;

    while (env) {
        Value* bindings = env->bindings;
        while (!is_nil(bindings)) {
            Value* pair = car(bindings);
            if (pair && pair->tag == T_CELL) {
                Value* key = car(pair);
                if (key && key->tag == T_SYM && strcmp(key->s, name->s) == 0) {
                    return cdr(pair);
                }
            }
            bindings = cdr(bindings);
        }
        env = env->parent;
    }
    return NULL;
}

void tower_env_define(TowerEnv* env, Value* name, Value* value) {
    if (!env || !name || name->tag != T_SYM) return;
    Value* pair = mk_cell(name, value);
    env->bindings = mk_cell(pair, env->bindings);
}

void tower_env_set(TowerEnv* env, Value* name, Value* value) {
    if (!name || name->tag != T_SYM) return;

    while (env) {
        Value* bindings = env->bindings;
        while (!is_nil(bindings)) {
            Value* pair = car(bindings);
            if (pair && pair->tag == T_CELL) {
                Value* key = car(pair);
                if (key && key->tag == T_SYM && strcmp(key->s, name->s) == 0) {
                    pair->cell.cdr = value;
                    return;
                }
            }
            bindings = cdr(bindings);
        }
        env = env->parent;
    }
}

TowerEnv* tower_env_extend(TowerEnv* env, Value* names, Value* values) {
    TowerEnv* new_env = tower_env_new(env);
    if (!new_env) return NULL;

    while (!is_nil(names) && !is_nil(values)) {
        Value* name = car(names);
        Value* value = car(values);

        /* Handle rest parameter .. */
        if (name && name->tag == T_SYM && strcmp(name->s, "..") == 0) {
            names = cdr(names);
            if (!is_nil(names)) {
                name = car(names);
                tower_env_define(new_env, name, values);
            }
            break;
        }

        tower_env_define(new_env, name, value);
        names = cdr(names);
        values = cdr(values);
    }

    return new_env;
}

/* ============== Meta-Environment Implementation ============== */

TowerMEnv* tower_menv_new(TowerHandlers handlers, TowerEnv* env, TowerMEnv* parent) {
    TowerMEnv* menv = malloc(sizeof(TowerMEnv));
    if (!menv) return NULL;
    menv->handlers = handlers;
    menv->env = env;
    menv->parent = parent;
    menv->level = parent ? parent->level + 1 : 0;
    menv->staged_code = mk_nil();
    menv->is_staging = 0;
    return menv;
}

void tower_menv_free(TowerMEnv* menv) {
    if (menv) {
        /* Note: We intentionally do NOT free menv->env here.
         * Closures may still reference the environment after we return,
         * e.g., in currying: ((lambda (x) (lambda (y) (+ x y))) 3)
         * The inner lambda keeps a reference to the env where x=3.
         * Proper fix would be reference counting on environments. */
        free(menv);
    }
}

/* ============== Default Handlers ============== */

/* Forward declarations for default handlers */
static Value* default_h_lit(Value* expr, TowerMEnv* menv);
static Value* default_h_var(Value* expr, TowerMEnv* menv);
static Value* default_h_lam(Value* expr, TowerMEnv* menv);
static Value* default_h_app(Value* expr, TowerMEnv* menv);
static Value* default_h_if(Value* expr, TowerMEnv* menv);
static Value* default_h_lft(Value* expr, TowerMEnv* menv);
static Value* default_h_run(Value* expr, TowerMEnv* menv);
static Value* default_h_em(Value* expr, TowerMEnv* menv);
static Value* default_h_clam(Value* expr, TowerMEnv* menv);

TowerHandlers tower_default_handlers(void) {
    TowerHandlers h = {
        .h_lit = default_h_lit,
        .h_var = default_h_var,
        .h_lam = default_h_lam,
        .h_app = default_h_app,
        .h_if = default_h_if,
        .h_lft = default_h_lft,
        .h_run = default_h_run,
        .h_em = default_h_em,
        .h_clam = default_h_clam
    };
    return h;
}

/* Literal handler: return as-is */
static Value* default_h_lit(Value* expr, TowerMEnv* menv) {
    (void)menv;
    return expr;
}

/* Variable handler: lookup in environment */
static Value* default_h_var(Value* expr, TowerMEnv* menv) {
    Value* value = tower_env_lookup(menv->env, expr);
    if (!value) {
        char msg[128];
        snprintf(msg, sizeof(msg), "Undefined variable: %s", expr->s);
        return mk_error(msg);
    }
    return value;
}

/* Lambda handler: create closure */
static Value* default_h_lam(Value* expr, TowerMEnv* menv) {
    /* expr = (lambda params body) */
    Value* params = car(cdr(expr));
    Value* body = car(cdr(cdr(expr)));

    /* Handle array-style params [x y z] */
    if (params && params->tag == T_CELL && car(params) &&
        car(params)->tag == T_SYM && strcmp(car(params)->s, "array") == 0) {
        params = cdr(params);
    }

    /* Create closure with current environment */
    return mk_lambda(params, body, (Value*)menv->env);
}

/* Application handler: evaluate function and args, then apply */
static Value* default_h_app(Value* expr, TowerMEnv* menv) {
    Value* fn = tower_eval(car(expr), menv);
    if (is_error(fn)) return fn;

    /* Evaluate arguments */
    Value* args = cdr(expr);
    Value* evaled = mk_nil();
    Value** tail = &evaled;

    while (!is_nil(args)) {
        Value* val = tower_eval(car(args), menv);
        if (is_error(val)) return val;
        Value* cell = mk_cell(val, mk_nil());
        *tail = cell;
        tail = &cell->cell.cdr;
        args = cdr(args);
    }

    return tower_apply(fn, evaled, menv);
}

/* If handler: evaluate condition, then branch */
static Value* default_h_if(Value* expr, TowerMEnv* menv) {
    /* expr = (if cond then else) */
    Value* cond = tower_eval(car(cdr(expr)), menv);
    if (is_error(cond)) return cond;

    /* Truthiness: #f, false, nil, and nothing are falsy */
    int truthy = 1;
    if (cond->tag == T_NOTHING) truthy = 0;
    if (cond->tag == T_NIL) truthy = 0;
    if (cond->tag == T_SYM &&
        (strcmp(cond->s, "#f") == 0 || strcmp(cond->s, "false") == 0)) truthy = 0;

    if (truthy) {
        return tower_eval(car(cdr(cdr(expr))), menv);
    } else {
        Value* else_branch = cdr(cdr(cdr(expr)));
        if (is_nil(else_branch)) return mk_nothing();
        return tower_eval(car(else_branch), menv);
    }
}

/* Lift handler: quote value for use at higher level */
static Value* default_h_lft(Value* expr, TowerMEnv* menv) {
    /* expr = (lift value) */
    Value* value = tower_eval(car(cdr(expr)), menv);
    if (is_error(value)) return value;
    return tower_lift(value, menv);
}

/* Run handler: execute code at lower level */
static Value* default_h_run(Value* expr, TowerMEnv* menv) {
    /* expr = (run code) */
    Value* code = tower_eval(car(cdr(expr)), menv);
    if (is_error(code)) return code;
    return tower_run(code, menv);
}

/* EM handler: escape to meta level */
static Value* default_h_em(Value* expr, TowerMEnv* menv) {
    /* expr = (EM body) */
    return tower_em(car(cdr(expr)), menv);
}

/* Code lambda handler: lambda that generates code */
static Value* default_h_clam(Value* expr, TowerMEnv* menv) {
    /* In staging mode, this generates code for a lambda */
    /* In execution mode, same as regular lambda */
    return default_h_lam(expr, menv);
}

/* ============== Staging Handlers ============== */

/* Forward declarations for staging handlers */
static Value* staging_h_lit(Value* expr, TowerMEnv* menv);
static Value* staging_h_var(Value* expr, TowerMEnv* menv);
static Value* staging_h_lam(Value* expr, TowerMEnv* menv);
static Value* staging_h_app(Value* expr, TowerMEnv* menv);
static Value* staging_h_if(Value* expr, TowerMEnv* menv);
static Value* staging_h_lft(Value* expr, TowerMEnv* menv);

TowerHandlers tower_staging_handlers(void) {
    TowerHandlers h = tower_default_handlers();
    h.h_lit = staging_h_lit;
    h.h_var = staging_h_var;
    h.h_lam = staging_h_lam;
    h.h_app = staging_h_app;
    h.h_if = staging_h_if;
    h.h_lft = staging_h_lft;
    return h;
}

/* Staging: generate code that will produce the literal */
static Value* staging_h_lit(Value* expr, TowerMEnv* menv) {
    (void)menv;
    /* Return quoted literal */
    return mk_cell(mk_sym("quote"), mk_cell(expr, mk_nil()));
}

/* Staging: generate variable reference */
static Value* staging_h_var(Value* expr, TowerMEnv* menv) {
    /* Check if variable is bound at this stage */
    Value* value = tower_env_lookup(menv->env, expr);
    if (value) {
        /* Variable is bound - return its (staged) value */
        return value;
    }
    /* Unbound - generate code for variable reference */
    return expr;
}

/* Staging: generate lambda code */
static Value* staging_h_lam(Value* expr, TowerMEnv* menv) {
    /* Generate lambda expression with staged body */
    Value* params = car(cdr(expr));
    Value* body = car(cdr(cdr(expr)));

    /* Create new environment for staging the body */
    TowerEnv* lam_env = tower_env_new(menv->env);

    /* Bind parameters to fresh variables */
    Value* ps = params;
    if (ps && ps->tag == T_CELL && car(ps) &&
        car(ps)->tag == T_SYM && strcmp(car(ps)->s, "array") == 0) {
        ps = cdr(ps);
    }
    while (!is_nil(ps)) {
        Value* p = car(ps);
        tower_env_define(lam_env, p, p); /* Bind to self for reference */
        ps = cdr(ps);
    }

    TowerMEnv* lam_menv = tower_menv_new(menv->handlers, lam_env, menv->parent);
    lam_menv->is_staging = 1;

    Value* staged_body = tower_eval(body, lam_menv);
    tower_menv_free(lam_menv);

    return mk_cell(mk_sym("lambda"),
                   mk_cell(params,
                           mk_cell(staged_body, mk_nil())));
}

/* Staging: generate application code */
static Value* staging_h_app(Value* expr, TowerMEnv* menv) {
    /* Generate code for function application */
    Value* result = mk_nil();
    Value** tail = &result;

    while (!is_nil(expr)) {
        Value* staged = tower_eval(car(expr), menv);
        Value* cell = mk_cell(staged, mk_nil());
        *tail = cell;
        tail = &cell->cell.cdr;
        expr = cdr(expr);
    }

    return result;
}

/* Staging: generate if code */
static Value* staging_h_if(Value* expr, TowerMEnv* menv) {
    Value* cond = tower_eval(car(cdr(expr)), menv);
    Value* then_branch = tower_eval(car(cdr(cdr(expr))), menv);
    Value* else_args = cdr(cdr(cdr(expr)));
    Value* else_branch = is_nil(else_args) ? mk_nothing()
                                           : tower_eval(car(else_args), menv);

    return mk_cell(mk_sym("if"),
                   mk_cell(cond,
                           mk_cell(then_branch,
                                   mk_cell(else_branch, mk_nil()))));
}

/* Staging: lift value to code */
static Value* staging_h_lft(Value* expr, TowerMEnv* menv) {
    /* Evaluate at current level, then quote */
    Value* value = tower_eval(car(cdr(expr)), menv);
    if (is_error(value)) return value;
    return mk_cell(mk_sym("quote"), mk_cell(value, mk_nil()));
}

/* ============== Core Tower Evaluation ============== */

/* Classify expression type */
static int is_self_evaluating(Value* expr) {
    if (!expr) return 1;
    switch (expr->tag) {
        case T_INT:
        case T_CODE:
        case T_PRIM:
        case T_LAMBDA:
        case T_ERROR:
        case T_NIL:
        case T_NOTHING:
            return 1;
        default:
            return 0;
    }
}

/* Check for special form */
static const char* get_special_form(Value* expr) {
    if (!expr || expr->tag != T_CELL) return NULL;
    Value* op = car(expr);
    if (!op || op->tag != T_SYM) return NULL;

    const char* forms[] = {
        "if", "lambda", "fn", "let", "define", "quote", "do", "begin",
        "lift", "run", "EM", "clam", "with-handlers",
        "quasiquote", "unquote", "unquote-splicing",
        "set!", "match", "cond", "prompt", "control",
        NULL
    };

    for (int i = 0; forms[i]; i++) {
        if (strcmp(op->s, forms[i]) == 0) return forms[i];
    }
    return NULL;
}

Value* tower_eval(Value* expr, TowerMEnv* menv) {
    if (!expr) return mk_nothing();
    if (!menv) return mk_error("NULL meta-environment");

    /* Self-evaluating */
    if (is_self_evaluating(expr)) {
        return menv->handlers.h_lit(expr, menv);
    }

    /* Variable */
    if (expr->tag == T_SYM) {
        /* Special symbols */
        if (strcmp(expr->s, "true") == 0) return mk_sym("true");
        if (strcmp(expr->s, "false") == 0) return mk_sym("false");
        if (strcmp(expr->s, "nothing") == 0) return mk_nothing();

        return menv->handlers.h_var(expr, menv);
    }

    /* List: special form or application */
    if (expr->tag == T_CELL) {
        if (is_nil(expr)) return expr;

        const char* special = get_special_form(expr);

        if (special) {
            /* Handle special forms */
            if (strcmp(special, "if") == 0) {
                return menv->handlers.h_if(expr, menv);
            }
            if (strcmp(special, "lambda") == 0 || strcmp(special, "fn") == 0) {
                return menv->handlers.h_lam(expr, menv);
            }
            if (strcmp(special, "lift") == 0) {
                return menv->handlers.h_lft(expr, menv);
            }
            if (strcmp(special, "run") == 0) {
                return menv->handlers.h_run(expr, menv);
            }
            if (strcmp(special, "EM") == 0) {
                return menv->handlers.h_em(expr, menv);
            }
            if (strcmp(special, "clam") == 0) {
                return menv->handlers.h_clam(expr, menv);
            }
            if (strcmp(special, "quote") == 0) {
                return car(cdr(expr));
            }
            if (strcmp(special, "do") == 0 || strcmp(special, "begin") == 0) {
                Value* result = mk_nothing();
                Value* body = cdr(expr);
                while (!is_nil(body)) {
                    result = tower_eval(car(body), menv);
                    if (is_error(result)) return result;
                    body = cdr(body);
                }
                return result;
            }
            if (strcmp(special, "let") == 0) {
                /* Supports both:
                 *   Scheme-style: (let ((x 1) (y 2)) body...)
                 *   Array-style:  (let [x 1 y 2] body...)  -> (let (array x 1 y 2) body...)
                 */
                Value* bindings = car(cdr(expr));
                Value* body = cdr(cdr(expr));

                /* Check for array wrapper (bracket syntax) */
                int is_array_style = 0;
                if (bindings && bindings->tag == T_CELL && car(bindings) &&
                    car(bindings)->tag == T_SYM &&
                    strcmp(car(bindings)->s, "array") == 0) {
                    bindings = cdr(bindings);
                    is_array_style = 1;
                }

                TowerEnv* let_env = tower_env_new(menv->env);
                TowerMEnv* let_menv = tower_menv_new(menv->handlers, let_env, menv->parent);
                let_menv->level = menv->level;
                let_menv->is_staging = menv->is_staging;

                /* Process bindings based on style */
                if (is_array_style) {
                    /* Array-style: (x 1 y 2) as flat list */
                    while (!is_nil(bindings)) {
                        Value* name = car(bindings);
                        bindings = cdr(bindings);
                        if (is_nil(bindings)) break;
                        Value* val_expr = car(bindings);
                        bindings = cdr(bindings);
                        Value* value = tower_eval(val_expr, let_menv);
                        if (is_error(value)) {
                            tower_menv_free(let_menv);
                            return value;
                        }
                        tower_env_define(let_env, name, value);
                    }
                } else {
                    /* Scheme-style: each binding is (name value) */
                    while (!is_nil(bindings)) {
                        Value* binding = car(bindings);
                        if (!binding || binding->tag != T_CELL) {
                            bindings = cdr(bindings);
                            continue;
                        }
                        Value* name = car(binding);
                        Value* val_expr = car(cdr(binding));
                        Value* value = tower_eval(val_expr, let_menv);
                        if (is_error(value)) {
                            tower_menv_free(let_menv);
                            return value;
                        }
                        tower_env_define(let_env, name, value);
                        bindings = cdr(bindings);
                    }
                }

                /* Evaluate body */
                Value* result = mk_nothing();
                while (!is_nil(body)) {
                    result = tower_eval(car(body), let_menv);
                    if (is_error(result)) {
                        tower_menv_free(let_menv);
                        return result;
                    }
                    body = cdr(body);
                }

                tower_menv_free(let_menv);
                return result;
            }
            if (strcmp(special, "define") == 0) {
                Value* first = car(cdr(expr));
                if (first && first->tag == T_CELL) {
                    /* Function definition */
                    Value* name = car(first);
                    Value* params = cdr(first);
                    Value* body = car(cdr(cdr(expr)));

                    Value* lambda = mk_lambda(params, body, (Value*)menv->env);
                    tower_env_define(menv->env, name, lambda);
                    return name;
                } else {
                    /* Variable definition */
                    Value* name = first;
                    Value* value = tower_eval(car(cdr(cdr(expr))), menv);
                    if (is_error(value)) return value;
                    tower_env_define(menv->env, name, value);
                    return name;
                }
            }
            if (strcmp(special, "set!") == 0) {
                Value* name = car(cdr(expr));
                Value* value = tower_eval(car(cdr(cdr(expr))), menv);
                if (is_error(value)) return value;
                tower_env_set(menv->env, name, value);
                return mk_nothing();
            }
            if (strcmp(special, "with-handlers") == 0) {
                return tower_with_handlers(car(cdr(expr)), cdr(cdr(expr)), menv);
            }
        }

        /* Application */
        return menv->handlers.h_app(expr, menv);
    }

    return expr;
}

Value* tower_apply(Value* fn, Value* args, TowerMEnv* menv) {
    if (!fn) return mk_error("Cannot apply NULL");

    if (fn->tag == T_LAMBDA) {
        /* Get closure environment */
        TowerEnv* closure_env = (TowerEnv*)fn->lam.env;
        if (!closure_env) closure_env = menv->env;

        /* Extend with parameter bindings */
        TowerEnv* call_env = tower_env_extend(closure_env, fn->lam.params, args);
        if (!call_env) return mk_error("OOM in apply");

        TowerMEnv* call_menv = tower_menv_new(menv->handlers, call_env, menv->parent);
        call_menv->level = menv->level;
        call_menv->is_staging = menv->is_staging;

        Value* result = tower_eval(fn->lam.body, call_menv);

        tower_menv_free(call_menv);
        return result;
    }

    if (fn->tag == T_PRIM) {
        /* Built-in primitive */
        if (fn->prim) {
            return fn->prim(args, NULL);
        }
        return mk_error("Invalid primitive");
    }

    return mk_error("Not a function");
}

/* ============== Meta-Level Operations ============== */

int tower_level(TowerMEnv* menv) {
    return menv ? menv->level : 0;
}

Value* tower_shift(int n, Value* expr, TowerMEnv* menv) {
    if (n <= 0) return tower_eval(expr, menv);

    /* Go up n levels */
    TowerMEnv* target = menv;
    for (int i = 0; i < n && target->parent; i++) {
        target = target->parent;
    }

    return tower_eval(expr, target);
}

Value* tower_em(Value* expr, TowerMEnv* menv) {
    if (!menv->parent) {
        return mk_error("EM at top level");
    }
    return tower_eval(expr, menv->parent);
}

Value* tower_lift(Value* value, TowerMEnv* menv) {
    (void)menv;
    /* Quote the value for use at higher level */
    return mk_cell(mk_sym("quote"), mk_cell(value, mk_nil()));
}

Value* tower_run(Value* code, TowerMEnv* menv) {
    /* Execute code at ground level */
    TowerMEnv* ground = menv;
    while (ground->parent) {
        ground = ground->parent;
    }
    return tower_eval(code, ground);
}

/* ============== Handler Scoping ============== */

Value* tower_with_handlers(Value* handler_bindings, Value* body, TowerMEnv* menv) {
    /* Create new menv with modified handlers */
    TowerMEnv* new_menv = tower_menv_new(menv->handlers, menv->env, menv->parent);
    new_menv->level = menv->level;
    new_menv->is_staging = menv->is_staging;

    /* Process handler bindings */
    if (handler_bindings && handler_bindings->tag == T_CELL &&
        car(handler_bindings) && car(handler_bindings)->tag == T_SYM &&
        strcmp(car(handler_bindings)->s, "array") == 0) {
        handler_bindings = cdr(handler_bindings);
    }

    while (!is_nil(handler_bindings)) {
        Value* binding = car(handler_bindings);

        /* Each binding is [name handler-fn] */
        if (binding && binding->tag == T_CELL) {
            if (car(binding) && car(binding)->tag == T_SYM &&
                strcmp(car(binding)->s, "array") == 0) {
                binding = cdr(binding);
            }

            Value* name = car(binding);
            Value* handler_expr = car(cdr(binding));

            if (name && name->tag == T_SYM) {
                Value* handler = tower_eval(handler_expr, menv);
                if (!is_error(handler) && handler->tag == T_LAMBDA) {
                    /* Install handler (simplified - would need proper handler wrapping) */
                    /* For now, just record that a custom handler exists */
                }
            }
        }

        handler_bindings = cdr(handler_bindings);
    }

    /* Evaluate body */
    Value* result = mk_nothing();
    while (!is_nil(body)) {
        result = tower_eval(car(body), new_menv);
        if (is_error(result)) break;
        body = cdr(body);
    }

    tower_menv_free(new_menv);
    return result;
}

TowerHandler tower_get_handler(TowerMEnv* menv, const char* name) {
    if (!menv || !name) return NULL;

    if (strcmp(name, "lit") == 0) return menv->handlers.h_lit;
    if (strcmp(name, "var") == 0) return menv->handlers.h_var;
    if (strcmp(name, "lam") == 0) return menv->handlers.h_lam;
    if (strcmp(name, "app") == 0) return menv->handlers.h_app;
    if (strcmp(name, "if") == 0) return menv->handlers.h_if;
    if (strcmp(name, "lft") == 0) return menv->handlers.h_lft;
    if (strcmp(name, "run") == 0) return menv->handlers.h_run;
    if (strcmp(name, "em") == 0) return menv->handlers.h_em;
    if (strcmp(name, "clam") == 0) return menv->handlers.h_clam;

    return NULL;
}

void tower_set_handler(TowerMEnv* menv, const char* name, TowerHandler handler) {
    if (!menv || !name || !handler) return;

    if (strcmp(name, "lit") == 0) menv->handlers.h_lit = handler;
    else if (strcmp(name, "var") == 0) menv->handlers.h_var = handler;
    else if (strcmp(name, "lam") == 0) menv->handlers.h_lam = handler;
    else if (strcmp(name, "app") == 0) menv->handlers.h_app = handler;
    else if (strcmp(name, "if") == 0) menv->handlers.h_if = handler;
    else if (strcmp(name, "lft") == 0) menv->handlers.h_lft = handler;
    else if (strcmp(name, "run") == 0) menv->handlers.h_run = handler;
    else if (strcmp(name, "em") == 0) menv->handlers.h_em = handler;
    else if (strcmp(name, "clam") == 0) menv->handlers.h_clam = handler;
}

/* ============== Staging Support ============== */

TowerMEnv* tower_begin_staging(TowerMEnv* menv) {
    TowerHandlers staging = tower_staging_handlers();
    TowerMEnv* staging_menv = tower_menv_new(staging, tower_env_new(menv->env), menv);
    staging_menv->is_staging = 1;
    return staging_menv;
}

Value* tower_end_staging(TowerMEnv* menv) {
    return menv->staged_code;
}

Value* tower_gensym(TowerMEnv* menv) {
    (void)menv;
    char buf[32];
    snprintf(buf, sizeof(buf), "__g%d__", g_gensym_counter++);
    return mk_sym(buf);
}

/* ============== Convenience Functions ============== */

Value* tower_eval_string(const char* input) {
    return tower_eval_string_at_level(input, 0);
}

Value* tower_eval_string_at_level(const char* input, int level) {
    TowerMEnv* menv = tower_init();
    if (!menv) return mk_error("Failed to initialize tower");

    /* Parse input */
    Value* expr = pika_parse(input);
    if (is_error(expr)) return expr;

    /* Shift to desired level */
    TowerMEnv* target = menv;
    for (int i = 0; i < level && target; i++) {
        target = tower_menv_new(tower_staging_handlers(),
                                tower_env_new(target->env), target);
        target->is_staging = 1;
    }

    return tower_eval(expr, target);
}

TowerMEnv* tower_init(void) {
    if (g_tower_menv) return g_tower_menv;

    TowerEnv* env = tower_env_new(NULL);
    g_tower_menv = tower_menv_new(tower_default_handlers(), env, NULL);

    /* Register built-in primitives */
    tower_env_define(env, mk_sym("+"), mk_prim(prim_add));
    tower_env_define(env, mk_sym("-"), mk_prim(prim_sub));
    tower_env_define(env, mk_sym("*"), mk_prim(prim_mul));
    tower_env_define(env, mk_sym("/"), mk_prim(prim_div));
    tower_env_define(env, mk_sym("mod"), mk_prim(prim_mod));
    tower_env_define(env, mk_sym("="), mk_prim(prim_eq));
    tower_env_define(env, mk_sym("<"), mk_prim(prim_lt));
    tower_env_define(env, mk_sym(">"), mk_prim(prim_gt));
    tower_env_define(env, mk_sym("cons"), mk_prim(prim_cons));
    tower_env_define(env, mk_sym("car"), mk_prim(prim_car));
    tower_env_define(env, mk_sym("cdr"), mk_prim(prim_cdr));
    tower_env_define(env, mk_sym("null?"), mk_prim(prim_null));
    tower_env_define(env, mk_sym("list"), mk_prim(prim_list));
    tower_env_define(env, mk_sym("not"), mk_prim(prim_not));
    tower_env_define(env, mk_sym("print"), mk_prim(prim_print));
    /* Boolean constants */
    tower_env_define(env, mk_sym("#t"), mk_sym("#t"));
    tower_env_define(env, mk_sym("#f"), mk_sym("#f"));

    return g_tower_menv;
}

void tower_cleanup(void) {
    if (g_tower_menv) {
        tower_menv_free(g_tower_menv);
        g_tower_menv = NULL;
    }
    g_gensym_counter = 0;
}
