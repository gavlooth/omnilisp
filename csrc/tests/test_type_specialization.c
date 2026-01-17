/*
 * Type Specialization Tests
 *
 * Tests for Phase 1 type specialization - generating specialized code
 * for arithmetic operations when operand types are statically known.
 *
 * Verifies:
 * - Integer-Integer specialization (prim_add_Int_Int, etc.)
 * - Float-Float specialization (prim_add_Float_Float, etc.)
 * - Mixed Int-Float specialization
 * - Comparison operator specialization
 * - Nested expression specialization
 */

#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../ast/ast.h"
#include "../analysis/analysis.h"
#include "../codegen/codegen.h"

/* Test counters */
static int tests_run = 0;
static int tests_passed = 0;

#define TEST(name) static void name(void)
#define RUN_TEST(name) do { \
    printf("  %s: ", #name); \
    name(); \
    tests_run++; \
    tests_passed++; \
    printf("\033[32mPASS\033[0m\n"); \
} while(0)

#define ASSERT(cond) do { \
    if (!(cond)) { \
        printf("\033[31mFAIL\033[0m (line %d: %s)\n", __LINE__, #cond); \
        tests_run++; \
        return; \
    } \
} while(0)

#define ASSERT_CONTAINS(haystack, needle) do { \
    if (!strstr((haystack), (needle))) { \
        printf("\033[31mFAIL\033[0m (line %d: expected '%s' in output)\n", __LINE__, needle); \
        tests_run++; \
        return; \
    } \
} while(0)

#define ASSERT_NOT_CONTAINS(haystack, needle) do { \
    if (strstr((haystack), (needle))) { \
        printf("\033[31mFAIL\033[0m (line %d: unexpected '%s' in output)\n", __LINE__, needle); \
        tests_run++; \
        return; \
    } \
} while(0)

/* Check that a generic function is not CALLED (ignoring its definition in header)
 * Calls look like: prim_add(mk_  or prim_add(o_  or prim_add(box_
 * Definition looks like: prim_add(Obj*
 */
static int has_generic_call(const char* output, const char* func) {
    char call_pattern1[64], call_pattern2[64], call_pattern3[64];
    snprintf(call_pattern1, sizeof(call_pattern1), "%s(mk_", func);
    snprintf(call_pattern2, sizeof(call_pattern2), "%s(o_", func);
    snprintf(call_pattern3, sizeof(call_pattern3), "%s(box_", func);
    return strstr(output, call_pattern1) || strstr(output, call_pattern2) || strstr(output, call_pattern3);
}

#define ASSERT_NO_GENERIC_CALL(haystack, func) do { \
    if (has_generic_call((haystack), (func))) { \
        printf("\033[31mFAIL\033[0m (line %d: found call to generic '%s')\n", __LINE__, func); \
        tests_run++; \
        return; \
    } \
} while(0)

/* Helper to create AST nodes */
static OmniValue* mk_sym(const char* name) {
    return omni_new_sym(name);
}

static OmniValue* mk_int(long val) {
    return omni_new_int(val);
}

static OmniValue* mk_float(double val) {
    return omni_new_float(val);
}

static OmniValue* mk_cons(OmniValue* car, OmniValue* cdr) {
    return omni_new_cell(car, cdr);
}

static OmniValue* mk_list3(OmniValue* a, OmniValue* b, OmniValue* c) {
    return mk_cons(a, mk_cons(b, mk_cons(c, omni_nil)));
}

/* ========== Integer-Integer Specialization ========== */

TEST(test_int_int_add_specialization) {
    CodeGenContext* ctx = omni_codegen_new_buffer();
    ctx->analysis = omni_analysis_new();

    /* Build: (+ 3 4) */
    OmniValue* expr = mk_list3(mk_sym("+"), mk_int(3), mk_int(4));

    /* Generate complete program */
    omni_codegen_program(ctx, &expr, 1);

    char* output = omni_codegen_get_output(ctx);
    ASSERT(output != NULL);

    /* Should use specialized Int_Int function */
    ASSERT_CONTAINS(output, "prim_add_Int_Int");
    ASSERT_CONTAINS(output, "box_int");
    ASSERT_CONTAINS(output, "unbox_int");

    /* Should NOT call generic prim_add (definition in header is OK) */
    ASSERT_NO_GENERIC_CALL(output, "prim_add");

    free(output);
    omni_codegen_free(ctx);
}

TEST(test_int_int_sub_specialization) {
    CodeGenContext* ctx = omni_codegen_new_buffer();
    ctx->analysis = omni_analysis_new();

    /* Build: (- 10 3) */
    OmniValue* expr = mk_list3(mk_sym("-"), mk_int(10), mk_int(3));

    omni_codegen_program(ctx, &expr, 1);
    char* output = omni_codegen_get_output(ctx);
    ASSERT(output != NULL);

    ASSERT_CONTAINS(output, "prim_sub_Int_Int");
    ASSERT_NO_GENERIC_CALL(output, "prim_sub");

    free(output);
    omni_codegen_free(ctx);
}

TEST(test_int_int_mul_specialization) {
    CodeGenContext* ctx = omni_codegen_new_buffer();
    ctx->analysis = omni_analysis_new();

    /* Build: (* 5 6) */
    OmniValue* expr = mk_list3(mk_sym("*"), mk_int(5), mk_int(6));

    omni_codegen_program(ctx, &expr, 1);
    char* output = omni_codegen_get_output(ctx);
    ASSERT(output != NULL);

    ASSERT_CONTAINS(output, "prim_mul_Int_Int");
    ASSERT_NO_GENERIC_CALL(output, "prim_mul");

    free(output);
    omni_codegen_free(ctx);
}

TEST(test_int_int_comparison_specialization) {
    CodeGenContext* ctx = omni_codegen_new_buffer();
    ctx->analysis = omni_analysis_new();

    /* Build: (< 5 10) */
    OmniValue* expr = mk_list3(mk_sym("<"), mk_int(5), mk_int(10));

    omni_codegen_program(ctx, &expr, 1);
    char* output = omni_codegen_get_output(ctx);
    ASSERT(output != NULL);

    ASSERT_CONTAINS(output, "prim_lt_Int_Int");
    ASSERT_CONTAINS(output, "box_bool");  /* Comparisons return bool */
    ASSERT_NO_GENERIC_CALL(output, "prim_lt");

    free(output);
    omni_codegen_free(ctx);
}

/* ========== Float-Float Specialization ========== */

TEST(test_float_float_add_specialization) {
    CodeGenContext* ctx = omni_codegen_new_buffer();
    ctx->analysis = omni_analysis_new();

    /* Build: (+ 1.5 2.5) */
    OmniValue* expr = mk_list3(mk_sym("+"), mk_float(1.5), mk_float(2.5));

    omni_codegen_program(ctx, &expr, 1);
    char* output = omni_codegen_get_output(ctx);
    ASSERT(output != NULL);

    ASSERT_CONTAINS(output, "prim_add_Float_Float");
    ASSERT_CONTAINS(output, "box_float");
    ASSERT_CONTAINS(output, "unbox_float");
    ASSERT_NO_GENERIC_CALL(output, "prim_add");

    free(output);
    omni_codegen_free(ctx);
}

/* ========== Mixed Int-Float Specialization ========== */

TEST(test_mixed_int_float_specialization) {
    CodeGenContext* ctx = omni_codegen_new_buffer();
    ctx->analysis = omni_analysis_new();

    /* Build: (+ 3 1.5) */
    OmniValue* expr = mk_list3(mk_sym("+"), mk_int(3), mk_float(1.5));

    omni_codegen_program(ctx, &expr, 1);
    char* output = omni_codegen_get_output(ctx);
    ASSERT(output != NULL);

    ASSERT_CONTAINS(output, "prim_add_Int_Float");
    ASSERT_CONTAINS(output, "box_float");  /* Mixed returns float */

    free(output);
    omni_codegen_free(ctx);
}

/* ========== Nested Expression Specialization ========== */

TEST(test_nested_expression_specialization) {
    CodeGenContext* ctx = omni_codegen_new_buffer();
    ctx->analysis = omni_analysis_new();

    /* Build: (* (+ 2 3) 4) */
    OmniValue* inner = mk_list3(mk_sym("+"), mk_int(2), mk_int(3));
    OmniValue* expr = mk_list3(mk_sym("*"), inner, mk_int(4));

    omni_codegen_program(ctx, &expr, 1);
    char* output = omni_codegen_get_output(ctx);
    ASSERT(output != NULL);

    /* Both operations should be specialized */
    ASSERT_CONTAINS(output, "prim_add_Int_Int");
    ASSERT_CONTAINS(output, "prim_mul_Int_Int");

    free(output);
    omni_codegen_free(ctx);
}

/* ========== Fallback to Generic ========== */

TEST(test_unknown_type_falls_back) {
    CodeGenContext* ctx = omni_codegen_new_buffer();
    ctx->analysis = omni_analysis_new();

    /* Build: (+ x y) where x and y are variables (unknown types) */
    OmniValue* expr = mk_list3(mk_sym("+"), mk_sym("x"), mk_sym("y"));

    omni_codegen_program(ctx, &expr, 1);
    char* output = omni_codegen_get_output(ctx);
    ASSERT(output != NULL);

    /* Should CALL generic prim_add since types are unknown */
    ASSERT(has_generic_call(output, "prim_add"));
    /* Should NOT use specialized version */
    ASSERT_NOT_CONTAINS(output, "prim_add_Int_Int");

    free(output);
    omni_codegen_free(ctx);
}

/* ========== Main ========== */

int main(void) {
    printf("Type Specialization Tests\n");
    printf("==========================\n\n");

    printf("Integer-Integer Specialization:\n");
    RUN_TEST(test_int_int_add_specialization);
    RUN_TEST(test_int_int_sub_specialization);
    RUN_TEST(test_int_int_mul_specialization);
    RUN_TEST(test_int_int_comparison_specialization);

    printf("\nFloat-Float Specialization:\n");
    RUN_TEST(test_float_float_add_specialization);

    printf("\nMixed Int-Float Specialization:\n");
    RUN_TEST(test_mixed_int_float_specialization);

    printf("\nNested Expression Specialization:\n");
    RUN_TEST(test_nested_expression_specialization);

    printf("\nFallback to Generic:\n");
    RUN_TEST(test_unknown_type_falls_back);

    printf("\n==========================\n");
    printf("Results: %d/%d tests passed\n", tests_passed, tests_run);

    return tests_passed == tests_run ? 0 : 1;
}
