#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../ast/ast.h"
#include "../codegen/codegen.h"

/*
 * Issue 1 P2 regression test:
 *
 * Verify lambda codegen does NOT duplicate evaluation of the return
 * expression when escape-repair code is appended.
 *
 * Bug (pre-fix):
 * `codegen_lambda()` uses a temporary CodeGenContext buffer `tmp` to
 * generate:
 *   1) the return expression text
 *   2) the escape-repair statements (transmigrate/retain)
 *
 * But the buffer is not cleared between (1) and (2), so the second
 * `omni_codegen_get_output(tmp)` returns BOTH the expression statement
 * and the repair statement(s). The expression statement then appears
 * as a standalone statement in the generated lambda body, causing
 * the return expression to run twice.
 *
 * We detect this by compiling a lambda whose return expression has a
 * visible side effect (`(print 1)` emits `omni_print(...)` in C):
 * the generated lambda function must contain exactly one `omni_print(`
 * occurrence.
 */

static int tests_run;
static int tests_passed;

#define ASSERT_OR_DIE(cond)						\
	do {								\
		if (!(cond)) {						\
			fprintf(stderr,					\
				"\033[31mFAIL\033[0m:%d: %s\n",	\
				__LINE__, #cond);			\
			exit(1);					\
		}							\
	} while (0)

#define TEST(name) static void name(void)
#define RUN_TEST(name)							\
	do {								\
		printf("  %s: ", #name);				\
		fflush(stdout);						\
		name();							\
		tests_run++;						\
		tests_passed++;						\
		printf("\033[32mPASS\033[0m\n");			\
	} while (0)

static OmniValue *mk_sym(const char *name)
{
	return omni_new_sym(name);
}

static OmniValue *mk_int(long val)
{
	return omni_new_int(val);
}

static OmniValue *mk_cons(OmniValue *car, OmniValue *cdr)
{
	return omni_new_cell(car, cdr);
}

static OmniValue *mk_list1(OmniValue *a)
{
	return mk_cons(a, omni_nil);
}

static OmniValue *mk_list2(OmniValue *a, OmniValue *b)
{
	return mk_cons(a, mk_cons(b, omni_nil));
}

static OmniValue *mk_list3(OmniValue *a, OmniValue *b, OmniValue *c)
{
	return mk_cons(a, mk_cons(b, mk_cons(c, omni_nil)));
}

static size_t count_substr(const char *start, const char *end,
			   const char *needle)
{
	size_t count = 0;
	size_t nlen = strlen(needle);
	const char *p = start;

	if (!start || !end || !needle || nlen == 0)
		return 0;

	while (p < end) {
		const char *hit = strstr(p, needle);
		if (!hit || hit >= end)
			break;
		count++;
		p = hit + nlen;
	}

	return count;
}

TEST(test_lambda_return_expression_not_duplicated)
{
	/*
	 * Program:
	 *   (let ((f (lambda () (print 1))))
	 *     (f))
	 *
	 * The lambda return expression `(print 1)` generates `omni_print(...)`
	 * as part of the C expression. It must appear exactly once inside the
	 * lambda function definition.
	 */

	OmniValue *print_expr = mk_list2(mk_sym("print"), mk_int(1));
	OmniValue *params = omni_nil; /* no args */
	OmniValue *lam = mk_list3(mk_sym("lambda"), params, print_expr);

	OmniValue *binding = mk_list2(mk_sym("f"), lam);
	OmniValue *bindings = mk_cons(binding, omni_nil);

	OmniValue *call_f = mk_list1(mk_sym("f"));
	OmniValue *expr = mk_list3(mk_sym("let"), bindings, call_f);

	OmniValue *exprs[1] = { expr };

	CodeGenContext *cg = omni_codegen_new_buffer();
	ASSERT_OR_DIE(cg != NULL);

	omni_codegen_program(cg, exprs, 1);

	char *output = omni_codegen_get_output(cg);
	ASSERT_OR_DIE(output != NULL);

	const char *lambda_start = strstr(output, "static Obj* _lambda_");
	if (!lambda_start) {
		fprintf(stderr, "\n--- Generated output (prefix) ---\n");
		fprintf(stderr, "%.*s\n", 2000, output);
		fprintf(stderr, "--- End generated output ---\n");
		exit(1);
	}

	/*
	 * The generated lambda body is straight-line C in this case.
	 * Find the end of the function by locating the first "\n}\n"
	 * after the signature.
	 */
	const char *lambda_end = strstr(lambda_start, "\n}\n");
	ASSERT_OR_DIE(lambda_end != NULL);
	lambda_end += 3;

	/*
	 * Regression gate:
	 * `omni_print(` must appear exactly once in the lambda function.
	 * Pre-fix it appears twice due to a duplicated standalone statement.
	 */
	size_t prints = count_substr(lambda_start, lambda_end, "omni_print(");
	ASSERT_OR_DIE(prints == 1);

	free(output);
	omni_codegen_free(cg);
}

int main(void)
{
	printf("\n\033[33m=== Lambda Return Duplication Tests (Issue 1 P2) ===\033[0m\n");

	RUN_TEST(test_lambda_return_expression_not_duplicated);

	printf("\n\033[33m=== Summary ===\033[0m\n");
	printf("  Total:  %d\n", tests_run);
	printf("  \033[32mPassed: %d\033[0m\n", tests_passed);
	printf("  Failed: 0\n");

	return 0;
}
