/*
 * test_dict_insert_autorepair.c - Store barrier coverage for dict insert path
 *
 * Objective:
 *   Ensure `dict_set()` uses the store barrier (omni_store_repair) for
 *   NEW key insertion, not only for existing-key updates.
 *
 * Why this matters:
 *   `Dict` is a region-resident container (`mk_dict_region(r)` allocates the
 *   Dict + bucket array inside region `r`). If we insert a value allocated
 *   in a younger region into a dict allocated in an older region without
 *   repair, we create a pointer into a region that can die earlier, violating
 *   the Region Closure Property.
 *
 * Test strategy:
 *   1) Create an older region `dst` and allocate a dict in it.
 *   2) Create a younger region `src` and allocate a boxed pair in it.
 *   3) Insert that value into the dict under a stable key.
 *   4) Assert that the stored value is now owned by `dst` (transmigrated),
 *      so it can outlive `src`.
 *
 * This test should FAIL before the fix and PASS after.
 */

#include "test_framework.h"

void run_dict_insert_autorepair_tests(void)
{
	TEST_SUITE("Dict Insert Autorepair Tests (Issue 2 P4.4)");

	TEST("dict_set insert repairs younger value into older dict");
	{
		Region* dst = region_create();
		ASSERT_NOT_NULL(dst);
		dst->lifetime_rank = 0;

		Region* src = region_create();
		ASSERT_NOT_NULL(src);
		src->lifetime_rank = 1;

		Obj* dict = mk_dict_region(dst);
		ASSERT_NOT_NULL(dict);
		ASSERT(omni_obj_region(dict) == dst);

		/*
		 * Use a stable key. `mk_sym()` allocates in the global region,
		 * avoiding the separate question of whether dict keys should be
		 * repaired or required to be immortal/interned.
		 */
		Obj* key = mk_sym("k");
		ASSERT_NOT_NULL(key);

		Obj* young_value = mk_cell_region(src,
						  mk_int_region(src, 1),
						  mk_int_region(src, 2));
		ASSERT_NOT_NULL(young_value);
		ASSERT(omni_obj_region(young_value) == src);

		/*
		 * Insert path: this must run through the store barrier so the
		 * dict doesn't retain a pointer into `src`.
		 */
		dict_set(dict, key, young_value);

		Obj* got = dict_get(dict, key);
		ASSERT_NOT_NULL(got);

		/*
		 * After repair, the stored value must be in the dict's region.
		 * Pre-fix, this is `src` and the assertion fails immediately.
		 */
		ASSERT(omni_obj_region(got) == dst);

		region_exit(src);
		region_destroy_if_dead(src);

		/*
		 * The value should remain accessible after the source region is
		 * destroyed (it was transmigrated or otherwise repaired).
		 */
		got = dict_get(dict, key);
		ASSERT_NOT_NULL(got);
		ASSERT(omni_obj_region(got) == dst);

		region_exit(dst);
		region_destroy_if_dead(dst);
		PASS();
	}
}

