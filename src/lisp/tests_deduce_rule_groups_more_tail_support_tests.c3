module lisp;

import std::io;

fn void run_deduce_rule_validation_tests_more_tail_support_prelude_tests(Interp* interp, int* pass, int* fail) {
    {
        DeduceTupleSupportTable table = {};
        defer deduce_tuple_support_table_release(&table);

        char[1] recursive_key = {'r'};
        char[1] base_key = {'b'};

        bool ok = true;
        long total = -1;
        bool became_present = false;
        bool became_absent = false;

        Value* err = deduce_tuple_support_table_adjust_recursive_count(
            &table,
            &recursive_key[0],
            1,
            1,
            &total,
            &became_present,
            &became_absent,
            interp
        );
        ok = err == null &&
             total == 1 &&
             became_present &&
             !became_absent &&
             deduce_tuple_support_table_total_count(&table, &recursive_key[0], 1, interp) == 1;

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                &recursive_key[0],
                1,
                1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null &&
                 total == 2 &&
                 !became_present &&
                 !became_absent &&
                 deduce_tuple_support_table_total_count(&table, &recursive_key[0], 1, interp) == 2;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                &recursive_key[0],
                1,
                -1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null &&
                 total == 1 &&
                 !became_present &&
                 !became_absent &&
                 deduce_tuple_support_table_total_count(&table, &recursive_key[0], 1, interp) == 1;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                &recursive_key[0],
                1,
                -1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null &&
                 total == 0 &&
                 !became_present &&
                 became_absent &&
                 deduce_tuple_support_table_total_count(&table, &recursive_key[0], 1, interp) == 0;
        }

        if (ok) {
            err = deduce_tuple_support_table_seed_base_tuple(
                &table,
                &base_key[0],
                1,
                &total,
                interp
            );
            ok = err == null &&
                 total == 1 &&
                 deduce_tuple_support_table_total_count(&table, &base_key[0], 1, interp) == 1;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                &base_key[0],
                1,
                1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null &&
                 total == 2 &&
                 !became_present &&
                 !became_absent &&
                 deduce_tuple_support_table_total_count(&table, &base_key[0], 1, interp) == 2;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                &base_key[0],
                1,
                -1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null &&
                 total == 1 &&
                 !became_present &&
                 !became_absent &&
                 deduce_tuple_support_table_total_count(&table, &base_key[0], 1, interp) == 1;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                &base_key[0],
                1,
                -1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err != null &&
                 deduce_tuple_support_table_total_count(&table, &base_key[0], 1, interp) == 1;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce tuple support tables track zero-crossing transitions");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce tuple support tables track zero-crossing transitions");
            (*fail)++;
        }
    }
}

fn void run_deduce_rule_validation_tests_more_tail_support_followup_tests(Interp* interp, int* pass, int* fail) {
    {
        DeduceTupleSupportTable table = {};
        defer deduce_tuple_support_table_release(&table);

        char[] recursive_key = "recursive-support";
        char[] seeded_key = "seeded-support";
        bool ok = true;
        long total = 0;
        bool became_present = false;
        bool became_absent = false;

        Value* err = deduce_tuple_support_table_adjust_recursive_count(
            &table,
            recursive_key.ptr,
            recursive_key.len,
            1,
            &total,
            &became_present,
            &became_absent,
            interp
        );
        ok = err == null && total == 1 && became_present && !became_absent;

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                recursive_key.ptr,
                recursive_key.len,
                1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null && total == 2 && !became_present && !became_absent;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                recursive_key.ptr,
                recursive_key.len,
                -1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null && total == 1 && !became_present && !became_absent;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                recursive_key.ptr,
                recursive_key.len,
                -1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null && total == 0 && !became_present && became_absent;
        }

        if (ok) {
            err = deduce_tuple_support_table_seed_base_tuple(
                &table,
                seeded_key.ptr,
                seeded_key.len,
                &total,
                interp
            );
            ok = err == null && total == 1 &&
                 deduce_tuple_support_table_total_count(&table, seeded_key.ptr, seeded_key.len, interp) == 1;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                seeded_key.ptr,
                seeded_key.len,
                1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null && total == 2 && !became_present && !became_absent;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                seeded_key.ptr,
                seeded_key.len,
                -1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null && total == 1 && !became_present && !became_absent &&
                 deduce_tuple_support_table_total_count(&table, seeded_key.ptr, seeded_key.len, interp) == 1;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce tuple support tables preserve zero-crossing semantics");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce tuple support tables preserve zero-crossing semantics");
            (*fail)++;
        }
    }

    {
        DeduceAggregateGroups groups = {};
        defer deduce_aggregate_groups_release(&groups);

        DeduceRuleTermKind[2] head_kinds = {
            DEDUCE_RULE_TERM_VARIABLE,
            DEDUCE_RULE_TERM_AGGREGATE
        };
        DeduceAggregateOpKind[2] aggregate_ops = {
            DEDUCE_AGGREGATE_NONE,
            DEDUCE_AGGREGATE_COUNT
        };
        uint[2] head_var_ids = { 0, 1 };
        Value*[2] head_literals = { null, null };
        DeduceRuleSignature sig = {
            .head_term_kinds = &head_kinds[0],
            .head_term_aggregate_ops = &aggregate_ops[0],
            .head_term_var_ids = &head_var_ids[0],
            .head_term_literals = &head_literals[0],
            .head_term_count = 2,
        };

        bool[2] var_bound = { true, true };
        Value*[2] var_values = {
            make_string(interp, "g"),
            make_int(interp, 7)
        };
        DeduceAggregateGroup* group = null;
        bool ok = deduce_aggregate_group_find_or_add(
            &groups,
            &sig,
            var_bound[..],
            var_values[..],
            &group,
            interp
        ) == null && group != null;

        if (ok) {
            ok = deduce_aggregate_group_note_binding_with_polarity(
                group,
                &sig,
                var_bound[..],
                var_values[..],
                false,
                interp
            ) == null &&
            group.support_count == 1 &&
            group.term_states[1].initialized &&
            group.term_states[1].count_value == 1;
        }

        if (ok) {
            ok = deduce_aggregate_group_note_binding_with_polarity(
                group,
                &sig,
                var_bound[..],
                var_values[..],
                false,
                interp
            ) == null &&
            group.support_count == 2 &&
            group.term_states[1].count_value == 2;
        }

        if (ok) {
            ok = deduce_aggregate_group_note_binding_with_polarity(
                group,
                &sig,
                var_bound[..],
                var_values[..],
                true,
                interp
            ) == null &&
            group.support_count == 1 &&
            group.term_states[1].initialized &&
            group.term_states[1].count_value == 1;
        }

        if (ok) {
            ok = deduce_aggregate_group_note_binding_with_polarity(
                group,
                &sig,
                var_bound[..],
                var_values[..],
                true,
                interp
            ) == null &&
            group.support_count == 0 &&
            !group.term_states[1].initialized &&
            group.term_states[1].count_value == 0;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce aggregate count groups support signed add/remove transitions");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce aggregate count groups support signed add/remove transitions");
            (*fail)++;
        }
    }

    {
        DeduceAggregateGroups groups = {};
        defer deduce_aggregate_groups_release(&groups);

        DeduceRuleTermKind[2] head_kinds = {
            DEDUCE_RULE_TERM_VARIABLE,
            DEDUCE_RULE_TERM_AGGREGATE
        };
        DeduceAggregateOpKind[2] aggregate_ops = {
            DEDUCE_AGGREGATE_NONE,
            DEDUCE_AGGREGATE_MIN
        };
        uint[2] head_var_ids = { 0, 1 };
        Value*[2] head_literals = { null, null };
        DeduceRuleSignature sig = {
            .head_term_kinds = &head_kinds[0],
            .head_term_aggregate_ops = &aggregate_ops[0],
            .head_term_var_ids = &head_var_ids[0],
            .head_term_literals = &head_literals[0],
            .head_term_count = 2,
        };

        bool[2] var_bound = { true, true };
        Value*[2] low_values = {
            make_string(interp, "g"),
            make_int(interp, 2)
        };
        Value*[2] high_values = {
            make_string(interp, "g"),
            make_int(interp, 5)
        };
        DeduceAggregateGroup* group = null;
        bool ok = deduce_aggregate_group_find_or_add(
            &groups,
            &sig,
            var_bound[..],
            low_values[..],
            &group,
            interp
        ) == null && group != null;

        if (ok) {
            ok = deduce_aggregate_group_note_binding_with_polarity(
                group,
                &sig,
                var_bound[..],
                high_values[..],
                false,
                interp
            ) == null &&
            deduce_aggregate_group_note_binding_with_polarity(
                group,
                &sig,
                var_bound[..],
                low_values[..],
                false,
                interp
            ) == null &&
            group.support_count == 2 &&
            group.term_states[1].initialized &&
            group.term_states[1].extremum_value != null &&
            group.term_states[1].extremum_value.tag == INT &&
            group.term_states[1].extremum_value.int_val == 2;
        }

        if (ok) {
            ok = deduce_aggregate_group_note_binding_with_polarity(
                group,
                &sig,
                var_bound[..],
                low_values[..],
                true,
                interp
            ) == null &&
            group.support_count == 1 &&
            group.term_states[1].initialized &&
            group.term_states[1].extremum_value != null &&
            group.term_states[1].extremum_value.tag == INT &&
            group.term_states[1].extremum_value.int_val == 5;
        }

        if (ok) {
            ok = deduce_aggregate_group_note_binding_with_polarity(
                group,
                &sig,
                var_bound[..],
                high_values[..],
                true,
                interp
            ) == null &&
            group.support_count == 0 &&
            !group.term_states[1].initialized &&
            group.term_states[1].extremum_value == null;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce aggregate min groups recompute extrema on removals");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce aggregate min groups recompute extrema on removals");
            (*fail)++;
        }
    }
}
