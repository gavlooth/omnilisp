#!/usr/bin/env python3
import os
import re

# Files that still have raw raise_error calls
files = [
    'src/lisp/eval_promotion_root_store.c3',
    'src/lisp/value_environment_barrier.c3',
    'src/lisp/tests_memory_lifetime_runtime_alloc_groups_schema.c3',
    'src/lisp/tests_memory_lifetime_runtime_alloc_groups_core.c3',
    'src/lisp/tests_memory_lifetime_runtime_alloc_groups_apply_coroutine.c3',
    'src/lisp/tests_harness_helpers.c3',
    'src/lisp/schema_explain_effect.c3',
    'src/lisp/prim_tensor_map.c3',
    'src/lisp/prim_string_format.c3',
    'src/lisp/json_pointer_option_helpers.c3',
    'src/lisp/jit_define_method_table.c3',
    'src/lisp/jit_closure_runtime.c3',
    'src/lisp/eval_type_instance_builder.c3',
    'src/lisp/eval_promotion_root_clones.c3',
    'src/lisp/eval_promotion_root_clone_basic.c3',
    'src/lisp/eval_dispatch_types.c3',
    'src/lisp/eval_dispatch_error_payloads.c3',
    'src/lisp/eval_boundary_commit_flow.c3',
    'src/lisp/eval_boundary_commit_escape_cons.c3',
    'src/lisp/value_constructors.c3',
    'src/lisp/tests_runtime_feature_jit_groups_failures.c3',
    'src/lisp/tests_memory_lifetime_runtime_alloc_groups_data_parsing.c3',
    'src/lisp/tests_memory_lifetime_groups.c3',
    'src/lisp/schema_validation.c3',
    'src/lisp/schema_explain_payload_helpers.c3',
    'src/lisp/schema_explain_helpers.c3',
    'src/lisp/prim_ui_ftxui_lowering.c3',
    'src/lisp/prim_tensor_validation.c3',
    'src/lisp/prim_tensor_expr.c3',
    'src/lisp/prim_system.c3',
    'src/lisp/prim_runtime_memory_stats.c3',
    'src/lisp/prim_math.c3',
    'src/lisp/primitives_meta_types.c3',
    'src/lisp/primitives_iter_terminal.c3',
    'src/lisp/primitives_data_formats_csv_options.c3',
    'src/lisp/prim_io.c3',
    'src/lisp/prim_collection_hashmap.c3',
    'src/lisp/jit_eval_scope_helpers.c3',
    'src/lisp/jit_compile_expr_basic.c3',
    'src/lisp/eval_signal.c3',
]

for path in files:
    if not os.path.exists(path):
        continue
    with open(path, 'r') as f:
        src = f.read()

    # Skip if no raw raise_error calls
    if 'raise_error(' not in src or 'raise_error_with_payload_names' in src.split('raise_error(')[0]:
        if len(re.findall(r'raise_error\(', src)) == 0:
            continue

    # Pattern 1: raise_error(interp, "literal message")
    def repl_literal(m):
        msg = m.group(1)
        text = msg.strip('"')
        if 'out of memory' in text or 'failed to allocate' in text:
            code = 'runtime/out-of-memory'
        elif 'expected' in text and ('argument' in text or 'expected 1' in text or 'expected 2' in text):
            code = 'type/arity'
        elif 'expected' in text or 'must be' in text or 'invalid' in text:
            code = 'type/arg-mismatch'
        elif 'boundary' in text:
            code = 'boundary/copy-fault'
        elif 'jit' in text:
            code = 'runtime/evaluation-error'
        else:
            code = 'runtime/evaluation-error'
        return f'runtime_raise(interp, "{code}", {msg})'

    src = re.sub(r'raise_error\(interp, ("[^"]+")\)', repl_literal, src)

    # Pattern 2: raise_error(interp, msg)
    src = re.sub(
        r'raise_error\(interp, msg\)',
        r'runtime_raise(interp, "runtime/evaluation-error", msg)',
        src
    )

    # Pattern 3: raise_error(interp, emsg)
    src = re.sub(
        r'raise_error\(interp, emsg\)',
        r'runtime_raise(interp, "runtime/evaluation-error", emsg)',
        src
    )

    # Pattern 4: raise_error(interp, buf[:len])
    src = re.sub(
        r'raise_error\(interp, buf\[:len\]\)',
        r'runtime_raise(interp, "runtime/evaluation-error", buf[:len])',
        src
    )

    # Pattern 5: raise_error(interp, boundary_copy_fault_message(...))
    src = re.sub(
        r'raise_error\(interp, (boundary_copy_fault_message\([^)]+\))\)',
        r'boundary_raise(interp, "boundary/copy-fault", (String)\1)',
        src
    )

    # Pattern 6: raise_error(interp, jit_env_copy_fault_message(...))
    src = re.sub(
        r'raise_error\(interp, (jit_env_copy_fault_message\([^)]+\))\)',
        r'jit_raise(interp, "boundary/copy-fault", (String)\1)',
        src
    )

    with open(path, 'w') as f:
        f.write(src)
