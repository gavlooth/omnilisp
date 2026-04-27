#!/usr/bin/env python3
import re
from migrate_cli import path_args, rewrite_file

HELPER_PATTERN = re.compile(
    r'\n?fn Value\* jit_raise\([^)]*\) @inline \{\n'
    r'    return raise_error_with_payload_names\([^;]+;\n'
    r'\}\n?',
    re.MULTILINE,
)


for path in path_args(__file__):
    with open(path, 'r', encoding='utf-8') as f:
        src = f.read()
    src = HELPER_PATTERN.sub('\n', src)
    rewrite_file(path, src)
