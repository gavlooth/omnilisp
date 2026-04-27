#!/usr/bin/env python3
import re
from migrate_cli import rewrite_file, single_path_arg

path = single_path_arg(__file__)
with open(path, 'r', encoding='utf-8') as f:
    src = f.read()

# Fix all data_format_raise(interp,\n    "message");
# to data_format_raise(interp, "type/arg-mismatch",\n    "message");
pattern = r'data_format_raise\(interp,\s*\n\s*("[^"]+")\)'

def repl(m):
    return f'data_format_raise(interp, "type/arg-mismatch",\n    {m.group(1)})'

src = re.sub(pattern, repl, src)

rewrite_file(path, src)
