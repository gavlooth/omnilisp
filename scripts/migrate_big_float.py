#!/usr/bin/env python3
import re
from migrate_cli import single_path_arg

path = single_path_arg(__file__)
with open(path, 'r') as f:
    src = f.read()

lines = src.split('\n')
out_lines = []
inserted_helper = False

for line in lines:
    if not inserted_helper and line.startswith('const int BIG_FLOAT_OP_ADD'):
        out_lines.append('fn Value* big_float_raise(Interp* interp, String code, char[] message) @inline {')
        out_lines.append('    return raise_error_with_payload_names(interp, "big-float", code, (String)message, null);')
        out_lines.append('}')
        out_lines.append('')
        inserted_helper = True
    out_lines.append(line)

src = '\n'.join(out_lines)

# Pattern 1: raise_error(interp, message) where message is a variable
src = re.sub(
    r'raise_error\(interp, message\)',
    r'big_float_raise(interp, "big-float/invalid-argument", message)',
    src
)

# Pattern 2: raise_error(interp, "BigFloat: ...")
def repl_bigfloat_literal(m):
    msg = m.group(1)
    if 'failed to allocate' in msg or 'failed to allocate' in msg:
        code = 'big-float/out-of-memory'
    elif 'failed to render' in msg:
        code = 'big-float/runtime-error'
    else:
        code = 'big-float/invalid-argument'
    return f'big_float_raise(interp, "{code}", {msg})'

src = re.sub(r'raise_error\(interp, ("BigFloat:[^"]+")\)', repl_bigfloat_literal, src)

# Pattern 3: boundary errors - use boundary_raise
src = re.sub(
    r'raise_error\(interp, ("boundary:[^"]+")\)',
    r'boundary_raise(interp, "boundary/out-of-memory", \1)',
    src
)

with open(path, 'w') as f:
    f.write(src)
