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
    if not inserted_helper and line.startswith('module lisp;'):
        out_lines.append(line)
        out_lines.append('')
        out_lines.append('fn Value* big_complex_raise(Interp* interp, String code, char[] message) @inline {')
        out_lines.append('    return raise_error_with_payload_names(interp, "big-complex", code, (String)message, null);')
        out_lines.append('}')
        out_lines.append('')
        inserted_helper = True
        continue
    out_lines.append(line)

src = '\n'.join(out_lines)

# Pattern 1: raise_error(interp, message) where message is a variable
src = re.sub(
    r'raise_error\(interp, message\)',
    r'big_complex_raise(interp, "big-complex/invalid-argument", message)',
    src
)

# Pattern 2: raise_error(interp, "BigComplex: ...")
def repl_bigcomplex_literal(m):
    msg = m.group(1)
    if 'failed to allocate' in msg:
        code = 'big-complex/out-of-memory'
    else:
        code = 'big-complex/invalid-argument'
    return f'big_complex_raise(interp, "{code}", {msg})'

src = re.sub(r'raise_error\(interp, ("BigComplex:[^"]+")\)', repl_bigcomplex_literal, src)

# Pattern 3: boundary errors - use boundary_raise
src = re.sub(
    r'raise_error\(interp, ("boundary:[^"]+")\)',
    r'boundary_raise(interp, "boundary/out-of-memory", \1)',
    src
)

with open(path, 'w') as f:
    f.write(src)
