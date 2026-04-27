#!/usr/bin/env python3
import re
from migrate_cli import single_path_arg

path = single_path_arg(__file__)
with open(path, 'r') as f:
    src = f.read()

def classify_jit_code(msg):
    msg = msg.strip('"')
    if 'out of memory' in msg:
        return 'runtime/out-of-memory'
    if 'unbound' in msg or 'not found' in msg or 'did not match' in msg:
        return 'runtime/evaluation-error'
    if 'only supports' in msg or 'not an' in msg or 'failed' in msg:
        return 'type/arg-mismatch'
    return 'runtime/evaluation-error'

# Pattern 1: raise_error(interp, "literal message")
def repl_literal(m):
    msg = m.group(1)
    code = classify_jit_code(msg)
    return f'jit_raise(interp, "{code}", {msg})'

src = re.sub(r'raise_error\(interp, ("[^"]+")\)', repl_literal, src)

# Pattern 2: raise_error(interp, boundary_copy_fault_message(...))
src = re.sub(
    r'raise_error\(interp, (boundary_copy_fault_message\([^)]+\))\)',
    r'jit_raise(interp, "boundary/copy-fault", (String)\1)',
    src
)

with open(path, 'w') as f:
    f.write(src)
