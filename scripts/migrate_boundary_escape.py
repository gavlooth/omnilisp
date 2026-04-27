#!/usr/bin/env python3
import re
from migrate_cli import single_path_arg

def classify_boundary_code(msg):
    msg = msg.strip('"')
    if 'failed to allocate' in msg:
        return 'boundary/out-of-memory'
    if 'refusing' in msg:
        return 'boundary/invalid-state'
    return 'boundary/copy-fault'

path = single_path_arg(__file__)
with open(path, 'r') as f:
    src = f.read()

# Pattern 1: raise_error(interp, "literal message")
def repl_literal(m):
    msg = m.group(1)
    code = classify_boundary_code(msg)
    return f'boundary_raise(interp, "{code}", {m.group(1)})'

src = re.sub(r'raise_error\(interp, ("boundary:[^"]+")\)', repl_literal, src)

# Pattern 2: raise_error(interp, boundary_copy_fault_message(...))
src = re.sub(
    r'raise_error\(interp, (boundary_copy_fault_message\([^)]+\))\)',
    r'boundary_raise(interp, "boundary/copy-fault", (String)\1)',
    src
)

with open(path, 'w') as f:
    f.write(src)
