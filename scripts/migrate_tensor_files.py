#!/usr/bin/env python3
import re
from migrate_cli import single_path_arg

path = single_path_arg(__file__)
with open(path, 'r') as f:
    src = f.read()

# Use existing tensor_raise or tensor_runtime_raise
# For simple literal messages, classify and use tensor_raise

def classify_tensor_code(msg):
    msg = msg.strip('"')
    if 'out of memory' in msg or 'failed to allocate' in msg or 'too large' in msg:
        return 'tensor/out-of-memory'
    if 'expected' in msg or 'must be' in msg or 'invalid' in msg:
        return 'tensor/invalid-argument'
    if 'shape mismatch' in msg or 'shape-mismatch' in msg:
        return 'tensor/shape-mismatch'
    if 'dtype mismatch' in msg or 'dtype-mismatch' in msg:
        return 'tensor/dtype-mismatch'
    if 'backend unsupported' in msg or 'backend-unsupported' in msg:
        return 'tensor/backend-unsupported'
    if 'domain error' in msg or 'domain-error' in msg:
        return 'tensor/domain-error'
    if 'range error' in msg or 'range-error' in msg:
        return 'tensor/range-error'
    if 'unsupported' in msg:
        return 'tensor/unsupported'
    return 'tensor/invalid-argument'

# Pattern 1: raise_error(interp, "literal message")
def repl_literal(m):
    msg = m.group(1)
    code = classify_tensor_code(msg)
    return f'tensor_raise(interp, "{code}", {msg})'

src = re.sub(r'raise_error\(interp, ("[^"]+")\)', repl_literal, src)

# Pattern 2: raise_error(interp, variable_message)
src = re.sub(
    r'raise_error\(interp, ([a-zA-Z_][a-zA-Z0-9_]*)\)',
    r'tensor_raise(interp, "tensor/invalid-argument", \1)',
    src
)

with open(path, 'w') as f:
    f.write(src)
