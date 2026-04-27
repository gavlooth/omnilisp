#!/usr/bin/env python3
import re
from migrate_cli import single_path_arg

path = single_path_arg(__file__)
with open(path, 'r') as f:
    src = f.read()

def classify_ffi_code(msg):
    msg = msg.strip('"')
    if 'expected' in msg:
        return 'type/arg-mismatch'
    if 'too many' in msg:
        return 'type/arity'
    if 'out of memory' in msg:
        return 'ffi/out-of-memory'
    if 'unavailable' in msg or 'could not resolve' in msg:
        return 'ffi/unavailable'
    return 'ffi/invalid-state'

# Pattern 1: raise_error(interp, "literal message")
def repl_literal(m):
    msg = m.group(1)
    code = classify_ffi_code(msg)
    return f'ffi_raise(interp, "{code}", {msg})'

src = re.sub(r'raise_error\(interp, ("[^"]+")\)', repl_literal, src)

# Pattern 2: raise_error(interp, msg) where msg is a char[] variable
src = re.sub(
    r'raise_error\(interp, msg\)',
    r'ffi_raise(interp, "ffi/invalid-state", msg)',
    src
)

with open(path, 'w') as f:
    f.write(src)
