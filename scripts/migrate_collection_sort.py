#!/usr/bin/env python3
import re
from migrate_cli import single_path_arg

path = single_path_arg(__file__)
with open(path, 'r') as f:
    src = f.read()

def classify_collection_code(msg):
    msg = msg.strip('"')
    if 'expected' in msg and ('argument' in msg or 'expected 1' in msg or 'expected 2' in msg or 'expected 3' in msg or 'expected list' in msg or 'expected array' in msg):
        return 'type/arity' if 'expected 1 argument' in msg or 'expected 2' in msg or 'expected 3' in msg else 'type/arg-mismatch'
    if 'out of memory' in msg or 'out of range' in msg or 'too long' in msg or 'cyclic' in msg:
        return 'type/range-error'
    if 'index out of range' in msg:
        return 'type/range-error'
    if 'expected a proper list' in msg:
        return 'type/arg-mismatch'
    if 'comparator application failed' in msg:
        return 'runtime/evaluation-error'
    return 'type/arg-mismatch'

# Pattern 1: raise_error(interp, "literal message")
def repl_literal(m):
    msg = m.group(1)
    code = classify_collection_code(msg)
    return f'collection_raise(interp, "{code}", {msg})'

src = re.sub(r'raise_error\(interp, ("[^"]+")\)', repl_literal, src)

with open(path, 'w') as f:
    f.write(src)
