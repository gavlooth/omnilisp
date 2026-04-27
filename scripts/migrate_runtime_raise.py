#!/usr/bin/env python3
import re
from migrate_cli import single_path_arg

path = single_path_arg(__file__)
with open(path, 'r') as f:
    src = f.read()

# Only add helper if it's not already present in the file
if 'fn Value* runtime_raise(' not in src:
    lines = src.split('\n')
    out_lines = []
    inserted_helper = False
    for line in lines:
        if not inserted_helper and line.startswith('module lisp;'):
            out_lines.append(line)
            out_lines.append('')
            out_lines.append('fn Value* runtime_raise(Interp* interp, String code, char[] message) @inline {')
            out_lines.append('    return raise_error_with_payload_names(interp, "runtime", code, (String)message, null);')
            out_lines.append('}')
            out_lines.append('')
            inserted_helper = True
            continue
        out_lines.append(line)
    src = '\n'.join(out_lines)

def classify_runtime_code(msg):
    msg = msg.strip('"')
    if 'out of memory' in msg or 'failed to allocate' in msg:
        return 'runtime/out-of-memory'
    if 'expected' in msg and ('argument' in msg or 'expected 1' in msg or 'expected 2' in msg):
        return 'type/arity'
    if 'must be' in msg or 'expected' in msg or 'invalid' in msg:
        return 'type/arg-mismatch'
    if 'failed' in msg or 'missing' in msg:
        return 'runtime/invalid-state'
    return 'runtime/evaluation-error'

# Pattern 1: raise_error(interp, "literal message")
def repl_literal(m):
    msg = m.group(1)
    code = classify_runtime_code(msg)
    return f'runtime_raise(interp, "{code}", {msg})'

src = re.sub(r'raise_error\(interp, ("[^"]+")\)', repl_literal, src)

# Pattern 2: raise_error(interp, msg) where msg is a char[] variable
src = re.sub(
    r'raise_error\(interp, msg\)',
    r'runtime_raise(interp, "runtime/evaluation-error", msg)',
    src
)

# Pattern 3: raise_error(interp, boundary_copy_fault_message(...))
src = re.sub(
    r'raise_error\(interp, (boundary_copy_fault_message\([^)]+\))\)',
    r'runtime_raise(interp, "boundary/copy-fault", (String)\1)',
    src
)

with open(path, 'w') as f:
    f.write(src)
