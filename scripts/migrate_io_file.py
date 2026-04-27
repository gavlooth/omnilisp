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
        out_lines.append('fn Value* io_raise(Interp* interp, String code, char[] message) @inline {')
        out_lines.append('    return raise_error_with_payload_names(interp, "runtime", code, (String)message, null);')
        out_lines.append('}')
        out_lines.append('')
        inserted_helper = True
        continue
    out_lines.append(line)

src = '\n'.join(out_lines)

def classify_io_code(msg):
    msg = msg.strip('"')
    if 'expected' in msg:
        return 'type/arg-mismatch'
    if 'too long' in msg:
        return 'type/range-error'
    if 'out of memory' in msg or 'failed to allocate' in msg:
        return 'runtime/out-of-memory'
    if 'error while' in msg or 'cannot read' in msg:
        return 'runtime/evaluation-error'
    return 'runtime/evaluation-error'

# Pattern 1: raise_error(interp, "literal message")
def repl_literal(m):
    msg = m.group(1)
    code = classify_io_code(msg)
    return f'io_raise(interp, "{code}", {msg})'

src = re.sub(r'raise_error\(interp, ("[^"]+")\)', repl_literal, src)

# Pattern 2: raise_error(interp, msg) where msg is a char[] variable
src = re.sub(
    r'raise_error\(interp, msg\)',
    r'io_raise(interp, "runtime/evaluation-error", msg)',
    src
)

with open(path, 'w') as f:
    f.write(src)
