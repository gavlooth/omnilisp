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
        out_lines.append('fn Value* string_raise(Interp* interp, String code, char[] message) @inline {')
        out_lines.append('    return raise_error_with_payload_names(interp, "type", code, (String)message, null);')
        out_lines.append('}')
        out_lines.append('')
        inserted_helper = True
        continue
    out_lines.append(line)

src = '\n'.join(out_lines)

def classify_string_code(msg):
    msg = msg.strip('"')
    if 'expected' in msg and ('argument' in msg or 'expected 1' in msg or 'expected 2' in msg):
        return 'type/arity'
    if 'out of memory' in msg or 'failed to allocate' in msg:
        return 'type/out-of-memory'
    if 'too large' in msg or 'too long' in msg:
        return 'type/range-error'
    if 'expected' in msg or 'must be' in msg:
        return 'type/arg-mismatch'
    return 'type/arg-mismatch'

# Pattern 1: raise_error(interp, "literal message")
def repl_literal(m):
    msg = m.group(1)
    code = classify_string_code(msg)
    return f'string_raise(interp, "{code}", {msg})'

src = re.sub(r'raise_error\(interp, ("[^"]+")\)', repl_literal, src)

# Pattern 2: raise_error(interp, variable_message)
src = re.sub(
    r'raise_error\(interp, ([a-zA-Z_][a-zA-Z0-9_]*)\)',
    r'string_raise(interp, "type/arg-mismatch", \1)',
    src
)

with open(path, 'w') as f:
    f.write(src)
