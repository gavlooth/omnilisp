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
        out_lines.append('fn Value* jit_raise(Interp* interp, String code, char[] message) @inline {')
        out_lines.append('    return raise_error_with_payload_names(interp, "runtime", code, (String)message, null);')
        out_lines.append('}')
        out_lines.append('')
        inserted_helper = True
        continue
    out_lines.append(line)

src = '\n'.join(out_lines)

# Pattern 1: raise_error(interp, msg) where msg is a char[] variable
src = re.sub(
    r'raise_error\(interp, msg\)',
    r'jit_raise(interp, "runtime/evaluation-error", msg)',
    src
)

# Pattern 2: raise_error(interp, "literal message")
def repl_jit_literal(m):
    msg = m.group(1)
    text = msg.strip('"')
    if 'failed to allocate' in text or 'out of memory' in text:
        code = 'runtime/out-of-memory'
    elif 'expected' in text or 'missing' in text or 'invalid' in text:
        code = 'type/arg-mismatch'
    elif 'not a function' in text:
        code = 'type/arg-mismatch'
    else:
        code = 'runtime/evaluation-error'
    return f'jit_raise(interp, "{code}", {msg})'

src = re.sub(r'raise_error\(interp, ("[^"]+")\)', repl_jit_literal, src)

# Pattern 3: raise_error(interp, "unbound variable")
src = re.sub(
    r'raise_error\(interp, "unbound variable"\)',
    r'jit_raise(interp, "runtime/evaluation-error", "unbound variable")',
    src
)

# Pattern 4: raise_error(interp, "called value is not a function")
src = re.sub(
    r'raise_error\(interp, "called value is not a function"\)',
    r'jit_raise(interp, "type/arg-mismatch", "called value is not a function")',
    src
)

# Pattern 5: raise_error(interp, "too few arguments for variadic lambda")
src = re.sub(
    r'raise_error\(interp, "too few arguments for variadic lambda"\)',
    r'jit_raise(interp, "type/arity", "too few arguments for variadic lambda")',
    src
)

# Pattern 6: raise_error(interp, "invalid primitive application state")
src = re.sub(
    r'raise_error\(interp, "invalid primitive application state"\)',
    r'jit_raise(interp, "runtime/invalid-state", "invalid primitive application state")',
    src
)

# Pattern 7: raise_error(interp, buf[:len])
src = re.sub(
    r'raise_error\(interp, buf\[:len\]\)',
    r'raise_error_with_payload_names(interp, "type", "type/arity", (String)buf[:len], null)',
    src
)

with open(path, 'w') as f:
    f.write(src)
