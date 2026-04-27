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
        out_lines.append('fn Value* meta_raise(Interp* interp, String code, char[] message) @inline {')
        out_lines.append('    return raise_error_with_payload_names(interp, "type", code, (String)message, null);')
        out_lines.append('}')
        out_lines.append('')
        inserted_helper = True
        continue
    out_lines.append(line)

src = '\n'.join(out_lines)

# Categorize common patterns
def classify_meta_code(msg):
    msg = msg.strip('"')
    if 'expected' in msg and ('argument' in msg or 'expected 1' in msg or 'expected 2' in msg or 'expected expression' in msg):
        return 'type/arity'
    if 'expected number' in msg or 'expected integer' in msg or 'expected string' in msg or 'expected symbol' in msg or 'expected error value' in msg or 'expected quoted expression' in msg:
        return 'type/arg-mismatch'
    if 'complex numbers are not ordered' in msg:
        return 'type/arg-mismatch'
    if 'numeric comparison failed' in msg or 'could not convert' in msg:
        return 'runtime/evaluation-error'
    if 'expected function and argument list' in msg or 'expected argument list' in msg or 'expected a proper argument list' in msg:
        return 'type/arity'
    return 'type/arg-mismatch'

# Pattern 1: raise_error(interp, "literal message")
def repl_literal(m):
    msg = m.group(1)
    code = classify_meta_code(msg)
    return f'meta_raise(interp, "{code}", {msg})'

src = re.sub(r'raise_error\(interp, ("[^"]+")\)', repl_literal, src)

# Pattern 2: raise_error(interp, call_expr.lit.value.str_chars[:call_expr.lit.value.str_len])
src = re.sub(
    r'raise_error\(interp, call_expr\.lit\.value\.str_chars\[:call_expr\.lit\.value\.str_len\]\)',
    r'raise_error_with_payload_names(interp, "type", "type/arg-mismatch", (String)call_expr.lit.value.str_chars[:call_expr.lit.value.str_len], null)',
    src
)

# Pattern 3: raise_error(interp, r.error.message[:256])
src = re.sub(
    r'raise_error\(interp, r\.error\.message\[:256\]\)',
    r'raise_error_with_payload_names(interp, "runtime", "runtime/evaluation-error", (String)r.error.message[:256], null)',
    src
)

# Pattern 4: raise_error(interp, result.str_chars[:result.str_len])
src = re.sub(
    r'raise_error\(interp, result\.str_chars\[:result\.str_len\]\)',
    r'raise_error_with_payload_names(interp, "runtime", "runtime/evaluation-error", (String)result.str_chars[:result.str_len], null)',
    src
)

# Pattern 5: raise_error(interp, args[0].str_chars[:args[0].str_len])
src = re.sub(
    r'raise_error\(interp, args\[0\]\.str_chars\[:args\[0\]\.str_len\]\)',
    r'raise_error_with_payload_names(interp, "type", "type/arg-mismatch", (String)args[0].str_chars[:args[0].str_len], null)',
    src
)

with open(path, 'w') as f:
    f.write(src)
