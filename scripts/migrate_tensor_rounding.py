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
        out_lines.append('fn Value* tensor_raise(Interp* interp, String code, char[] message) @inline {')
        out_lines.append('    return raise_error_with_payload_names(interp, "tensor", code, (String)message, null);')
        out_lines.append('}')
        out_lines.append('')
        inserted_helper = True
        continue
    out_lines.append(line)

src = '\n'.join(out_lines)

# Pattern 1: raise_error(interp, message) where message is a variable
src = re.sub(
    r'raise_error\(interp, message\)',
    r'tensor_raise(interp, "tensor/invalid-argument", message)',
    src
)

# Pattern 2: raise_error(interp, range_message) where range_message is a variable
src = re.sub(
    r'raise_error\(interp, range_message\)',
    r'tensor_raise(interp, "tensor/domain-error", range_message)',
    src
)

# Pattern 3: raise_error(interp, "rounding: tensor integer result storage is too large")
src = re.sub(
    r'raise_error\(interp, "rounding: tensor integer result storage is too large"\)',
    r'tensor_raise(interp, "tensor/out-of-memory", "rounding: tensor integer result storage is too large")',
    src
)

# Pattern 4: raise_error(interp, "rounding: failed to allocate CUDA integer result buffer")
src = re.sub(
    r'raise_error\(interp, "rounding: failed to allocate CUDA integer result buffer"\)',
    r'tensor_raise(interp, "tensor/out-of-memory", "rounding: failed to allocate CUDA integer result buffer")',
    src
)

# Pattern 5: raise_error(interp, "rounding: invalid CUDA rounding operation")
src = re.sub(
    r'raise_error\(interp, "rounding: invalid CUDA rounding operation"\)',
    r'tensor_raise(interp, "tensor/invalid-argument", "rounding: invalid CUDA rounding operation")',
    src
)

# Pattern 6: raise_error(interp, "rounding: failed to allocate BigInteger tensor element")
src = re.sub(
    r'raise_error\(interp, "rounding: failed to allocate BigInteger tensor element"\)',
    r'tensor_raise(interp, "tensor/out-of-memory", "rounding: failed to allocate BigInteger tensor element")',
    src
)

# Pattern 7: raise_error(interp, "rounding: failed to allocate Vulkan integer result buffer")
src = re.sub(
    r'raise_error\(interp, "rounding: failed to allocate Vulkan integer result buffer"\)',
    r'tensor_raise(interp, "tensor/out-of-memory", "rounding: failed to allocate Vulkan integer result buffer")',
    src
)

# Pattern 8: raise_error(interp, "rounding: invalid Vulkan rounding operation")
src = re.sub(
    r'raise_error\(interp, "rounding: invalid Vulkan rounding operation"\)',
    r'tensor_raise(interp, "tensor/invalid-argument", "rounding: invalid Vulkan rounding operation")',
    src
)

# Pattern 9: raise_error(interp, "rounding: complex numbers are not supported")
src = re.sub(
    r'raise_error\(interp, "rounding: complex numbers are not supported"\)',
    r'tensor_raise(interp, "tensor/unsupported", "rounding: complex numbers are not supported")',
    src
)

with open(path, 'w') as f:
    f.write(src)
