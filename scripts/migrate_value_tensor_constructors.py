#!/usr/bin/env python3
import re
from migrate_cli import single_path_arg

path = single_path_arg(__file__)
with open(path, 'r') as f:
    src = f.read()

# Pattern 1: raise_error(interp, oom_message)
src = re.sub(
    r'raise_error\(interp, oom_message\)',
    r'tensor_runtime_raise(interp, oom_message)',
    src
)

# Pattern 2: boundary errors
src = re.sub(
    r'raise_error\(interp, ("boundary:[^"]+")\)',
    r'boundary_raise(interp, "boundary/out-of-memory", \1)',
    src
)

with open(path, 'w') as f:
    f.write(src)
