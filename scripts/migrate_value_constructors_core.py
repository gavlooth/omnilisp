#!/usr/bin/env python3
import re
from migrate_cli import single_path_arg

path = single_path_arg(__file__)
with open(path, 'r') as f:
    src = f.read()

# Pattern 1: boundary errors - use boundary_raise
src = re.sub(
    r'raise_error\(interp, ("boundary:[^"]+")\)',
    r'boundary_raise(interp, "boundary/out-of-memory", \1)',
    src
)

# Pattern 2: raise_error(interp, "cons: ...")
src = re.sub(
    r'raise_error\(interp, ("cons:[^"]+")\)',
    r'boundary_raise(interp, "boundary/out-of-memory", \1)',
    src
)

with open(path, 'w') as f:
    f.write(src)
