#!/usr/bin/env python3
import re
from migrate_cli import single_path_arg

path = single_path_arg(__file__)
with open(path, 'r') as f:
    src = f.read()

# Pattern 1: raise_error(interp, msg) where msg is a char[] variable
src = re.sub(
    r'raise_error\(interp, msg\)',
    r'math_raise(interp, "type/range-error", msg)',
    src
)

# Pattern 2: raise_error(interp, "/: division by zero")
src = re.sub(
    r'raise_error\(interp, "\/: division by zero"\)',
    r'math_raise(interp, "type/div-by-zero", "/: division by zero")',
    src
)

# Pattern 3: raise_error(interp, "%: division by zero")
src = re.sub(
    r'raise_error\(interp, "%: division by zero"\)',
    r'math_raise(interp, "type/div-by-zero", "%: division by zero")',
    src
)

# Pattern 4: raise_error(interp, "numeric operation: unknown binary op")
src = re.sub(
    r'raise_error\(interp, "numeric operation: unknown binary op"\)',
    r'math_raise(interp, "type/unsupported", "numeric operation: unknown binary op")',
    src
)

# Pattern 5: raise_error(interp, "numeric operation: mixed BigComplex and fixed-width complex requires explicit conversion")
src = re.sub(
    r'raise_error\(interp, "numeric operation: mixed BigComplex and fixed-width complex requires explicit conversion"\)',
    r'math_raise(interp, "type/arg-mismatch", "numeric operation: mixed BigComplex and fixed-width complex requires explicit conversion")',
    src
)

# Pattern 6: raise_error(interp, "numeric operation: value is not representable as Complex64")
src = re.sub(
    r'raise_error\(interp, "numeric operation: value is not representable as Complex64"\)',
    r'math_raise(interp, "type/range-error", "numeric operation: value is not representable as Complex64")',
    src
)

# Pattern 7: raise_error(interp, "numeric operation: value is not representable as Complex128")
src = re.sub(
    r'raise_error\(interp, "numeric operation: value is not representable as Complex128"\)',
    r'math_raise(interp, "type/range-error", "numeric operation: value is not representable as Complex128")',
    src
)

with open(path, 'w') as f:
    f.write(src)
