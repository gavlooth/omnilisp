# C3 Best Practices — Omni Lisp

## 1. defer — Resource Cleanup

Always pair acquire with defer. Never separate them.

```c3
char* buf = (char*)mem::malloc(size);
defer mem::free(buf);

File! f = file::open(path, "r");
defer (void)f.close();

mdb_txn_begin(env, null, 0, &txn);
defer mdb_txn_abort(txn);  // safe even after commit

self.depth++;
defer self.depth--;

// defer catch — cleanup only on error path
char[] data = mem::new_array(char, 12)!;
defer (catch err) { (void)mem::free(data); }
```

## 2. Optionals & Error Handling

`Type!` = value or excuse. `!` rethrows. `??` default. `if (catch)` handles.

```c3
fault MathError { DIVISION_BY_ZERO, OVERFLOW }

fn int! divide(int a, int b) {
    if (b == 0) return MathError.DIVISION_BY_ZERO?;
    return a / b;
}

int result = divide(10, 0)!;       // rethrow — caller must be fn Type!
int safe   = divide(10, 0) ?? -1;  // default on error

int! maybe = divide(10, 0);
if (catch err = maybe) {           // handle — `maybe` unwrapped after block
    switch (err) {
        case MathError.DIVISION_BY_ZERO: io::printn("div/0");
        default: io::printn("other");
    }
}
```

## 3. Contracts

`@require` precondition. `@ensure` postcondition. `@pure` no side effects.

```c3
<* @require index < arr.len : "out of bounds" *>
<* @ensure return > 0 *>
fn int get_positive(int[] arr, usz index) { return arr[index]; }

<* @pure *>
fn int add(int a, int b) { return a + b; }
```

## 4. Generic Modules

Parameterize by type and/or constant. Alias each instantiation.

```c3
module stack {Type};
struct Stack { usz size; Type* elems; }
fn void Stack.push(Stack* self, Type elem) { ... }
fn Type Stack.pop(Stack* self) { ... }

// Instantiate
alias IntStack = Stack{int};
alias FloatStack = Stack{float};
```

## 5. Semantic Macros

Type-aware. `#` = unevaluated arg. `@` = macro call. `$typeof` = type of.

```c3
macro swap(#a, #b) {
    $typeof(#a) temp = #a;
    #a = #b;
    #b = temp;
}
@swap(x, y);
```

## 6. Compile-Time

`$if`, `$assert`, `$embed`, `$typeof`, `$sizeof`, `$defined`.

```c3
$if env::ARCH == "x86_64":
    int simd_width = 256;
$endif

$assert(int.sizeof == 4);
char[] data = $embed("file.bin");
```

## 7. Distinct Types

Same representation, no implicit conversion.

```c3
distinct Meters = double;
distinct Seconds = double;
// Meters / Seconds → compile error. Explicit cast required.
```

## 8. Bitstruct

Bit-level layout for hardware/protocols.

```c3
bitstruct Flags : char {
    bool active : 0;
    int level   : 1..3;
    bool ready  : 4;
}
```

## 9. Slices

`Type[]` = ptr + len. Bounds-checked. INCLUSIVE ranges.

```c3
int[5] arr = { 1, 2, 3, 4, 5 };
int[] s = arr[1..3];    // {2,3,4} — INCLUSIVE both ends
int[] h = arr[:3];      // {1,2,3} — first 3
int[] t = arr[2..];     // {3,4,5} — from index 2
```

## 10. foreach

Value, pointer, indexed forms.

```c3
foreach (val : data) { ... }
foreach (&val : data) { *val *= 2; }  // mutate
foreach (i, val : data) { ... }       // with index
```

## 11. Switch

Exhaustive. No fallthrough. Ranges. Type switching.

```c3
switch (x) {
    case 1: ...;
    case 3..5: ...;    // range
    default: ...;
}
```

## 12. SIMD Vectors

`Type[<N>]`. Element-wise ops. Swizzling.

```c3
float[<4>] a = { 1, 2, 3, 4 };
float[<4>] b = a * 2.0;   // { 2, 4, 6, 8 }
float[<3>] c = a.xyz;     // swizzle
```

## 13. Atomics

`std::atomic::Atomic{Type}`. Lock-free.

```c3
import std::atomic;
atomic::Atomic{long} counter;
counter.store(0);
counter.add(1);
long val = counter.load();
mem::compare_exchange(&ptr, &expected, desired);
```

## 14. any Type

Runtime polymorphism. `typeid` + `void*`.

```c3
fn void print_any(any value) {
    switch (value) {
        case int: io::printfn("%d", *value);
        case String: io::printfn("%s", *value);
    }
}
```

## 15. Attributes

```c3
fn int add(int a, int b) @inline { return a + b; }
fn void internal() @local { ... }          // module-private
extern fn int puts(char* s) @extern("puts");
int[4] data @align(16);
```

## 16. Inline Assembly

```c3
fn void fence() { asm { "mfence"; } }
```

---

## Omni-Specific Rules

**malloc vs scope-region**: Prefer scope-region for Values. `mem::malloc` for C structs, FFI, Expr nodes. Every malloc → defer free or scope dtor.

**Register pattern**: `regular_prims[]` for primitives, `dispatched_prims[]` for dispatch tables, `register_fast_path()` for effects. Update array size.

**Testing**: `test_eq`/`test_str`/`test_tag` test both paths. `setup()` for side effects. `run()` for single expressions.

Sources:
- [C3 Language](https://c3-lang.org/)
- [C3 Features](https://c3-lang.org/getting-started/faq/allfeatures)
