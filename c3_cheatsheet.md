# C3 Programming Language Cheatsheet

C3 is designed as an "evolution, not a revolution" of C. It maintains C's ABI migration and familiar syntax while adding modern features like modules, generic types, and improved error handling.

## 1. Basics & Hello World

C3 uses modules and requires an explicit `main` function.

```c3
module hello_world;
import std::io;

fn void main() {
    io::printn("Hello, world!");
}
```

## 2. Types & Variables

C3 features standardized sized types and clear variable declarations.

```c3
// Sized Integers
i8, i16, i32, i64, i128     // Signed integers
u8, u16, u32, u64, u128     // Unsigned integers
isz, usz                    // Pointer-sized integers (like size_t/ptrdiff_t)

// Floats & Booleans
float, double, quad         // 32-bit, 64-bit, 128-bit floats
bool                        // true or false

// Variables
int x = 10;
const int MAX = 100;        // Constants
```

## 3. Functions

Functions use the `fn` keyword.

```c3
fn int add(int a, int b) {
    return a + b;
}

// Named parameters are supported during calls
int result = add(a: 5, b: 10);
```

## 4. Structs & Methods

C3 supports uniform function call syntax (UFCS), allowing you to define methods for structs without traditional object-oriented overhead.

```c3
struct Point {
    int x;
    int y;
}

// Struct method definition
fn void Point.print(Point* self) {
    io::printfn("(%d, %d)", self.x, self.y);
}

fn void main() {
    Point p = { 10, 20 };
    p.print();              // Equivalent to Point.print(&p)
}
```

## 5. Control Flow

Standard C control flow is present, alongside modern additions like `foreach` and `defer`.

```c3
// If / Else
if (x > 0) { ... } else { ... }

// Switch (No implicit fallthrough, supports ranges)
switch (x) {
    case 1..5: io::printn("1 to 5");
    case 6: io::printn("6");
    default: io::printn("Other");
}

// Foreach Loop
int[] numbers = {1, 2, 3};
foreach (idx, val : numbers) {
    io::printfn("Index %d: %d", idx, val);
}
// Or without index:
foreach (val : numbers) { ... }

// Defer (Executes at the end of the current scope)
fn void file_op() {
    File* f = file::open("data.txt");
    defer file::close(f); // Guaranteed to run on exit
    // ... use file
}
```

## 6. Arrays & Slices

Slices (`Type[]`) are fat pointers containing both a pointer to the data and its length.

```c3
int[5] arr = { 1, 2, 3, 4, 5 };   // Fixed array
int[] slice = arr[1..3];          // Slice containing {2, 3, 4}

usz len = slice.len;              // Get slice length
```

## 7. Error Handling (Faults & Optionals)

C3 uses an elegant "Fault" system instead of exceptions or standard C return codes. Functions that can fail return an "optional" type (marked with `!`).

```c3
// Define a custom fault
fault NetworkError {
    DISCONNECTED,
    TIMEOUT
}

// Function returning an optional (can fail)
fn int! get_data(bool fail) {
    if (fail) return NetworkError.TIMEOUT?; // Return a fault
    return 100;
}

fn void main() {
    // 1. Force unwrap (Panics if fault occurs)
    int data1 = get_data(false)!!; 
    
    // 2. Catch / Handle
    if (int data2 = catch get_data(true)) {
        io::printn("Success!");
    } else {
        io::printn("Failed to get data.");
    }
    
    // 3. Propagate (passes the error up)
    // return get_data(true)?;
}
```

## 8. Macros & Compile-Time Execution

C3 replaces the C preprocessor with a robust hygienic macro system and compile-time evaluation using `$`.

```c3
// Compile-time if
$if (env::OS == "windows") {
    import std::os::windows;
} $else {
    import std::os::posix;
}

// Hygienic Macros
macro @swap(a, b) {
    var temp = a;
    a = b;
    b = temp;
}

// Compile-time reflection
$foreach (member : Point.members) {
    io::printn(member.name);
}
```

## 9. Memory Management

C3 is manually managed but provides ergonomic tools like temporary allocators (`@pool`).

```c3
// Using standard heap allocation
int* p = mem::alloc(int);
defer mem::free(p);

// Using @pool (Temporary arena allocator)
// Memory allocated inside the pool is automatically freed when the scope ends.
@pool() {
    String s = string::printf("Temporary string: %d", 42);
    io::printn(s);
} // 's' is freed here
```

## 10. Contracts (Design by Contract)

You can define preconditions and postconditions for your functions.

```c3
fn int divide(int a, int b)
    @require(b != 0)          // Must be true before execution
    @ensure($return <= a)     // Must be true after execution
{
    return a / b;
}
```

## 11. Type Aliases & Enums

```c3
// Alias (Replaces typedef)
alias ID = u64;

// Enums
enum Color : i32 {
    RED = 1,
    GREEN = 2,
    BLUE = 3
}
```