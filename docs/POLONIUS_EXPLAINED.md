# Polonius: The Next-Gen Borrow Checker

## 1. What is it?
Polonius is the experimental new Borrow Checker for Rust. It is based on **Datalog**, a logic programming language subset.

## 2. The Problem it Solves
Standard borrow checkers (even NLL) are "Location Based." They look at the *code* to decide where a borrow lives.
*   **Issue:** Some borrows are "Conditional."
    ```rust
    let mut x = 5;
    let y = &mut x; // Mutable borrow
    if random() {
        *y = 6; // Use y
    } else {
        // y is NEVER used here!
        // But NLL might still think x is borrowed because y "exists" in the scope.
        x = 7; // Error in NLL, Valid in Polonius
    }
    ```

## 3. How it Works (The "Datalog" Magic)
Polonius defines borrow checking as a set of logical facts:
*   `borrow_live_at(Borrow, Point)`
*   `variable_defined_at(Var, Point)`
*   `path_accessed_at(Path, Point)`

It feeds these facts into a Datalog solver, which outputs: "Is there a conflict?"
This makes it **Flow-Sensitive** to an extreme degree. It understands that "The borrow dies in the `else` branch but lives in the `if` branch."

## 4. Relevance to OmniLisp
*   **Do we need it?** Not yet.
*   **Why?** Our "Region-RC" system is inherently more permissive than Rust.
    *   Rust *forbids* aliasing mutable pointers at compile time.
    *   OmniLisp *allows* them (via Tethers/Refcounting), enforcing safety at the *Region* level.
*   **Conclusion:** Since we allow runtime sharing (RC), we don't need the extreme static precision of Polonius to prove safety. We can afford to be slightly conservative in our static analysis because our "fallback" is just a Reference Count, not a compile error.
