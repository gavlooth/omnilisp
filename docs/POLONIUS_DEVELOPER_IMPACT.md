# Does Polonius Force Manual Borrowing?

## 1. The Short Answer
**Yes and No.**

*   **Yes:** You still have to write `&` (borrow) and `&mut` (mutable borrow) in your code. Polonius is a *checker*, not a code generator. It validates the annotations you wrote.
*   **No:** You typically write *fewer* lifetime annotations (`'a`) because Polonius is smart enough to figure them out where the old checker failed.

## 2. The OmniLisp Advantage (No Annotations)

In OmniLisp's **RC-G** architecture, we want to avoid *all* of this.

| Feature | Rust (Polonius) | OmniLisp (RC-G) |
| :--- | :--- | :--- |
| **Borrow Syntax** | Explicit (`&x`) | **Implicit** (Compiler decides) |
| **Lifetime Syntax** | Explicit (`<'a>`) | **None** (Region Inference) |
| **Conflict Resolution** | Compile Error | **Runtime Fallback** (RC/Tether) |

## 3. Conclusion
Polonius makes the *existing* restrictions of Rust less painful, but it preserves the core burden: **The developer must design the ownership model.**

OmniLisp's goal is to **infer** the ownership model. If we can't prove it's safe statically, we don't error—we just pay a small runtime cost (RC). This is "freedom with safety."
