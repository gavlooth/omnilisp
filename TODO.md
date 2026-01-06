# OmniLisp TODO

## Pikaparser Pattern Compiler Upgrade

Upgrade `omni_compile_pattern()` to leverage Pika's full power - which exceeds both standard PEG and traditional regex.

### Parsing Power Hierarchy

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARSING POWER HIERARCHY                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Formal Regex (DFA)                                              │
│      │  • No nesting, no recursion                               │
│      │  • O(n) matching                                          │
│      ▼                                                           │
│  Standard PEG (Packrat)                                          │
│      │  • Context-free grammars                                  │
│      │  • NO left recursion (infinite loop)                      │
│      │  • Anchored to start of input                             │
│      │  • First match only                                       │
│      ▼                                                           │
│  ╔═══════════════════════════════════════════════════════════╗   │
│  ║  PIKA PARSER  ◄── OmniLisp uses this                      ║   │
│  ║      • Left recursion support (expr <- expr '+' term)     ║   │
│  ║      • Substring matching (find anywhere in input)        ║   │
│  ║      • All non-overlapping matches                        ║   │
│  ║      • Grammar composition (named rules)                  ║   │
│  ║      • Bottom-up DP with memoization                      ║   │
│  ╚═══════════════════════════════════════════════════════════╝   │
│      │                                                           │
│      ▼                                                           │
│  PCRE (practical regex)                                          │
│      • Has backreferences (\1, \2) - Pika lacks this             │
│      • But backreferences cause exponential blowup               │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Pika-Specific Features to Leverage

| Feature | Standard PEG | Pika | Benefit |
|---------|--------------|------|---------|
| Left recursion | ❌ Fails | ✅ Native | Parse `expr <- expr '+' term` |
| Substring match | ❌ Anchored | ✅ Anywhere | Find patterns in middle of text |
| Multiple matches | ❌ First only | ✅ All | `find_all` returns every match |
| Indirect recursion | ❌ Fails | ✅ Works | Mutually recursive rules |
| Grammar composition | Limited | ✅ Full | Build complex parsers from rules |

### Current State

`omni_compile_pattern()` at `src/runtime/pika/omni_grammar.c:954-1017` only handles:
- Simple `[abc]` character classes
- `+` and `*` quantifiers

### Target State

**Two-tier API:**
1. **Simple patterns** - regex-like syntax for common cases
2. **Full grammars** - expose Pika's grammar DSL for advanced parsing

```c
// Tier 1: Simple regex-like patterns (current API, enhanced)
omni_pika_match("[a-z]+", input);           // Find lowercase words
omni_pika_match("\\d+\\.\\d+", input);      // Find decimals

// Tier 2: Full Pika grammars (NEW - leverages left recursion)
omni_pika_grammar_match(
    "expr   <- expr '+' term / term\n"
    "term   <- term '*' factor / factor\n"
    "factor <- '(' expr ')' / [0-9]+\n",
    "expr", input
);
```

### Phase 1: Analysis

- [ ] **T-pika-analyze-current**: Review current `omni_compile_pattern()` implementation
  - **Where**: `src/runtime/pika/omni_grammar.c:954-1017`
  - **What**: Document current limitations and edge cases
  - **Acceptance**: List of unsupported patterns documented

- [ ] **T-pika-analyze-meta**: Study `pika_meta_parse()` capabilities
  - **Where**: `src/runtime/pika_c/pika.c:2812-2900`
  - **What**: Understand PEG grammar format expected by meta-parser
  - **Acceptance**: PEG syntax requirements documented

- [ ] **T-pika-analyze-grammar**: Examine meta-grammar structure
  - **Where**: `build_meta_grammar()` in `src/runtime/pika_c/pika.c`
  - **What**: Identify supported PEG constructs (sequences, choices, quantifiers, etc.)
  - **Acceptance**: Mapping table of PEG constructs created

### Phase 2: Design

- [ ] **T-pika-design-mapping**: Design regex-to-PEG translation mapping
  - **Where**: N/A (design document)
  - **What**: Create mapping table:
    | Regex | PEG Equivalent |
    |-------|----------------|
    | `[a-z]` | `[a-z]` |
    | `a+` | `a+` |
    | `a*` | `a*` |
    | `a?` | `a?` |
    | `a\|b` | `a / b` |
    | `(ab)` | `(a b)` |
    | `^` | Start anchor (implicit or `&.`) |
    | `$` | End anchor (`!.`) |
    | `\.` | `"."` (literal) |
    | `\n` | Newline literal |
    | `\t` | Tab literal |
    | `\d` | `[0-9]` |
    | `\w` | `[a-zA-Z0-9_]` |
    | `\s` | `[ \t\n\r]` |
  - **Acceptance**: Complete mapping table reviewed and approved

- [ ] **T-pika-design-api**: Define internal API for converter
  - **Where**: N/A (design document)
  - **What**: Define function signatures:
    ```c
    typedef struct RegexToken { ... } RegexToken;
    RegexToken* regex_tokenize(const char* pattern, size_t* count);
    char* regex_to_peg(const char* pattern, char** error_out);
    ```
  - **Acceptance**: API signatures finalized

### Phase 3: Implementation - Lexer

- [ ] **T-pika-lexer-types**: Define regex token types
  - **Where**: `src/runtime/pika/omni_grammar.c` (add before `omni_compile_pattern`)
  - **What**: Add token type enum and struct:
    ```c
    typedef enum {
        RTOK_CHAR,        // literal character
        RTOK_CHARSET,     // [abc] or [a-z]
        RTOK_STAR,        // *
        RTOK_PLUS,        // +
        RTOK_QUESTION,    // ?
        RTOK_PIPE,        // |
        RTOK_LPAREN,      // (
        RTOK_RPAREN,      // )
        RTOK_ANCHOR_START,// ^
        RTOK_ANCHOR_END,  // $
        RTOK_DOT,         // . (any char)
        RTOK_ESCAPE,      // \n, \t, \d, etc.
    } RegexTokenType;

    typedef struct {
        RegexTokenType type;
        char* value;      // for CHAR, CHARSET, ESCAPE
        int value_len;
    } RegexToken;
    ```
  - **Acceptance**: Types compile without errors

- [ ] **T-pika-lexer-impl**: Implement `regex_tokenize()`
  - **Where**: `src/runtime/pika/omni_grammar.c`
  - **What**: Tokenize regex pattern string into token array
  - **How to verify**: Unit test with pattern `"[a-z]+|[0-9]*"` produces correct tokens
  - **Acceptance**:
    - Handles all token types in enum
    - Returns NULL with error for malformed patterns
    - Frees memory properly

- [ ] **T-pika-lexer-free**: Implement `regex_tokens_free()`
  - **Where**: `src/runtime/pika/omni_grammar.c`
  - **What**: Free token array and all value strings
  - **Acceptance**: No memory leaks (valgrind clean)

### Phase 4: Implementation - Converter

- [ ] **T-pika-converter-impl**: Implement `regex_to_peg()`
  - **Where**: `src/runtime/pika/omni_grammar.c`
  - **What**: Convert token stream to PEG grammar string
  - **Algorithm**:
    1. Tokenize input pattern
    2. Parse tokens into AST (handle precedence: atom < quantifier < sequence < alternation)
    3. Emit PEG syntax: `pattern <- <converted_expr>`
  - **How to verify**: `regex_to_peg("[a-z]+|[0-9]+")` returns `"pattern <- [a-z]+ / [0-9]+\n"`
  - **Acceptance**:
    - Handles nested groups `((a|b)+)*`
    - Handles escape sequences
    - Returns NULL with error for invalid patterns

- [ ] **T-pika-converter-escapes**: Handle escape sequences
  - **Where**: `src/runtime/pika/omni_grammar.c` inside `regex_to_peg()`
  - **What**: Convert:
    - `\d` → `[0-9]`
    - `\w` → `[a-zA-Z0-9_]`
    - `\s` → `[ \t\n\r]`
    - `\.` → `"."`
    - `\\` → `"\\"`
  - **Acceptance**: All common escapes supported

- [ ] **T-pika-converter-anchors**: Handle anchors
  - **Where**: `src/runtime/pika/omni_grammar.c` inside `regex_to_peg()`
  - **What**: Convert:
    - `^` at start → implicit (PEG matches from start by default) or `&.`
    - `$` at end → `!.` (negative lookahead for any char)
  - **Acceptance**: `"^foo$"` matches `"foo"` but not `"foo bar"`

### Phase 5: Integration

- [ ] **T-pika-replace-impl**: Replace `omni_compile_pattern()` body
  - **Where**: `src/runtime/pika/omni_grammar.c:954-1017`
  - **What**: Replace entire function body with:
    ```c
    PikaGrammar* omni_compile_pattern(const char* pattern, char** error_out) {
        char* peg_spec = regex_to_peg(pattern, error_out);
        if (!peg_spec) return NULL;

        PikaGrammar* grammar = pika_meta_parse(peg_spec, error_out);
        free(peg_spec);
        return grammar;
    }
    ```
  - **How to verify**: Existing tests in `test_pika_tower.c` still pass
  - **Acceptance**:
    - All existing `omni_pika_match/split/replace/find_all` tests pass
    - No memory leaks

- [ ] **T-pika-impl-match-rule**: Implement missing `omni_pika_match_rule()`
  - **Where**: `src/runtime/pika/omni_grammar.c` (after line 760)
  - **What**: Implement function declared at `omni_grammar.h:65`:
    ```c
    Value* omni_pika_match_rule(PikaGrammar* grammar, const char* rule, const char* input) {
        if (!grammar || !rule || !input) return mk_nil();

        PikaMemoTable* memo = pika_grammar_parse(grammar, input);
        if (!memo) return mk_nil();

        size_t match_count = 0;
        PikaMatch** matches = pika_memo_get_non_overlapping_matches_for_rule(
            memo, rule, &match_count
        );

        if (match_count == 0 || !matches) {
            pika_memo_free(memo);
            return mk_nil();
        }

        PikaMatch* match = matches[0];
        int start = pika_match_start(match);
        int len = pika_match_len(match);

        char* text = malloc(len + 1);
        memcpy(text, input + start, len);
        text[len] = '\0';

        Value* result = mk_code(text);
        free(text);
        free(matches);
        pika_memo_free(memo);

        return result;
    }
    ```
  - **How to verify**: New unit test passes
  - **Acceptance**: Function works with custom grammars

### Phase 6: Testing

- [ ] **T-pika-test-alternation**: Add alternation test
  - **Where**: `tests/test_pika_tower.c`
  - **What**: Add test:
    ```c
    TEST(pika_pattern_alternation) {
        Value* result = omni_pika_match("foo|bar", "bar baz");
        ASSERT(result != NULL);
        ASSERT_EQ(result->tag, T_CODE);
        ASSERT_STR_EQ(result->s, "bar");
    }
    ```
  - **Acceptance**: Test passes

- [ ] **T-pika-test-groups**: Add grouping test
  - **Where**: `tests/test_pika_tower.c`
  - **What**: Add test:
    ```c
    TEST(pika_pattern_groups) {
        Value* result = omni_pika_match("(ab)+", "ababab");
        ASSERT(result != NULL);
        ASSERT_EQ(result->tag, T_CODE);
        ASSERT_STR_EQ(result->s, "ababab");
    }
    ```
  - **Acceptance**: Test passes

- [ ] **T-pika-test-optional**: Add optional quantifier test
  - **Where**: `tests/test_pika_tower.c`
  - **What**: Add test:
    ```c
    TEST(pika_pattern_optional) {
        Value* r1 = omni_pika_match("colou?r", "color");
        ASSERT_STR_EQ(r1->s, "color");
        Value* r2 = omni_pika_match("colou?r", "colour");
        ASSERT_STR_EQ(r2->s, "colour");
    }
    ```
  - **Acceptance**: Test passes

- [ ] **T-pika-test-escapes**: Add escape sequence test
  - **Where**: `tests/test_pika_tower.c`
  - **What**: Add test:
    ```c
    TEST(pika_pattern_escapes) {
        Value* r1 = omni_pika_match("\\d+", "abc123def");
        ASSERT_STR_EQ(r1->s, "123");
        Value* r2 = omni_pika_match("\\.", "a.b");
        ASSERT_STR_EQ(r2->s, ".");
    }
    ```
  - **Acceptance**: Test passes

- [ ] **T-pika-test-dot**: Add dot (any char) test
  - **Where**: `tests/test_pika_tower.c`
  - **What**: Add test:
    ```c
    TEST(pika_pattern_dot) {
        Value* result = omni_pika_match("a.c", "abc");
        ASSERT_STR_EQ(result->s, "abc");
    }
    ```
  - **Acceptance**: Test passes

- [ ] **T-pika-test-match-rule**: Add `omni_pika_match_rule()` test
  - **Where**: `tests/test_pika_tower.c`
  - **What**: Add test:
    ```c
    TEST(pika_match_rule) {
        char* error = NULL;
        PikaGrammar* g = omni_compile_peg("number <- [0-9]+\n", &error);
        ASSERT(g != NULL);
        Value* result = omni_pika_match_rule(g, "number", "abc123def");
        ASSERT_STR_EQ(result->s, "123");
        pika_grammar_free(g);
    }
    ```
  - **Acceptance**: Test passes

- [ ] **T-pika-run-all**: Run full test suite
  - **Where**: Project root
  - **What**: `./tests.sh` or `make test`
  - **Acceptance**: All tests pass, no regressions

### Phase 7: Documentation

- [ ] **T-pika-doc-syntax**: Document supported pattern syntax
  - **Where**: `docs/pattern-syntax.md` (new file)
  - **What**: Create documentation covering:
    - Supported regex features
    - Examples for each feature
    - Differences from standard regex
    - PEG translation details
  - **Acceptance**: Documentation reviewed

- [x] **T-pika-update-readme**: Update relevant documentation
  - **Where**: Existing docs referencing pattern matching
  - **What**: Add links to new pattern syntax documentation
  - **Acceptance**: Cross-references added
  - **Status**: DONE - Created `docs/PATTERN_SYNTAX.md` with full hierarchy

### Phase 8: Pika-Specific Advanced API (NEW)

Implement the advanced API that leverages Pika's unique capabilities beyond standard PEG.
API defined in `src/runtime/pika/omni_grammar.h`.

- [ ] **T-pika-adv-struct**: Implement `OmniCompiledGrammar` struct
  - **Where**: `src/runtime/pika/omni_grammar.c`
  - **What**: Define opaque struct wrapping `PikaGrammar*` with metadata
  - **Acceptance**: Struct compiles and can hold grammar

- [ ] **T-pika-adv-grammar-match**: Implement `omni_pika_grammar_match()`
  - **Where**: `src/runtime/pika/omni_grammar.c`
  - **What**: Compile grammar spec, match against input with start rule
    ```c
    Value* omni_pika_grammar_match(const char* grammar_spec,
                                    const char* start_rule,
                                    const char* input) {
        char* error = NULL;
        PikaGrammar* g = pika_meta_parse(grammar_spec, &error);
        if (!g) { free(error); return mk_nil(); }
        Value* result = omni_pika_match_rule(g, start_rule, input);
        pika_grammar_free(g);
        return result;
    }
    ```
  - **How to verify**: Test with left-recursive grammar `expr <- expr '+' term / term`
  - **Acceptance**: Left recursion works (would fail in standard PEG)

- [ ] **T-pika-adv-find-positions**: Implement `omni_pika_find_all_positions()`
  - **Where**: `src/runtime/pika/omni_grammar.c`
  - **What**: Return all matches with line/column positions
  - **Algorithm**:
    1. Compile pattern to grammar
    2. Get all non-overlapping matches
    3. For each match, compute line/column from offset
    4. Build `OmniMatchResult` array
  - **Acceptance**: Returns correct positions for multi-line input

- [ ] **T-pika-adv-grammar-find-all**: Implement `omni_pika_grammar_find_all()`
  - **Where**: `src/runtime/pika/omni_grammar.c`
  - **What**: Combine grammar compilation with all-matches mode
  - **Acceptance**: Can find all expressions in code using grammar

- [ ] **T-pika-adv-compiled**: Implement compiled grammar caching
  - **Where**: `src/runtime/pika/omni_grammar.c`
  - **What**: Implement:
    - `omni_grammar_compile()` - compile and cache
    - `omni_grammar_compiled_free()` - free cached grammar
    - `omni_pika_compiled_match()` - match with cached
    - `omni_pika_compiled_find_all()` - find all with cached
  - **How to verify**: Benchmark shows reuse faster than recompile
  - **Acceptance**: Compiled grammars reusable across calls

- [ ] **T-pika-adv-result-free**: Implement `omni_match_result_free()`
  - **Where**: `src/runtime/pika/omni_grammar.c`
  - **What**: Free `OmniMatchResult` and all contained strings
  - **Acceptance**: No memory leaks (valgrind clean)

### Phase 9: Advanced API Tests

- [ ] **T-pika-adv-test-left-rec**: Test left recursion support
  - **Where**: `tests/test_pika_tower.c`
  - **What**: Test grammar that would fail in standard PEG:
    ```c
    TEST(pika_left_recursion) {
        // This would infinite loop in packrat parsers!
        Value* r = omni_pika_grammar_match(
            "expr <- expr '+' term / term\n"
            "term <- [0-9]+\n",
            "expr", "1+2+3"
        );
        ASSERT(r != NULL);
    }
    ```
  - **Acceptance**: Test passes (proves Pika advantage)

- [ ] **T-pika-adv-test-positions**: Test position tracking
  - **Where**: `tests/test_pika_tower.c`
  - **What**: Verify line/column numbers are correct
  - **Acceptance**: Positions match expected values

- [ ] **T-pika-adv-test-compiled**: Test compiled grammar reuse
  - **Where**: `tests/test_pika_tower.c`
  - **What**: Compile once, match multiple times
  - **Acceptance**: Results correct, no memory leaks

### Files to Modify

| File | Changes |
|------|---------|
| `src/runtime/pika/omni_grammar.h` | [DONE] Added Pika power hierarchy docs, advanced API types and declarations |
| `src/runtime/pika/omni_grammar.c` | Add lexer, converter, replace `omni_compile_pattern()`, implement advanced API |
| `tests/test_pika_tower.c` | Add new pattern tests and left-recursion tests |
| `docs/PATTERN_SYNTAX.md` | [DONE] Full parsing hierarchy documentation |

### Dependencies

- `pika_meta_parse()` must be working (verified: `src/runtime/pika_c/pika.c:2812-2900`)
- Existing string operation tests must continue to pass

### Verification

```bash
# Build
make clean && make

# Run tests
./tests.sh

# Memory check
valgrind --leak-check=full ./test_pika_tower
```

---

## References

- "Collapsing Towers of Interpreters" (Amin & Rompf, POPL 2018)
- "Pika parsing: reformulating packrat parsing as a dynamic programming algorithm solves the left recursion and error recovery problems" (Luke Hutchison, 2020)
- "Parsing Expression Grammars: A Recognition-Based Syntactic Foundation" (Bryan Ford, 2004)
