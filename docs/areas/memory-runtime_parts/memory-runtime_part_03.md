# memory-runtime Part 03

Source: `docs/areas/memory-runtime.md`

      intended construction error.
    - optional dispatch diagnostic payload construction now uses the same
      non-raising hashmap helper, preventing failed ancillary payload
      allocation from pre-seeding `raise_pending` under an active handler.
    - optional constructor mismatch diagnostic payload construction also uses
      the non-raising hashmap helper and validates payload key interning before
      constructing key symbols.
    - optional deduce integrity/check-context diagnostic payload construction
      now uses the non-raising hashmap helper and no-raise local setters, so
      failed ancillary integrity payload allocation cannot pre-seed
      `raise_pending` before the intended integrity violation raise.
    - optional iteration-limit diagnostic payload construction also uses the
      non-raising hashmap helper before the later iteration-limit raise.
    - goal-directed selector analysis and selector/relation surface diagnostic
      payload construction also uses the non-raising hashmap helper and local
      checked insertion before publishing the existing deduce OOM fallback.
    - goal-directed explain snapshot/component payloads now reject failed
      payload-symbol interning before constructing payload `SYMBOL` values,
      and why-result path dictionary lookup rejects invalid temporary key
      symbols before probing.
    - dispatch diagnostic payload insertion, process-spawn result maps, HTTP
      response maps, and FTXUI dictionary lookup now also guard failed symbol
      interning before constructing payload or lookup key symbols.
    - validation: host build, boundary facade usage, boundary change policy,
      status consistency, `git diff --check`, bounded normal `jit-policy` with
      FTXUI smoke enabled (`pass=51 fail=0`), and bounded ASAN `jit-policy`
      (`pass=50 fail=0`), plus bounded normal+ASAN `memory-lifetime-smoke`
      with FTXUI smoke enabled (`pass=200 fail=0`) and bounded normal+ASAN
      `deduce` slice (`pass=330 fail=0`), plus bounded normal+ASAN `async`
      with FTXUI smoke enabled (`pass=65 fail=0`) are green.
