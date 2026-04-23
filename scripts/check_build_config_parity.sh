#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

fail() {
  echo "FAIL: $*" >&2
  exit 1
}

python3 -m json.tool project.json >/dev/null

for include_dir in third_party/ftxui/include third_party/ftxui/src; do
  grep -q -F "\"${include_dir}\"" project.json \
    || fail "project.json is missing C include dir ${include_dir}"
done

mapfile -t helper_sources < <(
  sed -n 's/^[[:space:]]*"\(csrc\/[^"]*\.[chp]*\)",$/\1/p; s/^[[:space:]]*"\(third_party\/tomlc17\/[^"]*\.c\)",$/\1/p' \
    scripts/build_omni_chelpers.sh |
    sort -u
)

mapfile -t project_sources < <(
  sed -n 's/^[[:space:]]*"\(csrc\/[^"]*\.[chp]*\)",$/\1/p; s/^[[:space:]]*"\(third_party\/tomlc17\/[^"]*\.c\)",$/\1/p' \
    project.json |
    sort -u
)

missing="$(
  comm -23 \
    <(printf '%s\n' "${helper_sources[@]}" | sort -u) \
    <(printf '%s\n' "${project_sources[@]}" | sort -u)
)"

if [[ -n "$missing" ]]; then
  echo "$missing" | sed 's/^/  /' >&2
  fail "project.json is missing helper archive sources listed by scripts/build_omni_chelpers.sh"
fi

grep -q -F 'OMNI_RUNTIME_TOOLCHAIN_LIB_PATH' scripts/run_e2e.sh \
  || fail "scripts/run_e2e.sh no longer exposes the runtime library path override"

grep -q -F 'OMNI_AOT_LINK_LIBRARY_PATH' src/entry_build_backend_compile.c3 \
  || fail "AOT backend no longer exposes the link library path override"

echo "OK: build configuration parity checks passed."
