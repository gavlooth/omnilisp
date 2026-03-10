#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

extract_function_body() {
  local file="$1"
  local fn_name="$2"
  local out_file="$3"
  awk -v marker="fn Value* ${fn_name}(" '
    BEGIN {
      in_fn = 0
      seen_open = 0
      completed = 0
      depth = 0
      found = 0
    }
    {
      if (!in_fn && index($0, marker) > 0) {
        in_fn = 1
        found = 1
      }
      if (in_fn) {
        print $0
        line = $0
        opens = gsub(/\{/, "", line)
        closes = gsub(/\}/, "", line)
        if (opens > 0) seen_open = 1
        depth += opens
        depth -= closes
        if (seen_open && depth <= 0) {
          completed = 1
          exit 0
        }
      }
    }
    END {
      if (!found) exit 2
      if (!completed) exit 3
    }
  ' "$file" > "$out_file"
}

resolve_function_file() {
  local preferred_file="$1"
  local fn_name="$2"
  local marker="fn Value* ${fn_name}("

  if [[ -f "$preferred_file" ]] && rg -q -F "$marker" "$preferred_file"; then
    echo "$preferred_file"
    return 0
  fi

  local discovered
  discovered="$(rg -l -F "$marker" src/lisp/*.c3 2>/dev/null | head -n 1 || true)"
  if [[ -n "$discovered" ]]; then
    echo "$discovered"
    return 0
  fi

  return 1
}

check_required_pattern() {
  local body_file="$1"
  local required_pattern="$2"
  local file="$3"
  local fn_name="$4"
  if ! rg -q "$required_pattern" "$body_file"; then
    echo "FAIL: async fallback policy missing required guard in ${fn_name} (${file})"
    echo "  required pattern: $required_pattern"
    exit 1
  fi
}

check_forbidden_pattern() {
  local body_file="$1"
  local forbidden_pattern="$2"
  local file="$3"
  local fn_name="$4"
  if rg -n "$forbidden_pattern" "$body_file" >/dev/null; then
    echo "FAIL: async fallback policy violation in ${fn_name} (${file})"
    echo "  forbidden pattern: $forbidden_pattern"
    rg -n "$forbidden_pattern" "$body_file" | sed 's/^/    /'
    exit 1
  fi
}

check_function_policy() {
  local file="$1"
  local fn_name="$2"
  local required_pattern="$3"
  shift 3
  local body_file="$tmp_dir/${fn_name}.body"
  local source_file

  if ! source_file="$(resolve_function_file "$file" "$fn_name")"; then
    echo "FAIL: missing source for async fallback policy function ${fn_name} (preferred: ${file})"
    exit 1
  fi
  if ! extract_function_body "$source_file" "$fn_name" "$body_file"; then
    echo "FAIL: could not extract function body for ${fn_name} from ${source_file}"
    exit 1
  fi

  check_required_pattern "$body_file" "$required_pattern" "$source_file" "$fn_name"
  local forbidden_pattern
  for forbidden_pattern in "$@"; do
    check_forbidden_pattern "$body_file" "$forbidden_pattern" "$source_file" "$fn_name"
  done
}

check_function_policy \
  "src/lisp/async_tcp_transport_core.c3" \
  "prim_tcp_connect" \
  "io/tcp-connect-fiber-required" \
  "tcp_connect_blocking\\(" \
  "scheduler_run_internal_io_offload_job\\(" \
  "scheduler_run_offload_sync\\("

check_function_policy \
  "src/lisp/async_tcp_transport_core.c3" \
  "prim_tcp_listen" \
  "io/tcp-listen-fiber-required" \
  "tcp_listen_blocking\\(" \
  "scheduler_run_internal_io_offload_job\\(" \
  "scheduler_run_offload_sync\\("

check_function_policy \
  "src/lisp/async_tcp_transport_core.c3" \
  "prim_tcp_accept" \
  "io/tcp-accept-fiber-required" \
  "tcp_accept_fd\\(" \
  "scheduler_run_internal_io_offload_job\\(" \
  "scheduler_run_offload_sync\\("

check_function_policy \
  "src/lisp/async_tcp_transport_core.c3" \
  "prim_tcp_close" \
  "io/tcp-close-fiber-required" \
  "scheduler_run_internal_io_offload_job\\(" \
  "scheduler_run_offload_sync\\("

check_function_policy \
  "src/lisp/async_tcp_transport_core.c3" \
  "prim_tcp_read" \
  "io/tcp-read-fiber-required" \
  "c_recv\\(" \
  "scheduler_run_internal_io_offload_job\\(" \
  "scheduler_run_offload_sync\\("

check_function_policy \
  "src/lisp/async_tcp_transport_core.c3" \
  "prim_tcp_write" \
  "io/tcp-write-fiber-required" \
  "c_send\\(" \
  "scheduler_run_internal_io_offload_job\\(" \
  "scheduler_run_offload_sync\\("

check_function_policy \
  "src/lisp/async_udp_pipe.c3" \
  "prim_pipe_connect" \
  "io/pipe-connect-fiber-required" \
  "\\bpipe_connect_fd\\(" \
  "c_connect\\(" \
  "scheduler_run_internal_io_offload_job\\(" \
  "scheduler_run_offload_sync\\("

check_function_policy \
  "src/lisp/async_udp_pipe.c3" \
  "prim_pipe_listen" \
  "io/pipe-listen-fiber-required" \
  "\\bpipe_listen_fd\\(" \
  "c_bind\\(" \
  "c_listen\\(" \
  "scheduler_run_internal_io_offload_job\\(" \
  "scheduler_run_offload_sync\\("

check_function_policy \
  "src/lisp/http.c3" \
  "prim_http_get" \
  "io/http-get-fiber-required" \
  "scheduler_run_internal_io_offload_job\\(" \
  "scheduler_run_offload_sync\\("

check_function_policy \
  "src/lisp/http.c3" \
  "prim_http_request" \
  "io/http-request-fiber-required" \
  "scheduler_run_internal_io_offload_job\\(" \
  "scheduler_run_offload_sync\\("

check_function_policy \
  "src/lisp/async_process_signal_dns.c3" \
  "prim_process_wait" \
  "io/process-wait-fiber-required" \
  "omni_uv_process_wait\\("

check_function_policy \
  "src/lisp/prim_io.c3" \
  "prim_read_file" \
  "io/read-file-fiber-required" \
  "io_uv_read_all_file\\(" \
  "omni_uv_fs_open\\(" \
  "omni_uv_fs_read\\(" \
  "scheduler_run_offload_sync\\("

check_function_policy \
  "src/lisp/prim_io.c3" \
  "prim_write_file" \
  "io/write-file-fiber-required" \
  "omni_uv_fs_open\\(" \
  "omni_uv_fs_write\\(" \
  "scheduler_run_offload_sync\\("

check_function_policy \
  "src/lisp/prim_io.c3" \
  "prim_file_exists" \
  "io/file-exists\\?-fiber-required" \
  "omni_uv_fs_stat_basic\\(" \
  "scheduler_run_offload_sync\\("

check_function_policy \
  "src/lisp/prim_io.c3" \
  "prim_read_lines" \
  "io/read-lines-fiber-required" \
  "io_uv_read_all_file\\(" \
  "omni_uv_fs_open\\(" \
  "omni_uv_fs_read\\(" \
  "scheduler_run_offload_sync\\("

check_function_policy \
  "src/lisp/tls.c3" \
  "prim_tls_connect" \
  "io/tls-connect-fiber-required" \
  "br_ssl_client_init_full\\(" \
  "br_ssl_client_reset\\(" \
  "scheduler_run_offload_sync\\("

check_function_policy \
  "src/lisp/tls.c3" \
  "prim_tls_read" \
  "io/tls-read-fiber-required" \
  "br_sslio_read\\(" \
  "scheduler_run_offload_sync\\("

check_function_policy \
  "src/lisp/tls.c3" \
  "prim_tls_write" \
  "io/tls-write-fiber-required" \
  "br_sslio_write_all\\(" \
  "br_sslio_flush\\(" \
  "scheduler_run_offload_sync\\("

check_function_policy \
  "src/lisp/tls.c3" \
  "prim_tls_close" \
  "io/tls-close-fiber-required" \
  "br_sslio_close\\(" \
  "scheduler_run_offload_sync\\("

echo "OK: async fallback policy guard passed."
