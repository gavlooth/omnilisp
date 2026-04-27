#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT_DIR"

MAIN_BIN="$ROOT_DIR/build/main"
SERVER_SCRIPT="$ROOT_DIR/tests/lib/tls/server_once.omni"
CA_CERT="$ROOT_DIR/tests/lib/tls/ca.cert.pem"

if [[ ! -x "$MAIN_BIN" ]]; then
  echo "build/main not found. Run: c3c build"
  exit 1
fi

CLIENT_SCRIPT="$(mktemp "$ROOT_DIR/.tmp_tls_client_XXXXXX.omni")"
SERVER_OUT="$(mktemp "$ROOT_DIR/.tmp_tls_server_out_XXXXXX.log")"
SERVER_ERR="$(mktemp "$ROOT_DIR/.tmp_tls_server_err_XXXXXX.log")"
CLIENT_OUT="$(mktemp "$ROOT_DIR/.tmp_tls_client_out_XXXXXX.log")"
READY_FILE="$(mktemp "$ROOT_DIR/.tmp_tls_ready_XXXXXX")"
rm -f "$READY_FILE"
SERVER_PID=""
STOP_SERVER_RC=0

cleanup() {
  if [[ -n "${SERVER_PID:-}" ]] && kill -0 "$SERVER_PID" >/dev/null 2>&1; then
    kill "$SERVER_PID" >/dev/null 2>&1 || true
    wait "$SERVER_PID" >/dev/null 2>&1 || true
  fi
  rm -f "$CLIENT_SCRIPT" "$SERVER_OUT" "$SERVER_ERR" "$CLIENT_OUT" "$READY_FILE"
}
trap cleanup EXIT

cat >"$CLIENT_SCRIPT" <<'OMNI'
(block
  (define p (parse-number (default (getenv "OMNI_TLS_TEST_PORT") "45101")))
  (define host (default (getenv "OMNI_TLS_CLIENT_HOST") "localhost"))
  (define expect-mode (default (getenv "OMNI_TLS_EXPECT_MODE") "pong"))
  (define ca (default (getenv "OMNI_TLS_CA") "tests/lib/tls/ca.cert.pem"))
  (define cert (getenv "OMNI_TLS_CLIENT_CERT"))
  (define key (getenv "OMNI_TLS_CLIENT_KEY"))
  (define resume? (= (default (getenv "OMNI_TLS_CLIENT_RESUME") "0") "1"))
  (define outcome
    (await
      (spawn (lambda ()
        (define c nil)
        (define ctls nil)
        (define result
          (try
            (lambda (ignored)
              (block
                (set! c (tcp-connect "127.0.0.1" p))
                (set! ctls
                  (if (and cert key)
                      (if resume?
                          (tls-connect c host ca cert key true)
                          (tls-connect c host ca cert key))
                      (tls-connect c host ca)))
                (tls-write ctls "ping")
                (tls-read ctls)))
            (lambda (e) (if (and (dict? e) (has? e 'message)) (ref e 'message) e))))
        (if ctls (try (lambda (ignored) (tls-close ctls)) (lambda (e) nil)) nil)
        (if c (try (lambda (ignored) (tcp-close c)) (lambda (e) nil)) nil)
        result))))
  (define ok
    (if (= expect-mode "pong")
        (= outcome "pong")
        (if (= expect-mode "handshake-error")
            (and (string? outcome)
                 (or (string-contains? outcome "write failed")
                     (or (string-contains? outcome "read failed")
                         (string-contains? outcome "reset failed"))))
            (if (= expect-mode "mtls-key-error")
                (and (string? outcome)
                     (string-contains? outcome "failed to configure client cert/key"))
                false))))
  (if (not ok) (print outcome) nil)
  (assert! ok "tls probe assertion failed")
)
OMNI

start_server() {
  local port="$1"
  local cert="$2"
  local key="$3"

  if [[ -n "$cert" && -n "$key" ]]; then
    ASAN_OPTIONS=detect_leaks=0 \
      OMNI_TLS_TEST_PORT="$port" \
      OMNI_TLS_TEST_CERT="$cert" \
      OMNI_TLS_TEST_KEY="$key" \
      OMNI_TLS_READY_FILE="$READY_FILE" \
      LD_LIBRARY_PATH=/usr/local/lib \
      "$MAIN_BIN" "$SERVER_SCRIPT" >"$SERVER_OUT" 2>"$SERVER_ERR" &
  else
    ASAN_OPTIONS=detect_leaks=0 \
      OMNI_TLS_TEST_PORT="$port" \
      OMNI_TLS_READY_FILE="$READY_FILE" \
      LD_LIBRARY_PATH=/usr/local/lib \
      "$MAIN_BIN" "$SERVER_SCRIPT" >"$SERVER_OUT" 2>"$SERVER_ERR" &
  fi
  SERVER_PID=$!
}

wait_for_server_ready() {
  for _ in $(seq 1 300); do
    if [[ -s "$READY_FILE" ]]; then
      return 0
    fi
    if [[ -n "${SERVER_PID:-}" ]] && ! kill -0 "$SERVER_PID" >/dev/null 2>&1; then
      return 1
    fi
    sleep 0.01
  done
  return 1
}

stop_server() {
  STOP_SERVER_RC=0
  if [[ -z "${SERVER_PID:-}" ]]; then
    return
  fi

  for _ in $(seq 1 300); do
    if ! kill -0 "$SERVER_PID" >/dev/null 2>&1; then
      wait "$SERVER_PID" || STOP_SERVER_RC=$?
      SERVER_PID=""
      return
    fi
    sleep 0.01
  done

  kill "$SERVER_PID" >/dev/null 2>&1 || true
  wait "$SERVER_PID" || STOP_SERVER_RC=$?
  SERVER_PID=""
}

run_case() {
  local name="$1"
  local port="$2"
  local server_cert="$3"
  local server_key="$4"
  local host="$5"
  local expect_mode="$6"
  local client_cert="$7"
  local client_key="$8"
  local client_resume="$9"

  : >"$SERVER_OUT"
  : >"$SERVER_ERR"
  : >"$CLIENT_OUT"
  rm -f "$READY_FILE"

  start_server "$port" "$server_cert" "$server_key"
  if ! wait_for_server_ready; then
    stop_server
    echo "[FAIL] $name (server did not report readiness)"
    if [[ -s "$SERVER_ERR" ]]; then
      echo "  server stderr:"
      sed 's/^/    /' "$SERVER_ERR" || true
    fi
    return 1
  fi

  local client_rc=0
  local -a client_env=(
    "OMNI_TLS_TEST_PORT=$port"
    "OMNI_TLS_CLIENT_HOST=$host"
    "OMNI_TLS_EXPECT_MODE=$expect_mode"
    "OMNI_TLS_CA=$CA_CERT"
    "OMNI_TLS_CLIENT_RESUME=$client_resume"
    "LD_LIBRARY_PATH=/usr/local/lib"
  )
  if [[ -n "$client_cert" ]]; then
    client_env+=("OMNI_TLS_CLIENT_CERT=$client_cert")
  fi
  if [[ -n "$client_key" ]]; then
    client_env+=("OMNI_TLS_CLIENT_KEY=$client_key")
  fi

  env "${client_env[@]}" "$MAIN_BIN" "$CLIENT_SCRIPT" >"$CLIENT_OUT" 2>&1 || client_rc=$?

  stop_server
  local server_rc="$STOP_SERVER_RC"

  local ok=0
  if [[ "$client_rc" -eq 0 ]]; then
    if [[ "$expect_mode" == "pong" ]]; then
      if [[ "$server_rc" -eq 0 ]]; then
        ok=1
      fi
    else
      # Negative TLS cases validate client-side rejection semantics.
      # The one-shot fixture may require forced termination after the client aborts handshake early.
      ok=1
    fi
  fi

  if [[ "$ok" -eq 1 ]]; then
    if [[ "$expect_mode" != "pong" && "$server_rc" -ne 0 ]]; then
      echo "[PASS] $name (server fixture terminated by harness rc=$server_rc)"
    else
      echo "[PASS] $name"
    fi
    return 0
  fi

  echo "[FAIL] $name (client_rc=$client_rc server_rc=$server_rc)"
  echo "  client output:"
  sed 's/^/    /' "$CLIENT_OUT" || true
  if [[ -s "$SERVER_ERR" ]]; then
    echo "  server stderr:"
    sed 's/^/    /' "$SERVER_ERR" || true
  fi
  return 1
}

pass=0
fail=0

if run_case "tls valid cert handshake" 46201 "" "" "localhost" "pong" "" "" "0"; then ((pass+=1)); else ((fail+=1)); fi
if run_case "tls rejects untrusted cert" 46202 "tests/lib/tls/bad-server.cert.pem" "tests/lib/tls/bad-server.key.pem" "localhost" "handshake-error" "" "" "0"; then ((pass+=1)); else ((fail+=1)); fi
if run_case "tls rejects hostname mismatch" 46203 "" "" "not-localhost" "handshake-error" "" "" "0"; then ((pass+=1)); else ((fail+=1)); fi
if run_case "tls mTLS optional cert/key pass" 46204 "" "" "localhost" "pong" "tests/lib/tls/client.cert.pem" "tests/lib/tls/client.key.pem" "1"; then ((pass+=1)); else ((fail+=1)); fi
if run_case "tls mTLS invalid key fails" 46205 "" "" "localhost" "mtls-key-error" "tests/lib/tls/client.cert.pem" "tests/lib/tls/missing-client.key.pem" "0"; then ((pass+=1)); else ((fail+=1)); fi

echo "TLS targeted summary: pass=$pass fail=$fail"
if [[ "$fail" -ne 0 ]]; then
  exit 1
fi
