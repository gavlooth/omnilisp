# I/O, Networking & JSON

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 21. I/O

All I/O goes through algebraic effects. When no handler is installed, a fast
path calls the raw primitives directly (zero overhead).

### Console

```lisp
(print "hello")          ;; no newline
(println "hello")        ;; with newline
(display 42)             ;; display any value
(newline)                ;; just a newline
```

### Files

```lisp
(define rf
  (spawn (lambda ()
    (write-file "out.txt" "hello")
    (read-file "out.txt"))))
(await rf)                            ;; => "hello"

(define ff
  (spawn (lambda () (file-exists? "out.txt"))))
(await ff)                            ;; => true

(define lf
  (spawn (lambda () (read-lines "out.txt"))))
(await lf)                            ;; => list of lines

(load "module.omni")                  ;; load and evaluate file
```

`read-file`, `write-file`, `file-exists?`, and `read-lines` are async file
effects and require running fiber context. Calling them outside a fiber raises
deterministic `io/*-fiber-required` errors.

### Intercepting I/O

```lisp
;; Suppress output
(handle (begin (println "silent") 42)
  (io/println x (resolve nil)))
;; => 42, nothing printed

;; Capture output
(handle (begin (println "captured") nil)
  (io/println x x))
;; => "captured"

;; Mock filesystem
(handle (read-file "config.txt")
  (io/read-file path (resolve "mocked content")))
;; => "mocked content"
```

---

## 22. Networking

### TCP

```lisp
(define tf
  (spawn (lambda ()
    (define conn (tcp-connect "example.com" 80))
    (tcp-write conn "GET / HTTP/1.0\r\nHost: example.com\r\n\r\n")
    (define response (tcp-read conn))
    (tcp-close conn)
    response)))

(await tf)
```

### TCP Server

```lisp
(define sf
  (spawn (lambda ()
    ;; listener on localhost:8080 (optional backlog third arg)
    (define listener (tcp-listen "127.0.0.1" 8080 128))
    ;; accept one client connection
    (define client (tcp-accept listener))
    ;; handle request bytes
    (define req (tcp-read client))
    (tcp-write client "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nOK")
    (tcp-close client)
    (tcp-close listener))))

(await sf)
```

`tcp-connect`, `tcp-listen`, `tcp-accept`, `tcp-read`, `tcp-write`, and
`tcp-close` are async fiber operations and require running fiber context.
Calling them outside a fiber raises deterministic `io/tcp-*-fiber-required`
errors.

### API Layering Contract (Anti-Drift)

The runtime intentionally keeps TCP as operation-level effects and raw
primitives (`io/tcp-connect` -> `__raw-tcp-connect`, etc.) instead of one
monolithic runtime entrypoint. This is required for:
- precise effect interception/mocking per operation,
- direct fast-path mapping when no handler is installed,
- stable policy and parity checks at primitive granularity.

Public ergonomic facades (for example, a unified `tcp` helper) are allowed, but
they must stay thin and delegate to canonical operation wrappers (`tcp-connect`,
`tcp-read`, `tcp-write`, `tcp-close`). New code should not bypass that boundary
or introduce parallel runtime plumbing.

### UDP

```lisp
(define recv (udp-socket))
(udp-bind recv "127.0.0.1" 45001)

(define snd (udp-socket))
(udp-send snd "127.0.0.1" 45001 "ping")
(define payload (udp-recv recv))

(udp-close snd)
(udp-close recv)
```

`udp-recv` is fiber-safe and uses the scheduler async readiness bridge in fiber
context.

### Unix Domain Sockets

```lisp
(define pf
  (spawn (lambda ()
    (define path "/tmp/omni_example.sock")
    (define listener (pipe-listen path))
    (define client (pipe-connect path))
    (define server (tcp-accept listener))
    (tcp-write client "pipe-ok")
    (define msg (tcp-read server))
    (tcp-close client)
    (tcp-close server)
    (tcp-close listener)
    (fs-unlink path)
    msg)))

(await pf)
```

`pipe-connect`/`pipe-listen` return `tcp-handle`-compatible stream/listener
handles, so existing `tcp-accept`/`tcp-read`/`tcp-write`/`tcp-close` APIs apply.
In async paths these primitives run on libuv and require running fiber context
for effect execution.

### Process Control

```lisp
(define proc (process-spawn "/bin/sh" ["-c" "printf omni"] nil))
(define out (fs-read (ref proc 'stdout) 32))
(define status (await (spawn (lambda () (process-wait (ref proc 'handle))))))

(fs-close (ref proc 'stdin))
(fs-close (ref proc 'stdout))
(fs-close (ref proc 'stderr))
```

`process-spawn` returns a dict with:
- `'handle` (process handle for `process-wait`/`process-kill`)
- `'pid` (child pid)
- `'stdin`/`'stdout`/`'stderr` (`fs-handle` values compatible with `fs-read`/`fs-write`/`fs-close`)
- `env` may be `nil` (inherit parent environment) or a list/array of
  `KEY=VALUE` strings/symbols for explicit environment override.
- `process-wait` is fiber-only and raises `io/process-wait-fiber-required`
  outside running fiber context.

### Signals

```lisp
(define hits {'n 0})
(define h (signal-handle 10 (lambda (sig) (dict-set! hits 'n (+ (ref hits 'n) 1)))))

(shell "kill -USR1 $PPID")
(async-sleep 20)

(signal-unhandle h)
(ref hits 'n)
```

`signal-handle` installs a libuv-backed watcher and invokes `callback` with the
signal number argument when delivery is observed by the runtime event loop.

### DNS

```lisp
(dns-resolve "example.com")   ;; => IP address string
```

### TLS

```lisp
(define tf
  (spawn (lambda ()
    (define tcp (tcp-connect "example.com" 443))
    (define tls (tls-connect tcp "example.com"))
    ;; explicit CA bundle path (optional third argument)
    ;; (define tls (tls-connect tcp "example.com" "/etc/ssl/certs/ca-certificates.crt"))
    ;; client auth mTLS (optional client cert/key PEM pair)
    ;; (define tls (tls-connect tcp "example.com" "client-cert.pem" "client-key.pem"))
    ;; CA + mTLS + session resumption policy
    ;; (define tls (tls-connect tcp "example.com"
    ;;                         "/etc/ssl/certs/ca-certificates.crt"
    ;;                         "client-cert.pem"
    ;;                         "client-key.pem"
    ;;                         true))
    (tls-write tls "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n")
    (define response (tls-read tls))
    (tls-close tls)
    response)))

(await tf)
```

`tls-connect` performs certificate validation against a CA trust store. Trust
bundle resolution order is: `OMNI_TLS_CA_FILE`, `SSL_CERT_FILE`, then common
system CA bundle paths (`/etc/ssl/certs/ca-certificates.crt`, etc.).
Optional `resume-session?` accepts `true` or `false` as the final argument.
When `true`, Omni caches a BearSSL client session per hostname in-process and
tries session resumption on future `tls-connect` calls for that host.

Server-side wrap (RSA key + PEM cert chain):

```lisp
(define sf
  (spawn (lambda ()
    (define listener (tcp-listen "127.0.0.1" 8443 8))
    (define tcp-client (tcp-accept listener))
    (define tls-client
      (tls-server-wrap tcp-client "server-cert.pem" "server-key.pem"))
    tls-client)))

(await sf)
```

Current `tls-server-wrap` support expects a PEM certificate chain and PEM RSA
private key path.

### HTTP

```lisp
(define hf
  (spawn (lambda () (http-get "https://example.com"))))

(await hf)   ;; => response dict
```

### Timer

```lisp
(async-sleep 1000)   ;; sleep for 1 second
```

All network operations go through effects and can be intercepted for testing.

---

## 23. JSON

```lisp
;; Parse JSON string to Omni values
(json-parse "{\"name\": \"Alice\", \"age\": 30}")
;; => dict: {'name "Alice" 'age 30}

(json-parse "[1, 2, 3]")
;; => array: [1 2 3]

;; Pointer lookup
(define payload (json-parse "{\"users\":[{\"id\":1,\"name\":\"Alice\"}, {\"id\":2,\"name\":\"Bob\"}]}"))
(json-get payload "/users/1/name")
;; => "Bob"

;; Emit Omni values as JSON
(json-emit {'name "Alice" 'age 30})
;; => "{\"name\":\"Alice\",\"age\":30}"

;; Pretty-printed JSON
(json-emit-pretty {'name "Alice" 'age 30})
;; => "{\n  \"name\": \"Alice\",\n  \"age\": 30\n}"

;; Emit with explicit writer flags
(json-emit [1.5 2.0] '((precision 2) (escape-slashes true) (newline-at-end true)))
;; => "[1.50,2.00]\n"

;; Parse with permissive options
(json-parse "{\"a\": 1 /*comment*/ , \"b\": 2}" '(allow-comments))
;; => {'a 1 'b 2}

(json-parse "[1,2,3,]" '(allow-trailing-commas))
;; => [1 2 3]

(json-parse "[NaN, Infinity, -Infinity]" '(allow-nan-inf))
;; => [NaN Infinity -Infinity]

;; Parse permissive matrix in one call
(json-parse "{\"a\": 1 /*comment*/ , \"b\": 2}" '(allow-comments allow-trailing-commas))
;; => {'a 1 'b 2}
```

JSON types map to Omni types: object -> dict, array -> array, string -> string,
number -> int or double, null -> nil, true/false -> true/nil.
