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
(read-file "data.txt")              ;; => file contents as string
(write-file "out.txt" "hello")       ;; write string to file
(file-exists? "data.txt")            ;; => true/false
(read-lines "data.txt")              ;; => list of lines
(load "module.omni")                  ;; load and evaluate file
```

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
(define conn (tcp-connect "example.com" 80))
(tcp-write conn "GET / HTTP/1.0\r\nHost: example.com\r\n\r\n")
(define response (tcp-read conn))
(tcp-close conn)
```

### TCP Server

```lisp
;; listener on localhost:8080 (optional backlog third arg)
(define listener (tcp-listen "127.0.0.1" 8080 128))

;; accept one client connection
(define client (tcp-accept listener))

;; handle request bytes
(define req (tcp-read client))
(tcp-write client "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nOK")
(tcp-close client)
(tcp-close listener)
```

`tcp-accept` is fiber-safe: when called inside a running fiber it offloads the
blocking `accept()` syscall to the runtime worker and resumes the fiber on
completion.

### DNS

```lisp
(dns-resolve "example.com")   ;; => IP address string
```

### TLS

```lisp
(define tcp (tcp-connect "example.com" 443))
(define tls (tls-connect tcp "example.com"))
(tls-write tls "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n")
(define response (tls-read tls))
(tls-close tls)
```

### HTTP

```lisp
(http-get "https://example.com")   ;; => response string
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

;; Emit Omni values as JSON
(json-emit {'name "Alice" 'age 30})
;; => "{\"name\":\"Alice\",\"age\":30}"

;; Pretty-printed JSON
(json-emit-pretty {'name "Alice" 'age 30})
;; => "{\n  \"name\": \"Alice\",\n  \"age\": 30\n}"
```

JSON types map to Omni types: object -> dict, array -> array, string -> string,
number -> int or double, null -> nil, true/false -> true/nil.
