;; Omni Lisp Standard Library
;; Embedded at compile time via $embed("stdlib/stdlib.lisp")

;; =========================================================================
;; Built-in Effect Declarations (typed I/O effects)
;; =========================================================================
(define [effect] (io/print (^Any x)))
(define [effect] (io/println (^Any x)))
(define [effect] (io/display (^Any x)))
(define [effect] (io/newline (^Any x)))
(define [effect] (io/read-line (^Any x)))
(define [effect] (io/read-file (^String path)))
(define [effect] (io/write-file (^Any x)))
(define [effect] (io/file-exists? (^String path)))
(define [effect] (io/read-lines (^String path)))
(define [effect] (io/fs-open (^Any args)))
(define [effect] (io/fs-read (^Any args)))
(define [effect] (io/fs-write (^Any args)))
(define [effect] (io/fs-close (^Any handle)))
(define [effect] (io/fs-stat (^String path)))
(define [effect] (io/fs-readdir (^String path)))
(define [effect] (io/fs-rename (^Any args)))
(define [effect] (io/fs-unlink (^String path)))
(define [effect] (io/tcp-connect (^Any args)))
(define [effect] (io/tcp-listen (^Any args)))
(define [effect] (io/tcp-accept (^Any handle)))
(define [effect] (io/tcp-read (^Any handle)))
(define [effect] (io/tcp-write (^Any args)))
(define [effect] (io/tcp-close (^Any handle)))
(define [effect] (io/udp-socket (^Any x)))
(define [effect] (io/udp-bind (^Any args)))
(define [effect] (io/udp-send (^Any args)))
(define [effect] (io/udp-recv (^Any handle)))
(define [effect] (io/udp-close (^Any handle)))
(define [effect] (io/pipe-connect (^String path)))
(define [effect] (io/pipe-listen (^String path)))
(define [effect] (io/process-spawn (^Any args)))
(define [effect] (io/process-wait (^Any handle)))
(define [effect] (io/process-kill (^Any args)))
(define [effect] (io/signal-handle (^Any args)))
(define [effect] (io/signal-unhandle (^Any handle)))
(define [effect] (io/dns-resolve (^String host)))
(define [effect] (io/async-sleep (^Integer ms)))
(define [effect] (io/offload (^Any job)))
(define [effect] (io/thread-spawn (^Any job)))
(define [effect] (io/thread-join (^Any thread-handle)))
(define [effect] (io/thread-join-timeout (^Any args)))
(define [effect] (io/thread-cancel (^Any thread-handle)))
(define [effect] (io/task-spawn (^Any job)))
(define [effect] (io/task-join (^Any task-handle)))
(define [effect] (io/task-join-timeout (^Any args)))
(define [effect] (io/task-cancel (^Any task-handle)))
(define [effect] (io/tls-connect (^Any args)))
(define [effect] (io/tls-server-wrap (^Any args)))
(define [effect] (io/tls-read (^Any args)))
(define [effect] (io/tls-write (^Any args)))
(define [effect] (io/tls-close (^Any handle)))

;; Error effect — all recoverable errors flow through this
(define [effect] (raise (^Any msg)))

;; =========================================================================
;; Standard Macros (defined before HOFs since macros may be used by them)
;; =========================================================================
(define [macro] when
  (syntax-match
    ([test .. body]
      (template (if (insert test) (block (splice body)) nil)))))

(define [macro] unless
  (syntax-match
    ([test .. body]
      (template (if (insert test) nil (block (splice body)))))))

;; branch: cond-style conditional chain with explicit default marker
;; Usage:
;;   (branch (cond1 expr1) (cond2 expr2) ... (_ default-expr))
;; Rules:
;;   - _ is the default clause marker
;;   - _ must appear only in final position
;;   - if no condition matches and no default is provided, result is nil
(define [macro] branch
  (syntax-match
    ([]
      (template nil))
    ([['__placeholder result]]
      (template (insert result)))
    ([['__placeholder result] .. rest]
      (template (assert! false "branch: _ default clause must be final")))
    ([[test result]]
      (template (if (insert test) (insert result) nil)))
    ([[test result] .. rest]
      (template (if (insert test) (insert result) (branch .. rest))))))
(define (default v fallback) (if (null? v) fallback v))

;; with-defaults: wrap body with nil-checked defaults for dict destructured bindings
;; Usage: (with-defaults name1 val1 name2 val2 ... body)
;; Last argument is the body expression. Preceding pairs are (name default-value).
;; Expands to nested lets: (let (name1 (default name1 val1)) (let (name2 ...) body))
(define [macro] with-defaults
  (syntax-match
    ([body]
      (template (insert body)))
    ([name val .. rest]
      (template (let ((insert name) (default (insert name) (insert val)))
                  (with-defaults .. rest))))))
(define with-trampoline (lambda (thunk) (handle (thunk nil) (bounce next-thunk (resolve (with-trampoline next-thunk))))))

;; =========================================================================
;; Type Predicates (defined via is? and abstract type hierarchy)
;; =========================================================================
(define (integer? x) (is? x 'Integer))
(define (int? x) (integer? x))
(define (double? x) (is? x 'Double))
(define (number? x) (is? x 'Number))
(define (string? x) (is? x 'String))
(define (symbol? x) (is? x 'Symbol))
(define (boolean? x) (or (null? x) (= x true)))
(define (bool? x) (boolean? x))
(define (list? x) (or (null? x) (pair? x)))
(define (closure? x) (is? x 'Closure))
(define (array? x) (is? x 'Array))
(define (dict? x) (is? x 'Dictionary))
(define (dictionary? x) (is? x 'Dictionary))

;; =========================================================================
;; Numeric Predicates (defined via dispatched comparison ops)
;; =========================================================================
(define (zero? x) (= x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (even? x) (= (% x 2) 0))
(define (odd? x) (not (= (% x 2) 0)))

;; =========================================================================
;; Higher-Order Functions (multi-arg, dispatched on collection type)
;; =========================================================================

;; reverse: (reverse lst) — must be defined before map/filter (they depend on it)
(define (reverse (^List lst)) (__reverse-list lst))
(define (reverse (^Array arr)) (let (len (length arr) result []) (let loop (i len) (if (= i 0) result (let (j (- i 1)) (block (push! result (ref arr j)) (loop j)))))))

;; map: (map f coll) — apply f to each element, return same collection type
(define (map f (^List lst)) (let loop (xs lst acc nil) (if (null? xs) (reverse acc) (loop (cdr xs) (cons (f (car xs)) acc)))))
(define (map f (^Array arr)) (let (len (length arr) result []) (let loop (i 0) (if (= i len) result (block (push! result (f (ref arr i))) (loop (+ i 1)))))))
(define (map (^Closure f)) (lambda (coll) (map f coll)))

;; filter: (filter pred coll) — keep elements where (pred x) is truthy
(define (filter pred (^List lst)) (let loop (xs lst acc nil) (if (null? xs) (reverse acc) (if (pred (car xs)) (loop (cdr xs) (cons (car xs) acc)) (loop (cdr xs) acc)))))
(define (filter pred (^Array arr)) (let (len (length arr) result []) (let loop (i 0) (if (= i len) result (if (pred (ref arr i)) (block (push! result (ref arr i)) (loop (+ i 1))) (loop (+ i 1)))))))
(define (filter (^Closure pred)) (lambda (coll) (filter pred coll)))

;; foldl: (foldl f acc coll) — left fold, f takes 2 args: (f acc x)
(define (foldl f acc (^List lst)) (let loop (a acc xs lst) (if (null? xs) a (loop (f a (car xs)) (cdr xs)))))
(define (foldl f acc (^Array arr)) (let (len (length arr)) (let loop (a acc i 0) (if (= i len) a (loop (f a (ref arr i)) (+ i 1))))))

;; foldr: (foldr f init coll) — right fold, f takes 2 args: (f x acc)
(define (foldr f init (^List lst)) (foldl (lambda (acc x) (f x acc)) init (reverse lst)))
(define (foldr f init (^Array arr)) (let (len (length arr)) (let loop (a init i len) (if (= i 0) a (let (j (- i 1)) (loop (f (ref arr j) a) j))))))

;; append: (append a b) — concatenate two lists
(define (append a b) (let loop (xs (reverse a) acc b) (if (null? xs) acc (loop (cdr xs) (cons (car xs) acc)))))

;; compose: (compose f g) — function composition, returns (lambda (x) (f (g x)))
(define (compose f g) (lambda (x) (f (g x))))

;; partial: (partial f . initial-args) — partial application
(define (partial f .. initial-args) (lambda (.. new-args) (apply f (append initial-args new-args))))

;; id: identity function
(define (id x) x)

;; nth: (nth n lst) — get nth element (0-indexed)
(define (nth n lst) (let loop (i n xs lst) (if (= i 0) (car xs) (loop (- i 1) (cdr xs)))))

;; take: (take n coll) — first n elements (shape-preserving)
(define (take n (^List lst)) (let loop (i n xs lst acc nil) (if (= i 0) (reverse acc) (if (null? xs) (reverse acc) (loop (- i 1) (cdr xs) (cons (car xs) acc))))))
(define (take n (^Array arr)) (let (len (length arr) result []) (let loop (i 0) (if (or (= i n) (= i len)) result (block (push! result (ref arr i)) (loop (+ i 1)))))))

;; drop: (drop n coll) — skip first n elements (shape-preserving)
(define (drop n (^List lst)) (let loop (i n xs lst) (if (= i 0) xs (if (null? xs) nil (loop (- i 1) (cdr xs))))))
(define (drop n (^Array arr)) (let (len (length arr) result []) (let loop (i n) (if (>= i len) result (block (push! result (ref arr i)) (loop (+ i 1)))))))

;; zip: (zip a b) — zip two collections
(define (zip (^List a) (^List b)) (let loop (xs a ys b acc nil) (if (or (null? xs) (null? ys)) (reverse acc) (loop (cdr xs) (cdr ys) (cons (cons (car xs) (car ys)) acc)))))
(define (zip (^Array a) (^Array b)) (let (la (length a) lb (length b) result []) (let loop (i 0) (if (or (= i la) (= i lb)) result (block (push! result (cons (ref a i) (ref b i))) (loop (+ i 1)))))))

;; range: (range n) — list from 0 to n-1 (iterative, builds in reverse)
(define (range n) (let loop (i (- n 1) acc nil) (if (< i 0) acc (loop (- i 1) (cons i acc)))))

;; for-each: (for-each f lst) — apply f to each element for side effects, return nil
(define (for-each f (^List lst)) (let loop (xs lst) (if (null? xs) nil (block (f (car xs)) (loop (cdr xs))))))
(define (for-each (^Closure f)) (lambda (lst) (for-each f lst)))

;; any?: (any? pred lst) — true if pred is truthy for any element
(define (any? pred (^List lst)) (let loop (xs lst) (if (null? xs) nil (if (pred (car xs)) true (loop (cdr xs))))))
(define (any? (^Closure pred)) (lambda (lst) (any? pred lst)))

;; every?: (every? pred lst) — true if pred is truthy for all elements
(define (every? pred (^List lst)) (let loop (xs lst) (if (null? xs) true (if (pred (car xs)) (loop (cdr xs)) nil))))
(define (every? (^Closure pred)) (lambda (lst) (every? pred lst)))

;; =========================================================================
;; Error Handling Convention (via algebraic effects)
;; =========================================================================

;; canonical-error-payload?: payload has the canonical raise shape
(define (canonical-error-payload? payload)
  (and (dict? payload)
       (and (has? payload 'code)
            (and (has? payload 'domain)
                 (and (has? payload 'message)
                      (has? payload 'data)))))
)

;; canonicalize-error-payload: normalize any raised value to canonical payload schema
(define (canonicalize-error-payload payload default-code default-domain default-message)
  (if (canonical-error-payload? payload)
      payload
      { 'code default-code
        'domain default-domain
        'message (if (string? payload) payload default-message)
        'data payload }))

;; try: run a thunk, catch errors via 'raise' effect
(define (try thunk handler)
  (handle (thunk nil)
          (raise msg
                 (handler (canonicalize-error-payload
                            msg
                            'runtime/string-raise-payload
                            'runtime
                            "raise: non-canonical payload")))))

;; assert!: check condition, raise if false
(define (assert! condition msg)
  (if condition
      true
      (signal raise
              { 'code 'stdlib/assert-failed
                'domain 'stdlib
                'message (if (string? msg) msg "assert!: condition failed")
                'data msg })))

;; =========================================================================
;; Mathematical Constants
;; =========================================================================
(define pi 3.141592653589793)
(define e 2.718281828459045)

;; =========================================================================
;; Additional HOFs
;; =========================================================================

;; flatten: flatten nested lists into a flat list
(define (flatten lst) (let loop (l lst acc nil) (if (null? l) (reverse acc) (if (pair? (car l)) (loop (cdr l) (let loop2 (inner (car l) a acc) (if (null? inner) a (loop2 (cdr inner) (cons (car inner) a))))) (loop (cdr l) (cons (car l) acc))))))

;; partition: split list by predicate, returns (kept . rejected)
(define (partition pred lst) (let loop (l lst yes nil no nil) (if (null? l) (cons (reverse yes) (reverse no)) (if (pred (car l)) (loop (cdr l) (cons (car l) yes) no) (loop (cdr l) yes (cons (car l) no))))))

;; remove: remove elements matching predicate (uses filter)
(define (remove pred lst) (filter (lambda (x) (not (pred x))) lst))

;; find: first element matching predicate, or nil
(define (find pred lst) (let loop (l lst) (if (null? l) nil (if (pred (car l)) (car l) (loop (cdr l))))))

;; =========================================================================
;; Generators & Lazy Streams (using existing effects)
;; =========================================================================

;; stream-yield: (stream-yield val) inside a generator — returns (cons val continuation)
;; Note: yield is now a primitive for fibers. Use stream-yield for generator patterns.
(define [macro] stream-yield
  (syntax-match
    ([val]
      (template (capture k (cons (insert val) k))))))

;; stream-take: consume n values from a generator continuation
(define (stream-take n gen) (let loop (i n g gen acc nil) (if (= i 0) (reverse acc) (if (null? g) (reverse acc) (let (pair (if (procedure? g) (g nil) g)) (if (null? pair) (reverse acc) (loop (- i 1) (cdr pair) (cons (car pair) acc))))))))

;; delay/force: promise (lazy evaluation)
(define (delay thunk) (let (result nil forced nil) (lambda () (if forced result (block (set! result (thunk nil)) (set! forced true) result)))))
(define (force p) (p))

;; =========================================================================
;; Iterators: Lazy Sequences
;; =========================================================================

;; iterator-empty: the empty iterator
(define iterator-empty (__make-iterator (lambda () nil)))

;; Iterator: canonical iterator constructor / conversion surface
(define (Iterator (^Closure thunk)) (__make-iterator thunk))
(define (Iterator (^List lst)) (__iterator-from-list lst))
(define (Iterator (^Array arr)) (__iterator-from-array arr))
(define (Iterator (^Dictionary d)) (__iterator-from-dict d))
(define (Iterator (^Iterator it)) it)

;; Iterator-dispatched map/filter (lazy)
(define (map f (^Iterator it)) (__iterator-map f it))
(define (filter pred (^Iterator it)) (__iterator-filter pred it))
(define (take n (^Iterator it)) (if (= n 0) iterator-empty (__iterator-take n it)))
(define (drop n (^Iterator it)) (if (= n 0) it (let (pair (next it)) (if (null? pair) iterator-empty (drop (- n 1) (cdr pair))))))
(define (zip (^Iterator a) (^Iterator b)) (__iterator-zip a b))
(define (foldl f acc (^Iterator it)) (__iterator-foldl f acc it))

;; Infinite sources
(define (range-from n) (__iterator-range-from n))
(define (repeat x) (__iterator-repeat x))
(define (cycle coll) (__iterator-cycle coll))

;; =========================================================================
;; I/O Effect Wrappers
;; These redefine I/O operations to go through the effects system.
;; When no handler is installed, the fast path calls __raw-* directly.
;; =========================================================================
(define print (lambda (x) (signal io/print x)))
(define println (lambda (x) (signal io/println x)))
(define display (lambda (x) (signal io/display x)))
(define newline (lambda () (signal io/newline nil)))
(define read-line (lambda () (signal io/read-line nil)))
(define read-file (lambda (path) (signal io/read-file path)))
(define write-file (lambda (path content) (signal io/write-file (cons path content))))
(define file-exists? (lambda (path) (signal io/file-exists? path)))
(define read-lines (lambda (path) (signal io/read-lines path)))
;; Canonical descriptive filesystem names remain exported (`filesystem-*`).
;; `fs-*` spellings are retained.
(define fs-open (lambda (path flags .. rest) (signal io/fs-open (cons path (cons flags rest)))))
(define fs-read (lambda (handle n) (signal io/fs-read (cons handle n))))
(define fs-write (lambda (handle data) (signal io/fs-write (cons handle data))))
(define fs-close (lambda (handle) (signal io/fs-close handle)))
(define fs-stat (lambda (path) (signal io/fs-stat path)))
(define fs-readdir (lambda (path) (signal io/fs-readdir path)))
(define fs-rename (lambda (src dst) (signal io/fs-rename (cons src dst))))
(define fs-unlink (lambda (path) (signal io/fs-unlink path)))
(define filesystem-open fs-open)
(define filesystem-read fs-read)
(define filesystem-write fs-write)
(define filesystem-close fs-close)
(define filesystem-stat fs-stat)
(define filesystem-read-directory fs-readdir)
(define filesystem-rename fs-rename)
(define filesystem-unlink fs-unlink)
(define (tcp-connect (^String host) (^Integer port)) (signal io/tcp-connect (cons host port)))
;; Keep untyped fallback so invalid args still flow through io/tcp-connect canonical payload errors.
(define (tcp-connect host port) (signal io/tcp-connect (cons host port)))
(define (tcp-listen (^String host) (^Integer port) .. rest) (signal io/tcp-listen (cons host (cons port rest))))
;; Keep untyped fallback so invalid args still flow through io/tcp-listen canonical payload errors.
(define (tcp-listen host port .. rest) (signal io/tcp-listen (cons host (cons port rest))))
(define (tcp-accept listener) (signal io/tcp-accept listener))
(define (tcp-read handle) (signal io/tcp-read handle))
(define (tcp-write handle data) (signal io/tcp-write (cons handle data)))
(define (tcp-close handle) (signal io/tcp-close handle))
(define (udp-socket) (signal io/udp-socket nil))
(define (udp-bind handle (^String host) (^Integer port)) (signal io/udp-bind (cons handle (cons host (cons port nil)))))
(define (udp-send handle (^String host) (^Integer port) data) (signal io/udp-send (cons handle (cons host (cons port (cons data nil))))))
(define (udp-recv handle) (signal io/udp-recv handle))
(define (udp-close handle) (signal io/udp-close handle))
(define (pipe-connect (^String path)) (signal io/pipe-connect path))
(define (pipe-listen (^String path)) (signal io/pipe-listen path))
(define (process-spawn (^String cmd) args env) (signal io/process-spawn (cons cmd (cons args (cons env nil)))))
(define (process-wait handle) (signal io/process-wait handle))
(define (process-kill handle (^Integer sig)) (signal io/process-kill (cons handle (cons sig nil))))
(define (signal-handle (^Integer sig) callback) (signal io/signal-handle (cons sig (cons callback nil))))
(define (signal-unhandle handle) (signal io/signal-unhandle handle))
(define (dns-resolve (^String host)) (signal io/dns-resolve host))
(define (async-sleep (^Integer ms)) (signal io/async-sleep ms))
(define (offload (^Symbol op) .. args) (signal io/offload (cons op args)))
;; Keep untyped fallback so invalid op payloads still flow through io/offload canonical payload errors.
(define (offload op .. args) (signal io/offload (cons op args)))
(define (thread-spawn (^Symbol op) .. args) (signal io/thread-spawn (cons op args)))
;; Keep untyped fallback so invalid op payloads still flow through io/thread-spawn canonical payload errors.
(define (thread-spawn op .. args) (signal io/thread-spawn (cons op args)))
(define (thread-join thread-handle) (signal io/thread-join thread-handle))
(define (thread-join-timeout thread-handle (^Integer timeout-ms)) (signal io/thread-join-timeout (cons thread-handle timeout-ms)))
(define (thread-cancel thread-handle) (signal io/thread-cancel thread-handle))
(define (task-spawn (^Symbol op) .. args) (signal io/task-spawn (cons op args)))
;; Keep untyped fallback so invalid op payloads still flow through io/task-spawn canonical payload errors.
(define (task-spawn op .. args) (signal io/task-spawn (cons op args)))
(define (task-join task-handle) (signal io/task-join task-handle))
(define (task-join-timeout task-handle (^Integer timeout-ms)) (signal io/task-join-timeout (cons task-handle timeout-ms)))
(define (task-cancel task-handle) (signal io/task-cancel task-handle))
(define (tls-connect tcp-handle (^String hostname) .. rest) (signal io/tls-connect (cons tcp-handle (cons hostname rest))))
;; Keep untyped fallback so invalid args still flow through io/tls-connect canonical payload errors.
(define (tls-connect tcp-handle hostname .. rest) (signal io/tls-connect (cons tcp-handle (cons hostname rest))))
(define (tls-server-wrap tcp-handle (^String cert-pem-path) (^String key-pem-path)) (signal io/tls-server-wrap (cons tcp-handle (cons cert-pem-path (cons key-pem-path nil)))))
;; Keep variadic fallback so optional max-bytes and invalid arg shapes flow through
;; io/tls-read canonical payload errors.
(define (tls-read handle .. rest) (signal io/tls-read (cons handle rest)))
(define (tls-write handle data) (signal io/tls-write (cons handle data)))
(define (tls-close handle) (signal io/tls-close handle))

;; Canonical descriptive protocol aliases; short protocol spellings remain
;;
(define transmission-control-connect tcp-connect)
(define transmission-control-listen tcp-listen)
(define transmission-control-accept tcp-accept)
(define transmission-control-read tcp-read)
(define transmission-control-write tcp-write)
(define transmission-control-close tcp-close)

(define user-datagram-socket udp-socket)
(define user-datagram-bind udp-bind)
(define user-datagram-send udp-send)
(define user-datagram-receive udp-recv)
(define user-datagram-close udp-close)

(define domain-name-resolve dns-resolve)

(define transport-layer-security-connect tls-connect)
(define transport-layer-security-server-wrap tls-server-wrap)
(define transport-layer-security-read tls-read)
(define transport-layer-security-write tls-write)
(define transport-layer-security-close tls-close)

(define [effect] (io/http-get (^String url)))
(define [effect] (io/http-request (^Any args)))
(define (http-get (^String url)) (signal io/http-get url))
(define (http-request (^String method) (^String url) .. rest) (signal io/http-request (cons method (cons url rest))))
;; Keep untyped fallback so invalid args still flow through io/http-request canonical payload errors.
(define (http-request method url .. rest) (signal io/http-request (cons method (cons url rest))))

;; =========================================================================
;; Thin dispatch convenience helpers (composition only, no runtime duplication)
;; =========================================================================
(define (job-spawn kind op .. args)
  (if (= kind 'task)
      (apply task-spawn (cons op args))
      (if (= kind 'thread)
          (apply thread-spawn (cons op args))
          (signal raise
                  { 'code 'io/job-spawn-kind-invalid
                    'domain 'io
                    'message "job-spawn: kind must be 'task or 'thread"
                    'data {'kind kind} }))))
(define (job-join ^(Value task) handle) (task-join handle))
(define (job-join ^(Value thread) handle) (thread-join handle))
(define (job-join-timeout ^(Value task) handle (^Integer timeout-ms)) (task-join-timeout handle timeout-ms))
(define (job-join-timeout ^(Value thread) handle (^Integer timeout-ms)) (thread-join-timeout handle timeout-ms))
(define (job-cancel ^(Value task) handle) (task-cancel handle))
(define (job-cancel ^(Value thread) handle) (thread-cancel handle))

(define (request (^String url)) (http-get url))
(define (request (^String method) (^String url) (^String body) (^String headers)) (http-request method url body headers))
(define (request method url body headers) (http-request method url body headers))

;; format helpers stay thin: dispatch only selects existing primitive wrappers.
(define (parse fmt src .. rest)
  (let (f fmt s src o (if (null? rest) nil (car rest)))
    (if (= f 'json)
        (if (null? rest) (json-parse s) (json-parse s o))
        (if (= f 'toml)
            (if (null? rest) (toml-parse s) (toml-parse s o))
            (if (= f 'csv)
                (if (null? rest) (csv-parse s) (csv-parse s o))
                (signal raise
                        { 'code 'data/parse-format-unsupported
                          'domain 'data
                          'message "parse: unsupported format (supported: 'json, 'toml, 'csv)"
                          'data {'format f} }))))))

(define (emit fmt value .. rest)
  (let (f fmt v value o (if (null? rest) nil (car rest)))
    (if (= f 'json)
        (if (null? rest) (json-emit v) (json-emit v o))
        (if (= f 'json-pretty)
            (if (null? rest) (json-emit-pretty v) (json-emit-pretty v o))
            (if (= f 'csv)
                (if (null? rest) (csv-emit v) (csv-emit v o))
                (signal raise
                        { 'code 'data/emit-format-unsupported
                          'domain 'data
                          'message "emit: unsupported format (supported: 'json, 'json-pretty, 'csv)"
                          'data {'format f} })))))))
