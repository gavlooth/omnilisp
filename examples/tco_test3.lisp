;; Named let with multi-arg TCO
(define (sum-to n)
  (let loop (i n acc 0)
    (if (= i 0) acc (loop (- i 1) (+ acc i)))))

(println (sum-to 1000000))
