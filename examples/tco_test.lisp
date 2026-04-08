(define (loop-test n)
  (if (= n 0) 0 (loop-test (- n 1))))

(println (loop-test 100000))
