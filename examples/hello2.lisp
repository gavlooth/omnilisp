(define (make-adder x) (lambda (y) (+ x y)))
(define add5 (make-adder 5))
(println (add5 10))