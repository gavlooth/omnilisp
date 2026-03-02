(define (range-to n acc)
  (if (<= n 0)
      acc
      (range-to (- n 1) (cons n acc))))

(print (length (range-to 50000 '())))