(define qq-x 42)
(print `(a ,qq-x c))
(print `,(+ 1 2))
(define qq-lst (list 1 2 3))
(print `(a ,@qq-lst d))
(define qq-empty (list))
(print `(a ,@qq-empty b))

(define [macro] when ([test .. body] (if test (begin .. body) nil)))
(print (when true 42))

(define [macro] unless ([test .. body] (if test nil (begin .. body))))
(print (unless false 42))

(print "DONE")