(handle
  (block
    (println "Before signal")
    (println (+ 1 (signal ask 42)))
    (println "After signal"))
  (ask val
    (block
      (println "In handler")
      (resolve (+ val 100)))))