((list
   .
   (symbol) @_head) @local.scope
 (#match? @_head "^(lambda|λ|let|match|handle|module)$"))

((list
   .
   (symbol) @_head
   .
   (symbol) @local.definition)
 (#eq? @_head "define"))

((list
   .
   (symbol) @_head
   .
   (list
     .
     (symbol) @local.definition))
 (#eq? @_head "define"))

((list
   .
   (symbol) @_head
   .
   (list
     .
     (symbol) @local.definition))
 (#match? @_head "^(lambda|λ)$"))

(symbol) @local.reference
