; Generic structural forms for editor selections.
[
  (list)
  (array)
  (dict)
  (quote)
  (quasiquote)
  (unquote)
  (unquote_splicing)
  (accessor)
  (index_expression)
  (path)
] @form.outer

(comment) @comment.outer
(form_comment body: (_) @comment.inner) @comment.outer

; Any list with a symbolic head is a call-shaped form in Omni's surface syntax.
((list
   .
   (symbol) @_head) @call.outer)

; Top-level block-style forms are useful motion targets even before semantic indexing.
((list
   .
   (symbol) @_head
   (#match? @_head "^(if|let|block|match|handle|module)$")) @block.outer)

; Plain value bindings.
((list
   .
   (symbol) @_head
   (#eq? @_head "define")
   .
   (symbol) @assignment.inner) @assignment.outer)

; Function shorthand declarations and lambdas.
((list
   .
   (symbol) @_head
   (#eq? @_head "define")
   .
   (list
     .
     (symbol) @function.inner)) @function.outer)

((list
   .
   (symbol) @_head
   (#match? @_head "^(lambda|λ)$")) @function.outer)

; Module and type/effect declarations provide class-like structural regions.
((list
   .
   (symbol) @_head
   (#eq? @_head "module")
   .
   (symbol) @class.inner) @class.outer)

((list
   .
   (symbol) @_head
   (#eq? @_head "define")
   .
   (array
     (symbol) @_attr
     (#match? @_attr "^(type|struct|abstract|union|alias|effect|relation|schema)$"))
   .
   (symbol) @class.inner) @class.outer)
