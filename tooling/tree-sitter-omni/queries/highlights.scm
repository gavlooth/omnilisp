(comment) @comment
(form_comment_marker) @comment

(string) @string
(regex_literal) @string.regexp

(integer) @number
(float) @number.float

(placeholder) @variable.special

(path
  root: (symbol) @variable)

(path_segment
  name: (symbol) @property)

(list
  .
  (symbol) @keyword
  (#match? @keyword "^(lambda|λ|if|let|define|quote|block|match|checkpoint|capture|signal|handle|resolve|with-continuation|module|import|export|export-from|syntax-match|template|insert|splice|and|or|quasiquote|unquote|unquote-splicing|explain)$"))

(array
  (symbol) @attribute
  (#match? @attribute "^(macro|type|struct|abstract|union|alias|effect|relation|schema)$"))

(list
  .
  (symbol) @_head
  (#eq? @_head "define")
  .
  (symbol) @variable)

(list
  .
  (symbol) @_head
  (#eq? @_head "define")
  .
  (list
    .
    (symbol) @function))

(list
  .
  (symbol) @_head
  (#match? @_head "^(lambda|λ)$")
  .
  (list
    .
    (symbol) @variable.parameter))

(quote
  (symbol) @constant)

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  "'"
  "`"
  ","
  ",@"
  "."
  ".["
] @punctuation.special
