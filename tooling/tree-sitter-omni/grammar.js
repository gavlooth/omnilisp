const PREC = {
  PREFIX: 5,
  INDEX: 10,
  PATH: 20,
};

module.exports = grammar({
  name: "omni",

  extras: ($) => [
    /\s+/,
    $.comment,
  ],

  word: ($) => $.symbol,

  supertypes: ($) => [
    $._form,
  ],

  conflicts: ($) => [
    [$._form, $._index_target],
  ],

  rules: {
    source_file: ($) => repeat($._form),

    _form: ($) =>
      choice(
        $.form_comment,
        $.index_expression,
        $.quote,
        $.quasiquote,
        $.unquote_splicing,
        $.unquote,
        $.accessor,
        $.path,
        $.list,
        $.array,
        $.dict,
        $.regex_literal,
        $.string,
        $.float,
        $.integer,
        $.placeholder,
        $.symbol,
      ),

    comment: () => token(seq(";", /.*/)),

    form_comment: ($) =>
      prec.right(seq(
        field("marker", $.form_comment_marker),
        field("body", $._form),
      )),

    form_comment_marker: () => token(/#[1-9]?_/),

    quote: ($) =>
      prec.right(PREC.PREFIX, seq(
        "'",
        field("body", $._form),
      )),

    quasiquote: ($) =>
      prec.right(PREC.PREFIX, seq(
        "`",
        field("body", $._form),
      )),

    unquote: ($) =>
      prec.right(PREC.PREFIX, seq(
        ",",
        field("body", $._form),
      )),

    unquote_splicing: ($) =>
      prec.right(PREC.PREFIX, seq(
        ",@",
        field("body", $._form),
      )),

    list: ($) =>
      seq(
        "(",
        repeat($._form),
        ")",
      ),

    array: ($) =>
      seq(
        "[",
        repeat($._form),
        "]",
      ),

    dict: ($) =>
      seq(
        "{",
        repeat($._form),
        "}",
      ),

    accessor: ($) =>
      prec.right(PREC.PREFIX, seq(
        ".",
        field("key", $._form),
      )),

    index_expression: ($) =>
      prec.left(PREC.INDEX, seq(
        field("target", $._index_target),
        repeat1($.index_clause),
      )),

    index_clause: ($) =>
      seq(
        ".[",
        field("index", $._form),
        "]",
      ),

    _index_target: ($) =>
      choice(
        $.path,
        $.accessor,
        $.list,
        $.array,
        $.dict,
        $.quote,
        $.quasiquote,
        $.unquote,
        $.unquote_splicing,
        $.regex_literal,
        $.string,
        $.float,
        $.integer,
        $.placeholder,
        $.symbol,
      ),

    path: ($) =>
      prec.left(PREC.PATH, seq(
        field("root", $.symbol),
        repeat1($.path_segment),
      )),

    path_segment: ($) =>
      seq(
        ".",
        field("name", $.symbol),
      ),

    placeholder: () => "_",

    integer: () => token(prec(2, /-?[0-9]+/)),

    float: () => token(prec(3, /-?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?/)),

    string: () =>
      token(seq(
        '"',
        repeat(choice(
          /[^"\\\n]+/,
          /\\./,
        )),
        '"',
      )),

    regex_literal: () =>
      token(seq(
        '#r"',
        repeat(choice(
          /[^"\\\n]+/,
          /\\./,
        )),
        '"',
      )),

    symbol: () =>
      token(prec(1, /[0-9A-Za-z_+*\/=<>!?:@#$%&|^~\-\u0080-\uFFFF]+/u)),
  },
});
