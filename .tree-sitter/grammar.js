module.exports = grammar({
  name: 'rutile',

  extras: $ => [
    /\s/,
    $.comment,
    $.block_comment
  ],

  rules: {
    source_file: $ => repeat($._item),

    _item: $ => choice(
      $.word_definition,
      $.quotation,
      $.map,
      $.string,
      $.number,
      $.word,
      $.webhook_sigil
    ),

    word_definition: $ => seq(
      ':',
      field('name', $.word),
      repeat($._item),
      ';'
    ),

    quotation: $ => seq(
      '[',
      repeat($._item),
      ']'
    ),

    map: $ => seq(
      '{',
      repeat($._item),
      '}'
    ),

    string: $ => seq(
      '"',
      repeat(choice(
        /[^"\\]/,
        /\\./
      )),
      '"'
    ),

    number: $ => token(/-?\d+(\.\d+)?/),

    word: $ => token(/[a-zA-Z_][a-zA-Z0-9_?!-]*/),

    webhook_sigil: $ => token(/@[a-zA-Z_][a-zA-Z0-9_.]*/),

    comment: $ => token(seq('#', /.*/)),

    block_comment: $ => seq(
      '(',
      repeat(choice(
        /[^()]/,
        $.block_comment
      )),
      ')'
    )
  }
});