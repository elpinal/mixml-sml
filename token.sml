structure Token = struct
  datatype t
    = LOWER_IDENT of string
    | UPPER_IDENT of string
    | QUOTE_IDENT of string
    | NUMBER of int

    | CAP_TYPE
    | FUN
    | IN
    | LET
    | SEALS
    | LINK
    | WITH
    | VAL
    | TYPE
    | UNIT
    | MODULE
    | NEW
    | EXT
    | BOOL
    | INT
    | TRUE
    | FALSE
    | MATCH
    | DATA

    | LPAREN
    | RPAREN
    | LBRACK
    | RBRACK
    | LBRACE
    | RBRACE

    | COLON
    | RARROW
    | RDARROW
    | STAR
    | EQUAL
    | DOT
    | COMMA
    | MINUS
    | PLUS
    | UNDERSCORE
    | BAR

  val show = fn
      LOWER_IDENT s => s
    | UPPER_IDENT s => s
    | QUOTE_IDENT s => "'" ^ s
    | NUMBER n      => Int.toString n

    | CAP_TYPE => "Type"
    | FUN      => "fun"
    | IN       => "in"
    | LET      => "let"
    | SEALS    => "seals"
    | LINK     => "link"
    | WITH     => "with"
    | VAL      => "val"
    | TYPE     => "type"
    | UNIT     => "unit"
    | MODULE   => "module"
    | NEW      => "new"
    | EXT      => "ext"
    | BOOL     => "bool"
    | INT      => "int"
    | TRUE     => "true"
    | FALSE    => "false"
    | MATCH    => "match"
    | DATA     => "data"

    | LPAREN => "("
    | RPAREN => ")"
    | LBRACK => "["
    | RBRACK => "]"
    | LBRACE => "{"
    | RBRACE => "}"

    | COLON      => ":"
    | RARROW     => "->"
    | RDARROW    => "=>"
    | STAR       => "*"
    | EQUAL      => "="
    | DOT        => "."
    | COMMA      => ","
    | MINUS      => "-"
    | PLUS       => "+"
    | UNDERSCORE => "_"
    | BAR        => "|"
end
