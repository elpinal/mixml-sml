structure Lexer :> sig
  type t

  exception EOF

  val new : string -> t

  val peek : t -> char
  val peek_after : t -> int -> char

  val peek_option : t -> char option

  val proceed : t -> char -> unit
  val next : t -> char

  val pos : t -> position
end = struct
  type t =
    { src : CharVector.vector
    , position : position ref
    , offset : int ref
    }

  exception EOF

  fun new s =
  let
    val v = s
  in
    { src = v
    , position = ref Position.initial
    , offset = ref 0
    }
  end

  fun peek_after (l : t) n =
    CharVector.sub (#src l, n + !(#offset l))
    handle Subscript => raise EOF

  fun peek (l : t) = peek_after l 0

  fun peek_option l = SOME (peek l)
    handle EOF => NONE

  fun proceed (l : t) c =
    #offset l := !(#offset l) + 1
    before
    #position l := Position.next c (!(#position l))

  fun next (l : t) =
  let
    val c = peek l
  in
    proceed l c; c
  end

  fun pos (l : t) = !(#position l)
end

structure LexerError = struct
  exception IllegalChar of position * char
end

structure Lex : sig
  val lex : string -> Token.t loc Stream.stream
end = struct
  structure L = Lexer
  open LexerError

  fun ident l : string loc =
  let
    val start = L.pos l
    fun token t = Loc({from = start, to = L.pos l}, t)
    val cs = [L.next l]
    fun go cs =
      let val c = L.peek l handle L.EOF => #" " in
        if Char.isAlphaNum c orelse c = #"_"
        then go (c :: cs before L.proceed l c)
        else token (String.implode (rev cs))
      end
  in
    go cs
  end

  fun lower l =
  let
    val Loc(loc, s) = ident l
    val t =
      case s of
           "fun"    => Token.FUN
         | "in"     => Token.IN
         | "let"    => Token.LET
         | "seals"  => Token.SEALS
         | "link"   => Token.LINK
         | "with"   => Token.WITH
         | "val"    => Token.VAL
         | "type"   => Token.TYPE
         | "unit"   => Token.UNIT
         | "module" => Token.MODULE
         | "new"    => Token.NEW
         | "ext"    => Token.EXT
         | "bool"   => Token.BOOL
         | "int"    => Token.INT
         | "true"   => Token.TRUE
         | "false"  => Token.FALSE
         | "match"  => Token.MATCH
         | "data"   => Token.DATA
         | _        => Token.LOWER_IDENT s
  in
    Loc(loc, t)
  end

  fun upper l =
  let
    val Loc(loc, s) = ident l
    val t =
      case s of
           "Type" => Token.CAP_TYPE
         | _      => Token.UPPER_IDENT s
  in
    Loc(loc, t)
  end

  val ASCII_DIGIT_START = 48

  fun char_to_int c =
    Char.ord c - ASCII_DIGIT_START

  fun num l =
  let
    val start = L.pos l
    fun token t = Loc({from = start, to = L.pos l}, t)
    val n = char_to_int (L.next l)
    fun go n =
      let val c = L.peek l handle L.EOF => #" " in
        if Char.isDigit c
        then go (char_to_int c + n * 10 before L.proceed l c)
        else token (Token.NUMBER n)
      end
  in
    go n
  end

  fun skip_comment l =
  let
    val _ = L.next l
  in
    case L.peek l of
         #"\n" => L.proceed l #"\n"
       | _     => skip_comment l
  end handle L.EOF => ()

  (* Assume '-' is already consumed. *)
  fun hyphen token l =
    case L.peek_option l of
         SOME #">" => (L.proceed l #">"; token Token.RARROW)
       | _         => token Token.MINUS

  (* Assume '=' is already consumed. *)
  fun equal token l =
    case L.peek_option l of
         SOME #">" => (L.proceed l #">"; token Token.RDARROW)
       | _         => token Token.EQUAL

  (* Assume a single quote is already consumed. *)
  fun quote (token : Token.t -> Token.t loc) l =
  let
    val s : string = unloc (ident l)
  in
    token (Token.QUOTE_IDENT s)
  end

  fun lex1 l =
  let
    val start = L.pos l
    fun token t = Loc({from = start, to = L.pos l}, t)
    val c = L.peek l
  in
    case c of
         #" "  => (L.proceed l c; lex1 l)
       | #"\n" => (L.proceed l c; lex1 l)
       | #"\t" => (L.proceed l c; lex1 l)
       | #"\r" => (L.proceed l c; lex1 l)

       | #";" => (skip_comment l; lex1 l)

       | #"("  => (L.proceed l c; token Token.LPAREN)
       | #")"  => (L.proceed l c; token Token.RPAREN)
       | #"["  => (L.proceed l c; token Token.LBRACK)
       | #"]"  => (L.proceed l c; token Token.RBRACK)
       | #"{"  => (L.proceed l c; token Token.LBRACE)
       | #"}"  => (L.proceed l c; token Token.RBRACE)

       | #":"  => (L.proceed l c; token Token.COLON)
       | #"*"  => (L.proceed l c; token Token.STAR)
       | #"."  => (L.proceed l c; token Token.DOT)
       | #","  => (L.proceed l c; token Token.COMMA)
       | #"+"  => (L.proceed l c; token Token.PLUS)
       | #"_"  => (L.proceed l c; token Token.UNDERSCORE)
       | #"|"  => (L.proceed l c; token Token.BAR)
       | #"-"  => (L.proceed l c; hyphen token l)
       | #"="  => (L.proceed l c; equal token l)
       | #"'"  => (L.proceed l c; quote token l)
       | _     =>
           if Char.isLower c
           then lower l
           else if Char.isUpper c
           then upper l
           else if Char.isDigit c
           then num l
           else raise IllegalChar(start, c)
  end

  fun lex s =
  let
    val l = Lexer.new s
    fun go acc = go (lex1 l :: acc)
      handle L.EOF => acc
  in
    Stream.fromList (rev (go []))
  end
end
