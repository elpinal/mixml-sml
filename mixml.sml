structure MixML : sig
  val fail : string -> 'a

  val parse_file : string -> Syntax.module
  val elaborate : Syntax.module -> Statics.modsig
end = struct
  open Pretty
  open Std

  fun fail s =
    ( TextIO.output (TextIO.stdErr, s ^ "\n")
    ; OS.Process.exit OS.Process.failure
    )

  fun parse s =
  let
    val ts = Lex.lex s
    val (m, _) = Parser.parse ts
  in
    m
  end handle
      ParserError.UnexpectedEOF => fail "unexpected end of file"
    | ParserError.UnexpectedToken(l, t) => fail (Position.show (#from l) ^ ": unexpected token: " ^ Token.show t)
    | LexerError.IllegalChar(pos, c) => fail (Position.show pos ^ ": illegal character: " ^ str c)

  fun parse_file name =
    ( TextIO.openIn name
      handle IO.Io r => fail ("cannot open: " ^ #name r)
    )
    |> TextIO.inputAll
    |> parse

  fun elaborate m =
    Statics.elaborate_program Statics.Env.initial m
    handle
        Statics.MissingLabel l => fail ("missing label: " ^ Label.show l)
      | Statics.TypeMismatch(x, y) =>
          fail ("type mismatch: " ^ Statics.Show.show_type 0 x <+> "vs" <+> Statics.Show.show_type 0 y)
      | Statics.VarMismatch(x, y) =>
          fail ("type varable mismatch: " ^ FVar.show x <+> "vs" <+> FVar.show y)
      | Statics.TypeSelfCycle p =>
          fail ("transparent type cycle: self recursion at" <+> Path.show p)
      | Statics.TypeCycle(p1, p2) =>
          fail ("transparent type cycle:" <+> Path.show p1 <+> "depends on" <+> Path.show p2)
      | Statics.EscapingLocalAbstractType fv =>
          fail ("escaping local abstract type:" <+> FVar.show fv)
      | Statics.Env.Unbound v => fail ("unbound module variable:" <+> ModVar.show v)
      | Statics.Env.Val.Unbound v => fail ("unbound value variable:" <+> v)
end
