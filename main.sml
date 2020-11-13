fun main () =
let
  open Std

  val args = CommandLine.arguments ()
in
  case args of
       [] => MixML.fail "missing argument"
     | a :: args =>
         print ((Statics.Show.show_modsig $ MixML.elaborate $ MixML.parse_file a) ^ "\n")
end

val () = main ()
