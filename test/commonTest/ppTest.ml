(* Pretty Printing *)

open Format

open OUnit2

let printer pp x =
  x
    |> pp
    |> fprintf str_formatter "%t"
    |> flush_str_formatter

let assert_pp ~ctxt pp lines x =
  let expected = String.concat "\n" lines in
  let print = printer pp in
  x
    |> print
    |> assert_equal ~ctxt ~printer:Fun.id ~msg:"Pretty-printing output does not match" expected
