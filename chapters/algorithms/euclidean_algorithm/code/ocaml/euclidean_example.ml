(* contributed by Nicole Mazzuca (ubsan) *)

let euclid_mod a b =
  let rec inner a = function
  | 0 -> a
  | b -> inner b (a mod b)
  in (inner (abs a) (abs b))

let euclid_sub a b =
  let rec inner a b =
    if a = b then
      a
    else if a < b then
      inner a (b - a)
    else
      inner (a - b) b
  in (inner (abs a) (abs b))

let chk1 = euclid_mod (64 * 67) (64 * 81)
let chk2 = euclid_sub (128 * 12) (128 * 77)
let () =
  chk1 |> print_int |> print_newline;
  chk2 |> print_int |> print_newline

