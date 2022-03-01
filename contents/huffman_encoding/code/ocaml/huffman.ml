
(* Takes in a string and a character that needs to be removed from
  the string that is input
  For eg. in a string : "Chennai" I want to remove char : 'n'
  such that we get the old value and the new value as a tuple

  TL;DR : "Chennai" -> ("Cheai", "Chennai")
*)
let char_diff str ch = 
  let res = String.concat "" (String.split_on_char ch str) in
  (res, str)

(* Converts "ABCD" -> ['A'; 'B'; 'C'; 'D'] *)
let str_to_charlist s = List.init (String.length s) (String.get s)

(* Takes a word like "bibbity_bob" and converts to a tuple list of
   unique characters with their frequency

   TL;DR : 
    "bibbity_bobbity" -> 
    [('b', 6); ('i', 3); ('t', 2); ('y', 2); ('_', 1); ('o', 1)]
*)
let counter str =
  let char_lst = str_to_charlist str in
  let rec loop acc str char_lst =
    match char_lst with
    | [] -> List.filter (fun (_,y) -> y != 0) (List.rev acc)
          |> List.map (fun (x, y) -> (Printf.sprintf "%c" x, y))
    | hd :: tl -> 
      let (new_str, old_str) = char_diff str hd in
      loop 
        ((hd, (String.length old_str - String.length new_str)) :: acc) 
        new_str tl in
  loop [] str char_lst

(* References -> https://ocaml.org/learn/tutorials/99problems.html *)

module Pq = struct
  type 'a t = {
    data: 'a list array;
    mutable first: int;
  }

  let make size = {
    data = Array.make size [];
    first = size;
  }

  let add q p x =
    q.data.(p) <- x :: q.data.(p);
    q.first <- min p q.first
  
  let get_min q =
    if q.first = Array.length (q.data) then None
    else
      match q.data.(q.first) with
      | [] -> assert false
      | hd :: tl ->
        let p = q.first in
        q.data.(q.first) <- tl;
        while q.first < (Array.length (q.data)) && q.data.(q.first) = [] do
          q.first <- q.first + 1
        done;
        Some(p,hd)
end

type tree = Leaf of string | Node of tree * tree

let rec create_huffman_tree q =
  match Pq.get_min q, Pq.get_min q with
  | Some(p1, t1), Some(p2, t2) ->
      Pq.add q (p1 + p2) (Node(t1, t2));
      create_huffman_tree q
  | Some(_, t), None | None, Some(_, t) -> t
  | None, None                          -> assert false

let rec prefixes_of_tree prefix trees = match trees with
  | Leaf s -> [(s, prefix)]
  | Node (t0, t1) -> 
    List.append (prefixes_of_tree (prefix ^ "0") t0) (prefixes_of_tree (prefix ^ "1") t1)

let huffman huffman_tree = prefixes_of_tree "" huffman_tree

(* Helper functions *)
let char_to_str = Printf.sprintf "%c"

let str_list msg = 
  List.map char_to_str (str_to_charlist msg)

let list_to_string lst =
  String.concat "" lst

(* Encoding and decoding functions *)
let encode codebook x =
  List.filter (fun (ch, _) -> ch = x) codebook |> fun x ->
  List.hd x |> snd

let encode_msg codebook msg =
  List.map (fun x -> encode codebook x) (str_list msg) |>
   list_to_string (List.map (fun x -> encode codebook x) (str_list msg))

let decode codebook key =
  List.find_opt (fun (_,code) -> key = code) codebook
  

let decode_msg codebook msg =
  let decoded_message = ref "" in
  let code = ref "" in
  let msg_list = str_list msg in
  List.iter (fun bit ->
    code := !code ^ bit;
    match (decode codebook !code) with
    | None -> ()
    | Some v -> 
      decoded_message := !decoded_message ^ (fst v);
      code := "";
    ) msg_list;
  !decoded_message

(* Printing functions below *)
let print_codebook codebook =
  let _ = Printf.printf "[\n" in
  let fmt_tup hd = Printf.sprintf "\t(%s, %s)" (fst hd) (snd hd) in
  let rec loop codebook = match codebook with
    | [] -> ()
    | hd :: [] -> 
      let tup = fmt_tup hd in
      Printf.printf "%s\n]\n" tup
    | hd :: tl ->
      let tup = fmt_tup hd in
      Printf.printf "%s,\n" tup;
      loop tl in
  loop codebook

let rec print_huffman_tree huffman_tree =
  match huffman_tree with
  | Leaf a -> Printf.sprintf "%s" a
  | Node (l, r) -> 
      let fmt_l = print_huffman_tree l in
      let fmt_r = print_huffman_tree r in
      Printf.sprintf "[%s,%s]" fmt_l fmt_r


(* Main Function *)
let _ =
  let message = "bibbity_bobbity" in
  let freq_ch_list = counter message in
  let size = List.fold_left (fun sum (_,p) -> sum + p) 0 freq_ch_list in
  let queue = Pq.make (size + 2) in
  let _ = List.iter (fun (s,f) -> Pq.add queue f (Leaf s)) freq_ch_list in
  let huffman_tree = create_huffman_tree queue in 
  let codebook = huffman huffman_tree in 
  let encoded_message = encode_msg codebook message in
  let decoded_message = decode_msg codebook encoded_message in
  let _ = Printf.printf "Message : %s\n" message in
  let _ = print_huffman_tree huffman_tree |> fun x -> 
    Printf.printf "Huffman Tree : %s\n" x in
  let _ = Printf.printf "Codebook : ";print_codebook codebook in
  let _ = Printf.printf "Encoded Message : %s\n" encoded_message in
  let _ = Printf.printf "Decoded Message : %s\n" decoded_message in
  ()