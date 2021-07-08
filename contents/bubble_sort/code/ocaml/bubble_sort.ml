(* References -> https://codereview.stackexchange.com/questions/125571/recursive-bubble-sort-in-ocaml *)
(* Bubble Sort in OCaml -> Using List and Array *)

let print_lst lst =
    Printf.printf "[ ";
    List.iter (fun x -> Printf.printf "%d " x) lst;
    Printf.printf "]\n"

let rec sort_lst lst =
    let sorted = match lst with
        | hd1 :: hd2 :: tl -> 
            if hd1 > hd2 then 
                hd2 :: sort_lst (hd1 :: tl) 
            else 
                hd1 :: sort_lst (hd2 :: tl)
        | hd1 :: tl ->
            hd1 :: sort_lst tl
        | tl -> tl in
    if lst = sorted then lst else sort_lst sorted

let print_arr arr =
    Printf.printf "[ ";
    Array.iter (fun x -> Printf.printf "%d " x) arr;
    Printf.printf "]\n"

let rec sort_arr arr =
    for i=0 to pred @@ Array.length arr do
        for j=i+1 to pred @@ Array.length arr do
            if arr.(i) > arr.(j) then begin
                let temp = arr.(i) in
                arr.(i) <- arr.(j);
                arr.(j) <- temp
            end else 
                    ()
            done
        done

let _ =
    let lst = List.init 10 (fun _ -> Random.int 10000) in
    Printf.printf "Pre-sorted List\n";
    print_lst lst;
    Printf.printf "Post-sorted List\n";
    sort_lst lst |> print_lst;
    let arr = Array.init 10 (fun _ -> Random.int 10000) in
    Printf.printf "Pre-sorted Array\n";
    print_arr arr;
    Printf.printf "Post-sorted Array\n";
    sort_arr arr |> fun _ ->
    print_arr arr