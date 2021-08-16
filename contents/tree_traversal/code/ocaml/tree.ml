(* submitted by Nicole Mazzuca (ubsan) *)
module Tree : sig
  type t

  val leaf : int -> t

  val with_child : t -> t -> t

  val dfs_recursive : t -> unit

  val dfs_stack : t -> unit

  val bfs_queue : t -> unit
end = struct
  type t = {children: t list; value: int}

  let leaf value = {children= []; value}

  let with_child {children; value} child = {children= child :: children; value}

  (* recursive is by far the easiest to do in functional langs *)
  let rec dfs_recursive self =
    Printf.printf "%d\n" self.value ;
    List.iter dfs_recursive self.children


  let dfs_stack self =
    let rec helper = function
      | [] -> ()
      | x :: xs ->
          Printf.printf "%d\n" x.value ;
          helper (x.children @ xs)
    in
    helper [self]


  let bfs_queue self =
    let rec push_all queue = function
      | x :: xs -> Queue.add x queue ; push_all queue xs
      | [] -> ()
    in
    let queue = Queue.create () in
    Queue.add self queue ;
    while not (Queue.is_empty queue) do
      let x = Queue.take queue in
      Printf.printf "%d\n" x.value ;
      push_all queue x.children
    done
end

let rec build_tree num_row num_child =
  let tree = Tree.leaf num_row in
  match num_row with
  | 0 -> tree
  | n ->
      let child = build_tree (num_row - 1) num_child in
      let rec helper child tree = function
        | 0 -> tree
        | n -> Tree.with_child (helper child tree (n - 1)) child
      in
      helper child tree num_child


let main () =
  let tree = build_tree 3 3 in
  print_endline "--- dfs_recursive ---" ;
  Tree.dfs_recursive tree ;
  print_endline "--- dfs_stack ---" ;
  Tree.dfs_stack tree ;
  print_endline "--- bfs_queue ---" ;
  Tree.bfs_queue tree


let () = main ()
