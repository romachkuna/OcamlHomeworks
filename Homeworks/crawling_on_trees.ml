type tree = Empty 
          | Node of int * tree * tree
type command = Left | Right | Up | New of int | Delete | Push | Pop

(* print a graphical representation (dot) of a binary tree (2. argument) to a file (1. argument) *)
let print_tree filename btree = 
  let file = open_out filename in
  Printf.fprintf file "digraph Tree {\n";
  let rec print next_id = function Empty -> 
    Printf.fprintf file "\tn%d[shape=rectangle,label=\"\"];\n" next_id; next_id + 1, next_id
  | Node (x, l, r) ->
    let node_id = next_id in
    Printf.fprintf file "\tn%d[label=\"%d\"];\n" node_id x;
    let next_id, lid = print (next_id + 1) l in
    let next_id, rid = print next_id r in 
    (Printf.fprintf file "\tn%d -> n%d[label=\"L\"];\n" node_id lid);
    (Printf.fprintf file "\tn%d -> n%d[label=\"R\"];\n" node_id rid);
    next_id, node_id
  in
  ignore(print 0 btree);
  Printf.fprintf file "}";
  close_out file

(*gives root of tree as int *)
let  root_finder tree  = match tree with
| Node(v,l,r) -> v
| Empty -> 0

(*move_up tree pointer -> tree
    this function takes pointer moves it to the root*)
let  move_up tree pointer =
            let rec helper_up tree pointer = match tree with
            | Node(v,l,r)  as k-> if pointer = l || pointer = r  then k else
                                        match pointer,tree with
                                        | Node(v,_,_),Node(root,_,_) -> if root>v then helper_up l pointer
                                                        else helper_up r pointer
            in helper_up tree pointer

(*point_finder tree node -> tree
  this function tels me which node im at when i replace a node *)
let rec pointer_finder tree x = match tree with
| Node(v,_,_) as k -> if x = v then k else
                        match tree with
                        | Node(v2,l2,r2) -> if x>v2  then pointer_finder r2 x else  pointer_finder l2 x
(*deletes node x from tree*)
let rec delete_node tree x = match tree with
| Empty -> Empty
| Node(v,l,r) -> if x = v then Empty else Node(v,delete_node l x,delete_node r x)


(*this function moves pointer to left or right*)
let move cmd tree = match tree,cmd with
| Empty,_ -> Empty
| Node(v,l,r),"left" -> l
| Node(v,l,r),"right" -> r

(*replaces node x for y*)
let rec replace_node tree x y = match tree with
| Empty -> Empty
| Node(v,l,r) -> if x = v then Node(y,Empty,Empty) else Node(v,replace_node l x y,replace_node r x y)

(*the inside function impl takes commands,stack,tree,and pointer which tells the program CURRENT node*)
let crawl cmds t =
  let rec impl cmds stack t pointer =
    match cmds, stack, t, pointer with
    | Up::xs,s,t,p -> impl xs s t (move_up t p)
    | Right::xs,s,t,p -> impl xs s t (move "right" p)
    | Left::xs,s,t,p -> impl xs s t (move "left" p)
    | Delete::xs, s, t, (Node(v,_,_) as k) ->
        impl xs s (delete_node t v) (pointer_finder (delete_node t v) (root_finder(move_up t k)))
    | Pop::cs, s::ss, t ,p -> impl cs ss t p
    | Push::cs,s ,t, p -> impl cs (p::s) t p
    | (New y)::cs,s,t,Node(v,_,_) -> impl cs s (replace_node t v y ) (pointer_finder (replace_node t v y) y)
    | [],s::ss,_,_ -> s
    | [],[],t,_ -> t

  in impl cmds [] t t

  let t_l = Node (2, Node (1, Empty, Empty), Node (3, Empty, Empty))
  let t_r = Node (6, Node (5, Empty, Empty), Node (7, Empty, Empty))
  let tree = Node (4, t_l , t_r)


  let big_tree = Node(18,
                          Node(9,Node(7,Node(6,Empty,Empty),Node(8,Empty,Empty)),Node(16,Node(10,Empty,Empty),Node(17,Empty,Empty)))
                          ,
                          Node(30,Node(20,Node(19,Empty,Empty),Node(25,Empty,Empty)),Node(40,Node(35,Empty,Empty),Node(45,Empty,Empty)))
                                  )
