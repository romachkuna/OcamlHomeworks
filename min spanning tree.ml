type graph = (int * float * int) list

(* THIS FUNCTION GIVES ME ALL SUCCESSOR NODES OF N *)
let successors n e = List.map (fun (x,_,v) -> if x=n then v else if v = n then x else 0)
                        (List.filter (fun (q,_,w) -> q=n || w=n) e)

(*THIS FUNCTION CHECKS IF THERE ARE DUPLICATES IN THE GRAPH, I USE THIS  IN BFS*)
let rec dupExist lst =
let rec exist elem lst =
  match lst with
  | [] -> false
  | hd::tl -> elem = hd || exist elem tl
  in
  match lst with
  | [] -> false
  | hd::tl -> (exist hd tl) || dupExist tl

(*BREADTH FIRST SEARCH ALGORITHM WHICH TELLS ME IF GRAPH IS A CYCLE
 IF IT IS - TRUE IF NOT FALSE*)
  let bfs graph = match graph with
  | [] -> false
  | graph -> let (m,b,k) = List.hd graph in
      let rec bfs_helper graph queue visited =
      let flattened =  List.flatten queue in
      let node = List.hd flattened in
      let root_s =  List.filter (fun a-> not (List.mem a visited)) (successors node graph) in
      let visit =  if (List.mem node visited) then visited else node::visited in
      let removed = List.filter (fun a -> node != a) flattened in

      if dupExist (List.flatten (removed::[root_s])) then true else if (List.length visit) = (List.length graph) then false else bfs_helper graph (removed::[root_s]) visit
      in bfs_helper graph [[m]] []


(*THIS FUNCTION GIVES ME SMALLEST NODES IN ASCENDING ORDER FOR KRUSKAL'S ALGORITHM *)
  let rec extract_sort =
    let my_function = function
     | [] -> raise Not_found
     | ((x,f,y) as k )::t -> List.fold_left
                 (fun ((a,b,c),list) (q,w,r) ->
                 if  b<= w
                 then ((a,b,c),(q,w,r)::list)
                 else ((q,w,r),(a,b,c)::list)

                 )
                 ((x,f,y), [])
                 t in
                 function
  | [] -> []
  | list ->
    let (least2, list) = my_function list
    in
      least2 :: extract_sort list;;


(*MINIMAL SPANNING TREE -  USING KRUSKAL'S ALGORITHM *)
(*THIS FUNCTION RETURNS NODES IN ANY ORDER IT COULD BE (4,5) OR (5,4)*)

 let mst graph =
            let smallest = extract_sort graph in
            let rec mst_helper lst acc = match lst with
            | [] -> acc
            | h::t -> if bfs (h::acc) then mst_helper t acc else mst_helper t (h::acc)
            in mst_helper smallest []
(*------------------------------------------------------------uni version------------------------------------------------------------------*)

type graph = (int * float * int) list

let mst = function 
  | [] -> []
  | ((n1,_,_)::_) as g -> 
  let rec main t visited edges =
    if edges = [] then t else
    let (v_to_uv, edges) = List.partition (fun (s,_,_) -> List.find_opt ((=) s) visited <> None) edges in
    let (uv_to_v, edges) = List.partition (fun (_,_,s) -> List.find_opt ((=) s) visited <> None) edges in
    let border_edges = v_to_uv @ (List.map (fun (s,w,d) -> d,w,s) uv_to_v) in
    let min_e = List.fold_left (fun (ms,mw,md) (s,w,d) -> if w < mw then s,w,d else ms,mw,md) (0,infinity,0) border_edges in
    let _,_,min_d = min_e in
    let edges = edges @ List.filter (fun (_,_,d) -> d <> min_d) border_edges in
    main (min_e::t) (min_d::visited) edges
  in
  main [] [n1] (List.filter (fun (s,_,d) -> s <> d) g)


