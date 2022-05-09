(*node = added node | gives me back new node value *)
(*let bfs graph = let (m,b,k) = List.hd graph in*)
(*    let rec bfs_helper graph queue visited  =*)
(*    let flattened = List.flatten queue in*)
(*    let node = List.hd flattened in*)
(*    let root_s = List.filter (fun a-> List.mem a visited) (successors node graph) in*)
(*    let visit =  if (List.mem node flattened) then visited else node::visited in*)
(*    let removed = List.filter (fun a -> node != a) flattened in*)

(*    if dupExist removed then true else if List.length visit = List.length graph then false else bfs_helper graph (removed::[root_s]) visit*)
(*    in bfs_helper graph [[m]] []*)


let rec newnode graph node = match graph,node with
| [],_ -> 0
| (x,_,y)::t,(x2,_,y2) -> if x2 = x || x2=y then x2 else if y2=y || y2=x then y2 else newnode t node


let neighbr graph x =
let rec neighbr_helper graph x acc = match graph with
| [] -> acc
| (q,_,r)::t -> if q=x  then neighbr_helper t x (r::acc) else if r=x  then neighbr_helper t x (q::acc) else neighbr_helper t x acc

in neighbr_helper graph x []

let  pathfinder graph x =
        let rec helper graph x acc = match graph with
| [] -> acc
| (q,_,r)::t -> if q = x then helper t x (r::acc) else if r = x then helper t x (q::acc) else helper t x acc
in helper graph x []

let old graph node = match node with
| (x,_,y) -> if (newnode graph node) = x then y else x

let rec clear lst x = match lst with
| [] -> (-1)
| h::t -> if h != x then h else clear t x

let allnodes graph lst =
            let rec helper lst acc = match lst with
| [] -> List.flatten acc
| h::t -> helper t ((pathfinder graph h)::acc)
in helper lst []

let rec cycle graph node = match graph,node with
| [],_ -> false
| (x,_,z)::t,(x2,_,y2) -> let newx = newnode graph node in
                       let adjacent = clear (neighbr graph newx) newx in
                       let paths = List.flatten ((pathfinder graph adjacent)::(allnodes graph (pathfinder graph adjacent)::[])) in
                       let oldnode = old graph node in
                       if List.mem oldnode paths then true else cycle t node


(*let map = [(1,1.7,5);(1,1.2,6);(1,1.3,2);(5,1.4,4);(4,1.5,7);(2,1.6,3);(3,1.7,9);(9,1.8,10);(4,1.9,6)]*)




let rec smallest nodes = match nodes with
                | (x,f,y)::[] -> (x,f,y)
                | (x,f,y)::t -> let (m,b,k) = smallest t in if f<b then (x,f,y) else (m,b,k)

let mst graph =
        let rec mst_helper graph acc = match graph with
        | [] -> acc
        | (x,f,y)::t  -> let shortest = smallest graph in

                     if cycle acc shortest then mst_helper t (shortest::acc) else mst_helper t acc

        in mst_helper graph []
(*-----------------------------------------------------------------------CHANGES---------------------------------------------------------*)

type graph = (int * float * int) list

let make_list graph = let rec helper graph acc =match graph with
| [] -> acc
| (x,_,y)::t -> helper t ((x,y)::acc)
in helper graph []

let successors n e =
  List.map (fun (_, v) -> v) (List.filter (fun (u, _) -> n = u) e)

let dfs graph start f =
  let rec rdfs visited node =
    if not (List.mem node visited) then
      begin
        f node;
        let s = successors node graph in
        List.fold_left rdfs (node::visited) s
      end
    else node::visited
  in rdfs [] start

let smallest lst = List.fold_left (fun (a1,f1,b1) (a2,f2,b2) ->  if f1 < f2 then (a1,f1,b1) else (a2,f2,b2)) (List.hd lst) lst

let rec dupExist lst =
let rec exist elem lst =
  match lst with
  | [] -> false
  | hd::tl -> elem = hd || exist elem tl
  in
  match lst with
  | [] -> false
  | hd::tl -> (exist hd tl) || dupExist tl


let mst graph = let j a = a::[] in
                let rec mst_helper graph2 acc = match graph2 with
                | [] -> acc
                | (x,_,_)::t -> let (m,b,k) = smallest graph2 in
                          let store = (m,b,k)::acc in
                          let intolist = dfs (make_list store) m j in (* PROBLEM WITH INTERATION SMALLEST FUNCTION*)
                          if dupExist intolist then mst_helper t acc else mst_helper t (store)
                in mst_helper graph []

let map = [(1,1.7,5);(1,1.2,6);(1,1.3,2);(5,1.4,4);(4,1.5,7);(2,1.1,3);(3,1.7,9);(9,1.8,10);(4,1.9,6)]

let map2 = [(4, 1.9, 6); (9, 1.8, 10); (3, 1.7, 9); (2, 1.1, 3)]


(* ========================================================================================================================================*)

let my_function = function
| [] -> raise Not_found
| ((x,f,y) as k )::t -> List.fold_left
            (fun ((a,b,c),list) (q,w,r) ->
            if  b<= w
            then ((a,b,c),(q,w,r)::list)
            else ((q,w,r),(a,b,c)::list)

            )
            ((x,f,y), [])
            t

let rec extract_sort = function
| [] -> []
| list ->
  let (least2, list) = my_function list
  in
    least2 :: extract_sort list;;
