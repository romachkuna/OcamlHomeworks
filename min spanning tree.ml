type graph = (int * float * int) list

let remove xs = let uniq_cons x xs = if List.mem x xs then xs else x :: xs in List.fold_right uniq_cons xs []

let successors n e = List.map (fun (x,_,v) -> if x=n then v else if v = n then x else 0)
                        (List.filter (fun (q,_,w) -> q=n || w=n) e)

let rec dupExist lst =
let rec exist elem lst =
  match lst with
  | [] -> false
  | hd::tl -> elem = hd || exist elem tl
  in
  match lst with
  | [] -> false
  | hd::tl -> (exist hd tl) || dupExist tl

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

let map = [(1,1.7,5);(1,1.2,6);(1,1.3,2);(5,1.4,4);(4,1.5,7);(2,1.1,3);(3,1.7,9);(9,1.8,10);(4,1.9,6)]

 let mst graph =
            let smallest = extract_sort graph in
            let rec mst_helper lst acc = match lst with
            | [] -> acc
            | h::t -> if bfs (h::acc) then mst_helper t acc else mst_helper t (h::acc)
            in mst_helper smallest []









