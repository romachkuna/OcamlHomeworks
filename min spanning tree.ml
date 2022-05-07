(*node = added node | gives me back new node value *)
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
