type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree)
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree


let layer_tree r =
            let rec impl r n = LNode(
            (r+n), (fun () -> impl r (n+1)) , (fun () -> impl r (n+1))
            )
            in impl r 0

let rec top n (LNode(x,l,r))  = let (a,b,c) = x,l,r in
             if n<= 0 then Node(a,Empty,Empty) else Node(a,(top (n-1) (b ()) ), (top (n-1) (c ()) ))



let rec map  f (LNode(x,l,r)) = LNode(f x, (fun () -> map f (l ())), (fun () -> map f (r ())))

let rec find p (LNode (x,l,r)) = if p x then LNode(x,l,r)
                                else find p (LNode (x,(fun () -> find p (l ())), (fun () -> find p (r ()))))
