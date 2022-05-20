type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree)
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree


let layer_tree r =
            let rec impl r n = LNode(
            (r+n), (fun () -> impl r (n+1)) , (fun () -> impl r (n+1))
            )
            in impl r 0

let rec  interval_tree l h = LNode( (l,h) , (fun () -> interval_tree l ((h+.l)/.2.)) ,
                             (fun () -> interval_tree ((h+.l)/.2.) h))

let rec rational_tree n d = LNode ( (n,d) , (fun () -> rational_tree n (d+1)),
                            (fun () -> rational_tree (n+1) d))

let rec top n (LNode(x,l,r))  = let (a,b,c) = x,l,r in
             if n<= 0 then Node(a,Empty,Empty) else Node(a,(top (n-1) (b ()) ), (top (n-1) (c ()) ))

let rec map  f (LNode(x,l,r)) = LNode(f x, (fun () -> map f (l ())), (fun () -> map f (r ())))

let find p (LNode (x,l,r)) =
                    let rec impl k n (LNode (x,l,r)) = if n mod 2 = 1 then
                            	if p x then LNode (x,l,r) else impl p (n-1) (l ())
                            else
                            	if p x then LNode (x,l,r) else impl p (n-1) (r ())
                            in impl p 1 (LNode(x,l,r))
