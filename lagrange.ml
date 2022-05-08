let  down  x1 list = let rec helper list acc =
match list with
| [] -> acc
| h::t -> if h = x1 then helper t acc else helper t ((x1-.h)*.acc)
in helper list 1.

let up f x1 list = let rec helper list acc =
match list with
| [] -> acc
| h::t -> if x1 = h then helper t acc else helper t ((f-.h)*.acc)
in helper list 1.

let all_x list =  let rec helper list acc = match list with
| [] -> acc
| (x,y)::t -> helper t (x::acc)
in helper list []

let lagrange lst f =
                let fx = all_x lst in
                let rec helper lst acc = match lst with
                | [] -> acc
                | (x,y)::t ->
                              let nominator = up f x fx in
                              let denominator = down x fx in
                              let result = nominator /. denominator in
                              helper t ((y *. result)+.acc)
                              in helper lst 0.
