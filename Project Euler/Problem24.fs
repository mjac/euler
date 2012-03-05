module Euler.Problems.Problem24

let symbols = [0..9]
let rec factorial n = match n with | 0 -> 1 | x -> x*(factorial (x-1))

(*
n is the required index
*)

let rec searchSpace n xs t =
    if Set.empty = xs
        then List.rev t
        else
            let range = factorial (Set.count xs - 1)
            let tidx = n / range
            let symbol = List.nth (Set.toList xs) tidx
            in searchSpace (n - tidx * range) (xs.Remove(symbol)) (symbol::t)

(*
Apparently this is faster
http://www.freewebz.com/permute/soda_submit.html
*)