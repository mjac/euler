module Euler.Problems.Problem7

let prime n =
    let rec modCheck c =
        if (c >= n) then true
        else if (n % c = 0) then false
        else modCheck (c+1)
    in
        modCheck 2


let primeFinder n =
    let rec checker c p =
        if (prime p) then (if (n = c) then p else checker (c+1) (p+1))
        else checker c (p+1)
    in
        checker 1 2