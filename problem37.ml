(*
The number 3797 has an interesting property. Being prime itself, it is possible
to continuously remove digits from left to right, and remain prime at each
stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797,
379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to
right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
*)

let primesieve n = let storage = Array.make (n + 1) true in
        storage.(0) <- false;
        storage.(1) <- false;
        let rec markprime n x y = if y <= n then (storage.(y) <- false; markprime n x (y + x))
                in Array.iteri (fun x p -> if p then markprime n x (x * 2)) storage;
        storage;;

let trimdigitright d = d / 10;;
let rec ordern n = if n = 0 then 1 else 10 * ordern (n / 10);;
let trimdigitleft d = d mod (ordern d / 10);;

(202, trimdigitright 202, trimdigitleft 202);;

let countArray f = Array.fold_left (fun a c -> if f c then a + 1 else a) 0;;

let n = 739397;;
let primes = primesieve n;;
let truncprimes = Array.make (n + 1) (false, false);;
truncprimes.(2) <- (true, true);;
truncprimes.(3) <- (true, true);;
truncprimes.(5) <- (true, true);;
truncprimes.(7) <- (true, true);;

Array.iteri (fun d p -> if primes.(d) && d >= 10 then truncprimes.(d) <-
        (let (a1,b1) = truncprimes.(trimdigitleft d) in a1, let (a2,b2) =
                truncprimes.(trimdigitright d) in b2)) truncprimes;;

let afold_lefti f fval0 arr =
        let (_, fvalres) = Array.fold_left (fun (index, fval) cval -> (index + 1, f index fval cval)) (0, fval0) arr
        in fvalres;;

afold_lefti (fun i c (a, b) -> if i >= 10 && a && b then (Printf.printf "%d %b %b\n" i a b; c + i) else c) 0 truncprimes;;

