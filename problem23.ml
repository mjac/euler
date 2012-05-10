(*
A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
*)
let rec range i j = if i > j then [] else i :: (range (i+1) j);;


let rec isqrt n =
  if n = 1 then 1
  else let n' = isqrt (n - 1) in
  (n' + (n / n')) / 2;;

let stringify l = match l with
        | [] -> "[]"
        | l -> "[" ^ (String.concat "; " (List.map (fun d -> Printf.sprintf "%d"
        d) l)) ^ "]";;

(*module PrimeSieve =
        struct
                let storage = ref [||]
                let rec markprime n (x, xp, xd, xf, xpmin) y =
                        if y > n then () 
                        else (let (y, yp, yd, yf, ypmin) = !storage.(y) in 
                                if yp != 0 then !storage.(y) <- (y, 0, 1, 0, yp)
                                else !storage.(y) <- (y, yp, yd, xf + yf, ypmin);
                                markprime n (x, xd, xf, xpmin) (y + x))
                let resize n = 
                        storage := Array.init (n + 1) (fun x -> (x, 1, 1, 0, 0));
                        !storage.(0) <- (0, 0, 0, 0, 0);
                        !storage.(1) <- (1, 0, 0, 0, 0); (* self, isprime, sum of proper
                        divisors, sum of prime factors, smallest prime factor *)
                        Array.iteri (fun x (p, spd, spf, pmin) -> if p == 1 then (markprime n x (x * 2)) else ()) !storage
                let setSize n = if n >= Array.length !storage then resize n; ()
                let isPrime n = setSize n; let (np, _, _, _) = !storage.(n) in
                np == 1
                let primes n = setSize n; List.filter (fun (x,_,_,_) -> x == 1) (Array.to_list !storage)
        end;;
*)
(*                Printf.printf "%b\n" (PrimeSieve.isPrime 32432);;*)
(*                let primeRange = PrimeSieve.primes (28123 / 2);;
                let properdivisors n = List.filter (fun x -> n mod x == 0) (range 1 (n / 2));;
                let divisors n = (properdivisors n) @ [n];;

                let sum l = List.fold_left (+) 0 l;;
                let abundant n = sum (properdivisors n) > n;;
                let abundantlist n = List.filter (fun n -> abundant n) (range 1 n);;
                let max l = List.fold_left (fun a b -> if a > b then a else b) l;;

                let sumnonsum2abundant n =
                        let arr = Array.init (n + 1) (fun x -> x) in
                                let l = abundantlist n in List.iter (fun x -> List.iter (fun y -> let z = y + x in if z <= n then arr.(z) <- 0) l) l;
                        sum (Array.fold_left (fun xs x -> if x == 0 then xs else
                                x::xs) [] arr);;*)
(*
                print_endline (stringify (abundantlist 15000));
                Printf.printf "%d\n" (sumnonsum2abundant 2);*)



let integer_exponent b e =
          let rec aux x i =
                      if i = e then x else aux (x * b) (i + 1)
                        in
                          aux 1 0;;
(*

let isPrime a = ()

let divisors a = ()
*)
(*fun isAbundant = sum divisors n > n*)

let maxpower n x =
        let rec _maxpower n x l =
                if x mod n == 0
                then _maxpower n (x / n) (l * n)
                else if x == 1 then (n*l - 1) / (1 - n) else l
        in _maxpower n x 1;;

let sumofdivisors n =
        let ds = Array.make (n + 1) 0 in
        let rec markmultiples d x = 
                if x < n then (
                        (if ds.(x) == 0 then ds.(x) <- maxpower d x);
                        markmultiples d (x + d)) in
        ds.(0) <- 1;
        ds.(1) <- 1;
        Array.iteri (fun d cp1 -> if cp1 == 0 then markmultiples d (d + d)) ds;
        let ds2 = Array.copy ds in
        Array.iteri (fun d s -> let cp1 = ds.(d) in if cp1 == 0 then ds2.(d) <-
                d + 1 else if cp1 < 0 then ds2.(d) <- -cp1 else (let cp2 = d / cp1 in
        ds2.(d) <- ds2.(cp1) * ds2.(cp2))) ds;
        ds2;;
(*
Array.iteri (fun d cp1 -> Printf.printf "%d %d\n" d cp1) (sumofdivisors 200);;
*)

let limit = 28123;;

let alldiv = sumofdivisors limit;;

let abundant = Array.fold_left (fun a c -> if c > 0 then c::a else a) [] (Array.mapi (fun d sd ->
        if sd - d > d then d else 0) alldiv);;

let sum2 = Array.make (limit + 1) 0;;

List.iter (fun n -> List.iter (fun m -> let p = m + n in if p <= limit then
        sum2.(p) <- 1) abundant) abundant;;

let result = ref 0;;

Array.iteri (fun d a -> if a == 0 then result := !result + d) sum2;;

Printf.printf "%d\n" !result;;
