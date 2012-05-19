

let primesieve n = let storage = Array.make (n + 1) 1 in
        storage.(0) <- 0;
        storage.(1) <- 0;
        let rec markprime n x y = if y <= n then (storage.(y) <- 0; markprime n x (y + x))
                in Array.iteri (fun x p -> if p == 1 then markprime n x (x * 2)) storage;
        storage;;

let sievetest = primesieve 1000;;

sievetest.(2) = 1;;
sievetest.(13) = 1;;
sievetest.(14) = 0;;

let rec ordern n = if n = 0 then 1 else 10 * ordern (n / 10);;

ordern 353;;

let isCircular sieve p =
        let rec _rec q primes = 
                let d = q mod 10 in
                        if d = 0 || sieve.(q) = 0 then [] else
                                 if p = q && List.length primes > 0 then primes
                                 else _rec (q / 10 + d * (ordern q / 10)) (q::primes)
        in _rec p [];;

(197, isCircular sievetest 197);;

let countArray f = Array.fold_left (fun a c -> if f c then a + 1 else a) 0;;
let countTrue = countArray (fun a -> a);;

let circularPrimes sieve = Array.mapi (fun x p -> p == 1 && List.length (isCircular sieve x) > 0) sieve;;

(13, countTrue (circularPrimes (primesieve 100)));;

countTrue (circularPrimes (primesieve 1000000));;
