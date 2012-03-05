module problem97

(*
The first known prime found to exceed one million digits was discovered in 1999, and is a Mersenne prime of the form 2^6972593-1;
it contains exactly 2,098,960 digits. Subsequently other Mersenne primes, of the form 2p1, have been found which contain more digits.

However, in 2004 there was found a massive non-Mersenne prime which contains 2,357,207 digits: 28433*2^7830457+1.

Find the last ten digits of this prime number.
*)


// int64 overflow :-(
let rec modularPow a n X =
    match n with
    | 0L -> 1L
    | x -> if x &&& 1L = 0L then let y = modularPow a (n/2L) X in ((y*y) % X) else let y = modularPow a (n-1L) X in ((y*a) % X);

let rec largeModularPow a n X t =
    match n with
    | 0L -> 1L
    | 1L -> t
    | x -> largeModularPow a (n-1L) X ((t*a) % X);
    
//((largeModularPow 2L 4L 1000000000L 2L) + 1L) % 1000000000L |> printfn "%i"
// (modularPow 2L 7830457L 2L 10000000000L)*1L |> printfn "%i"
//((modularPow 2L 7830457L 10000000000L) * 1L+1L) % 10000000000L |> printfn "%i"
((largeModularPow 2L 7830457L 10000000000L 2L)*28433L+1L) % 10000000000L |> printfn "%i"
//((modularPow 2L 43112609L 1000000L) - 1L) % 1000000L |> printfn "%i"


System.Console.ReadLine() |> ignore

(*
Since the last 10 digits of 2^N have a period of 7812500 (4*5^9), this was my Ruby solution: 

print (2**(7830457 % (4 * 5**9)) * 28433 + 1) % 10000000000

wow!

// a^b % m
modpow(a,b,m) 
  k= smallest number that makes a^k > m
  a=a^k % m  
  return modpow(a,b/k,m)

see http://en.wikipedia.org/wiki/Modular_arithmetic
find smaller number congruent...
*)