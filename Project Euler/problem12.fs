module problem12

let primeFactors n =
    let rec reduce n c (t::ts) = match n with
        | 0 -> []
        | 1 -> (t::ts)
        | x -> if x % c = 0 then reduce (x/c) c ((t+1)::ts)
               else reduce n (c+1) (if t = 0 then (t::ts) else (0::t::ts)) in reduce n 2 [0]

let divisors n = primeFactors n |> List.fold (fun a m -> a * (m+1)) 1

let rec divMatch c n divCount = if divisors n > divCount then n else divMatch (c+1) ((c+1)*(c+2)/2) divCount
divMatch 1 1 500 |> printfn "%i"

System.Console.ReadLine() |> ignore

(*
Just use mod, it is actually very easy, from 1 to sqrt triangle if mods it increment count... easy
** Notice you need only check for factors up to the square root of the triangle number - each number above has a mirror factor below.

http://www.shyamsundergupta.com/triangle.htm
Highly Composite Triangular Numbers:
Numbers such that d(n), the number of divisors of n, is greater than for any smaller n are called highly composite numbers.
If n is a triangular number then it can be termed as Highly Composite Triangular Number . For example 28 is a triangular number and d(28) = 6 .
Number of divisors of all triangular numbers less than 28 is less than 6. So 28 is a Highly Composite Triangular number.

*)

(* NOT NECESSARY *)

(* the Euclidean algorithm *)
let rec gcd a b = match b with | 0 -> a | x -> gcd b (a % b)

(* Least common multiple *)
let lcm a b = a*b/(gcd a b)

(*[1..30] |> List.fold (fun acc x -> lcm acc x) 1 |> printfn "%i"*)

(*
d(n) < 2 sqrt n
d(n) > 500, 500 < 2 sqrt n      n > 250^2
tri n = 1/2n(n+1) -> 1/2n(n+1) > 250^2   n^2 + n - 125000 = 0

Everything is http://en.wikipedia.org/wiki/Divisor#Divisibility_of_numbers

The unique factorization theorem says that every positive integer greater than 1 can be written in only one way as a product of prime numbers.
The prime numbers can be considered as the atomic elements which, when combined together, make up a composite number.

Divisors
http://stackoverflow.com/questions/110344/algorithm-to-calculate-the-number-of-divisors-of-a-given-number
http://mathforum.org/library/drmath/view/55843.html

a^n*b^m*c^p  will have (n + 1)(m + 1)(p + 1) factors.

Minimise the number... n=m=p

 lowest common multiple or (LCM) least common multiple 


1/2n(n+1) 1/2(n+1)(n+2)
(n+1) is difference
javascript:function divisors(n){var divs=[]; for(i=1;i<=n;++i) if(n%i==0) divs[divs.length]=i; return divs;};var t=1,u=1;while(t<100000){if (divisors(u).length>200){alert(u);break;}++t;u=u+t;};

javascript:var ps=[];function prime(n){for(var i in ps){if(n%ps[i]==0){return false;}}ps[ps.length]=n;return true;} var t=2; while(ps.length<=500){prime(t);++t;}document.write(ps);
no

GCM...
first number to have over 500 divisors
javascript:function zero(len){var ks=[];for(i=0;i<len;++i){ks[i]=0;}return ks;} var ps=[]; function prime(n){for(var i in ps){if(n%ps[i]==0){return false;}}ps[ps.length]=n;return true;} var t=2;while(t<=500){prime(t);++t;} function primedivs(n){var divs=zero(ps.length);while(n>1){for(var i in ps){if(n%ps[i]==0){n/=ps[i];++divs[i];}}}return divs;} function merge(us, vs){var ns=zero(ps.length);for(var i in ps){ns[i]=Math.max(us[i],vs[i]);}return ns;} t=2; var ts=zero(ps.length); while(t<=500){ts=merge(ts, primedivs(t));++t;};var total=1;for(var i in ps){total*=Math.pow(ps[i],ts[i]);document.write(ps[i] + " " + ts[i] + "<br />");};
*)