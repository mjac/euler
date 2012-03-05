module Euler.Problems.Problem26


(*
1/7 = 1     10   30    20   60   40   50   10
0     0 .   1    4     2    8    5    7    1

1/13= 1   10  100  90  120
      0 . 0   7     6    9
*)

let rec division n d t =
    if n < d then division (n*10) d ((0, n)::t) else let r = n % d in division (r*10) d ((n/d, n)::t);

let rec divisionDigit n d =
    if n < d then (n*10, 0, n) else ((n % d)*10, n/d, n)
    
let rec lazyDivision a b =
    LazyList.unfold (fun (n, d) -> 
        if n % d = 0 then None
        else (
            if n < d then Some((0, n), (n*10, d))
            else Some((n/d, n), ((n % d)*10, d))
        )
    ) (a,b)

let cycleLength a b =
    let rec countCycles l s =
        match l with
            | LazyList.Cons(h,t) ->
                match List.tryFindIndex (fun a -> a = h) s with
                    | Some(x) -> x + 1
                    | None -> countCycles t (h::s)
            | LazyList.Nil -> 0
    in
        countCycles (lazyDivision a b) []

//let largestCycle n = [2..1000] |> List.rev |> List.find (fun d -> cycleLength 1 d > 0)
let largestCycle n = [2..1000] |> List.rev |> List.find (fun d -> cycleLength 1 d > 0)
let longestCycle n =
    let a,b = 
        [2..1000] |>
        List.map (fun d -> (d, cycleLength 1 d)) |>
        List.fold (fun (u,v) (a,b) -> if b > v then (a,b) else (u,v)) (0,0)
    in a

(*(
    [2..1000] |>
        List.map (fun d -> (d, Problem26.cycleLength 1 d) |>
        List.fold (fun (u,v) (a,b) -> if b > v then (a,b) else (u,v)) (0,0))
    )*)
(*
    if n < d then division (n*10) d ((0, n)::t) else let r = n % d in division (r*10) d ((n/d, n)::t);

    LazyList.unfold
        (fun x ->
            if x < 13 then
                Some(x, x + 1)
            else
                None)
        10
        *)
(*
This guy wins

(defun multiplicative-order (k n &rest rs)
  "Gives the multiplicative order of K modulo N; or, the
   smallest integer M such that (= (mod (expt K M) N) R)
   for some R.  If no Rs are provided, 1 is used."
  (let ((rs (or rs '(1))))
    (loop for m from 1
          when (member (mod (expt k m) n) rs :test #'=)
            do (return m))))
 
(loop for i from 1000 downto 2
      with maxlen = 0
      with max-i = 0
      while (< maxlen i)
      when (and (= 1 (gcd i 10))
                (< maxlen (multiplicative-order 10 i)))
        do (setf maxlen (multiplicative-order 10 i)
                 max-i i)
      finally (return max-i))



It's pretty fast, even though there's probably an algorithm for multiplicative order that's better than linear time.

[hide code]

; Evaluation took:
;   0.14 seconds of real time
;   0.104006 seconds of user run time
;   0.004 seconds of system run time
;   214,557,432 CPU cycles
;   0 page faults and
;   3,800,336 bytes consed.

*)