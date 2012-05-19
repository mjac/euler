(*
145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their
digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.

Limited by d 9!, when 9! * numdigits d exceeds d

Upper limit turns out to be 2540160
*)


let rec digits d =
        let rec _rec n ds =
                if n = 0 then ds
                else _rec (n / 10) ((n mod 10)::ds)
        in _rec d [];;

(123, digits 123);;

let rec fac n = match n with 0 -> 1 | 1 -> 1 | n -> n * fac (n - 1);;

let rec range i j = if i > j then [] else i :: (range (i+1) j);;

let digitfactorals = Array.of_list (List.map (fun x -> fac x) (range 0 9));;

let rec sumcurious x s = let ds = digits x in
        if x > List.length ds * digitfactorals.(9) then s
        else let sumeq = List.fold_left (fun s d -> s + digitfactorals.(d)) 0 ds = x in
                sumcurious (x+1) (if sumeq then x + s else s);;

sumcurious 3 0;;
