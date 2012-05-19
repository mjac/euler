(*
The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in
base 10 and base 2.

(Please note that the palindromic number, in either base, may not include
leading zeros.)
*)


let rec digits b d =
        let rec _rec n ds =
                if n = 0 then ds
                else _rec (n / b) ((n mod b)::ds)
        in _rec d [];;

let palin b d = let ds = digits b d in ds = List.rev ds;;

let palin2and10 d = palin 2 d && palin 10 d;;

let rec sumto max x sum = if x > max then sum else sumto max (x + 1) (if palin2and10 x then (sum + x) else sum);;

sumto 1000000 1 0;;

