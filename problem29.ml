let decompose p =
        let rec fac f i n e =
                if n mod f == 0
                then fac f (i + 1) (n / f) e
                else (
                        let enew = if i > 0 then (f, i)::e else e in
                        if n == 1 then enew else (fac (f + 1)) 0 n enew
                )
        in fac 2 0 p [];;


let rec range i j = if i > j then [] else i :: (range (i+1) j);;

let r = range 2 100;;

let ht = Hashtbl.create 10000;;

List.iter (fun a -> List.iter (fun b -> Hashtbl.replace ht (List.map (fun (p, i) -> (p,
i * b)) (decompose a)) 1) r) r;;

Hashtbl.length ht;;
