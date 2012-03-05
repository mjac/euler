module Euler.Problems.Problem19

let leapYear y = (y % 4 = 0 && y % 100 <> 0) || y % 400 = 0

let monthDayBasic = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31]

let monthDays y m =
    if m = 1 && leapYear y
        then 29
        else List.nth monthDayBasic m

let rec dayCounter ylim f y m c t = if y > ylim then t else 
    dayCounter
        ylim f
        (if m = 11 then y + 1 else y)
        (if m = 11 then 0 else m + 1)
        (c + monthDays y m)
        (t + f c)

(* Jan 1st 1901 is 1 day in the week after Jan 1st 1900 *)
let calcSundays () = dayCounter 2000 (fun c -> if c % 7 = 0 then 1 else 0) 1901 0 2 0


(* Zeller's formula is notable *)