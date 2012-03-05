module Euler.Problems.Problem15


let nextCoords (x:int,y:int) (grid:int64[,]) =
    let l2 = if x = 20 then [] else [(x+1,y)]
    let l3 = if y = 20 then l2 else (x,y+1)::l2
    List.iter (fun (u:int,v:int) -> grid.[u,v] <- grid.[x,y]+grid.[u,v]) l3
    l3
 
let rec gridSearch grid r = match r with
    | [] -> ()
    | r -> List.collect (fun (x,y) -> nextCoords (x,y) grid) r |> Set.ofList |> Set.toList |> gridSearch grid

let routeCount w h =
    let grid = Array2D.create (w+1) (h+1) 0L
    grid.[0,0] <- 1L
    gridSearch grid [(0,0)]
    grid.[w,h]
    

(*
See
http://mathschallenge.net/index.php?section=problems&show=true&titleid=random_routes&full=true

40!/(20!20!)
40 Choose 20
*)