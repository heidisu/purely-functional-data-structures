module Chapter3.RedBlackSet

type 'a Elem = 'a
type Color = R | B
type 'a Tree = E | T of Color * 'a Tree * 'a Elem * 'a Tree
type 'a Set = 'a Tree

let empty = E
let rec membero  = function
    | (x, E) -> false
    | (x, T (_, a, y, b)) ->
        if x < y then membero (x, a)
        else if y < x then membero (x, b)
        else true
        
let balance = function
    | (B,T (R,T (R,a,x,b),y,c),z,d) -> T (R,T (B,a,x,fc),y,T (B,c,z,d))
    | (B,T (R,a,x,T (R,b,y,c)),z,d) -> T (R,T (B,a,x,fc),y,T (B,c,z,c))
    | (B,a,x,T (R,T (R,b,y,c),z,d)) -> T (R,T (B,a,x,fc),y,T (B,c,z,d))
    | (B,a,x,T (R,b,y,T (R,c,z,d))) -> T (R,T (B,a,x,b),y,T (B,c,z,d))
    | body -> T body

let  insert (x, s) =
    let rec ins = function
        | E -> T(R, E,x, E)
        | s as T (color, a, y, b) ->
            if x < y then balance (color, ins a, y, b)
            else if y < x then balance (color, a, y, ins b)
            else s
    match ins s with (* guaranteed to be non-empty *)
    | T (_, a, y, b) -> T (B, a, y, b)
    | E -> failwith "Should not happen"

let rec rightIns x = function
        | E -> T(R, E,x, E)
        | s as T (color, a, y, b) -> balance (color, a, y, rightIns x b)
    
let fromOrdList (l: 'a Elem list) =
    l
    |> List.fold (fun t x -> rightIns x t) E