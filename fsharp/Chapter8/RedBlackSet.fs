module Chapter8.RedBlackSet

type 'a Elem = 'a * bool
type Color = R | B
type 'a Tree = E | T of Color * 'a Tree * 'a Elem * 'a Tree
type 'a Set = int * int * 'a Tree

let empty: 'a Set = (0, 0, E)
let rec membero: 'a * 'a Set -> bool  = function
    | (x, (t, d, E)) -> false
    | (x, (t, d, T (_, a, (y, _), b))) ->
        if x < y then membero (x, (0, 0, a))
        else if y < x then membero (x, (0, 0, b))
        else true
        
let balance = function
    | (B,T (R,T (R,a,x,b),y,c),z,d) -> T (R,T (B,a,x,b),y,T (B,c,z,d))
    | (B,T (R,a,x,T (R,b,y,c)),z,d) -> T (R,T (B,a,x,b),y,T (B,c,z,d))
    | (B,a,x,T (R,T (R,b,y,c),z,d)) -> T (R,T (B,a,x,b),y,T (B,c,z,d))
    | (B,a,x,T (R,b,y,T (R,c,z,d))) -> T (R,T (B,a,x,b),y,T (B,c,z,d))
    | body -> T body

let  insert (v: 'a) ((t, d, s): 'a Set) : 'a Set =
    let rec ins x s =
        match x, s with
        | x, E -> T(R, E, x, E)
        | x, (s as T (color, a, y, b)) ->
            if x < y then balance (color, ins x a, y, b)
            else if y < x then balance (color, a, y, ins x b)
            else s
    match ins (v, false) s with (* guaranteed to be non-empty *)
    | T (_, a, y, b) -> (t + 1, d, T (B, a, y, b))
    | E -> failwith "Should not happen"

let rec rightIns x = function
        | E -> T(R, E,x, E)
        | s as T (color, a, y, b) -> balance (color, a, y, rightIns x b)
    
let fromOrdList (xs: 'a Elem list) =
    let rec balance' = function
        | [(R, v1, t1)] -> [(B, v1, t1)]
        | (R, v1, t1) :: (R, v2, t2) :: (B, v3, t3) :: xs ->
            (B, v1, t1) :: (balance' ((R, v2, T(B, t3, v3, t2)):: xs))
        | xs -> xs
    
    let rec ins = function
        | ts, [] -> ts
        | ts, x :: xs -> ins (balance' ((R, x, E) :: ts), xs)
    
    let rec toTree = function
        | t, [] -> t
        | t, (color, v, t') :: ts -> toTree (T (color, t', v, t), ts)
    
    toTree(E, ins([], xs))

let rebuild  (t: 'a Tree): 'a Set =
    printfn $"Rebuild %A{t}"
    let rec balance': (Color * 'a Elem * 'a Tree) list -> (Color * 'a Elem * 'a Tree) list  = function
        | (R, v1, t1) :: (R, v2, t2) :: (B, (v3, _), t3) :: xs ->
            (B, v1, t1) :: (balance' ((R, v2, T(B, t3, (v3, false), t2)):: xs))
        | xs -> xs
    
    let rec ins: int * (Color * 'a Elem * 'a Tree) list * 'a Tree  -> int * (Color * 'a Elem * 'a Tree) list = function
        | n, ts, E -> (n, ts)
        | n, ts, T (_, a, (x, b), c) ->
            let (n', ts') = ins (n, ts, a)
            // b = false => ikke slettet, x skal være med
            if not b then ins (n' + 1, balance' ((R, (x, b), E):: ts'), c)
            else ins(n', ts', c)
    
    let rec toTree: 'a Tree * (Color * 'a Elem * 'a Tree) list -> 'a Tree = function
        | t, [] -> t
        | t, (color, (v, _), t') :: ts -> toTree (T (color, t', (v, false), t), ts)
    let (n, ts') = ins (0, [], t) 
    (n, 0, toTree(E, ts'))
    
let delete (x: 'a) ((i, d, t): 'a Set): 'a Set =
    let rec delTree (t: 'a Tree): 'a Tree = 
        match t with
        | E -> E
        | T (c, a, (y, bo), b) ->
            if x < y then T (c, (delTree a), (y, bo), b)
            else if y < x then T(c, a, (y, bo), delTree b)
            else T (c, a, (y, true), b)
    let t' = delTree t
    if d > i then rebuild t' else (i - 1, d + 1, t')