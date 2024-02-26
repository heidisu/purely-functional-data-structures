module Chapter9.ZerolessRedundantRandomAccessList

open FSharpx.Collections

type 'a Tree = Leaf of 'a | Node of int * 'a Tree * 'a Tree
type 'a Digit =
 | One of 'a Tree
 | Two of 'a Tree * 'a Tree
 | Three of 'a Tree * 'a Tree * 'a Tree
type 'a RList = 'a Digit LazyList

let size t =
    match t with
    | Leaf _ -> 1
    | Node (w, _, _) -> w

let link t1 t2 = Node (size t1 + size t2, t1, t2)
    
let rec consTree t ts =
    match ts with
    | LazyList.Nil -> LazyList.cons (One t) LazyList.empty
    | LazyList.Cons(One t',ts) -> LazyList.cons (Two (t, t')) ts
    | LazyList.Cons(Two (t', t''), ts) -> LazyList.cons (Three (t, t', t'')) ts
    | LazyList.Cons(Three (t', t'', t'''), ts) -> LazyList.cons (Two (t, t')) (consTree (link t'' t''') ts)
    
let cons x ts = consTree (Leaf x) ts

let rec unconsTree l =
    match l with
    | LazyList.Cons(One t, LazyList.Nil) -> (t, LazyList.empty)
    | LazyList.Cons(One t, ts) ->
        let (Node (_, t1, t2), ts') = unconsTree ts
        (t, LazyList.cons (Two (t1, t2)) ts')
    | LazyList.Cons(Two (t, t'), ts) ->
        (t, LazyList.cons (One t') ts)
    | LazyList.Cons(Three (t, t', t''), ts) ->
        (t, LazyList.cons (Two (t', t'')) ts)

let head ts =
    match ts with
    | One (Leaf x) :: _ -> x 
    | Two (Leaf x, _) :: _ -> x
    | Three (Leaf x, _, _) :: _ -> x

let tail ts =
    let (_, ts') = unconsTree ts
    ts'