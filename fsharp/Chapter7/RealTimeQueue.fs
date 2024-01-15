module Chapter7.RealTimeQueue

open FSharpx.Collections

type 'a Queue = 'a LazyList * 'a list * 'a LazyList

let empty : 'a Queue = (LazyList.empty, [], LazyList.empty)

let rec rotate = function
    | LazyList.Nil, y::_, a -> LazyList.cons y a
    | LazyList.Cons (x, xs) , y :: ys, a -> LazyList.cons x (rotate (xs, ys, LazyList.cons y a))

let exec = function
    | f, r, LazyList.Cons (x, s) -> (f, r, s)
    | f, r, LazyList.Nil ->
        let f' = rotate (f, r, LazyList.empty) in (f', [], f')

let snoc (f, r, s) x = exec (f, x :: r, s)

let size (_, r, s) = LazyList.length s + 2 * List.length r

// |s| <= |f|
// mest effektiv når |f| = |r|, da er |s| tom.