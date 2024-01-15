module Chapter8.BankersDequeue
open FSharpx.Collections

type 'a Dequeue  = int * 'a LazyList * int * 'a LazyList

let c = 2
let check (lf, f, lr, r): 'a Dequeue =
    if lf > c * lr + 1 then
        let i = (lf + lr) / 2
        let j = lf + lr - i
        let f' = LazyList.take i f
        let r' = LazyList.append r (LazyList.rev (LazyList.drop i f))
        (i, f', j, r')
    else if lr > c * lf + 1 then
        let j = (lf + lr) / 2
        let i = lf + lr - j
        let r' = LazyList.take j r
        let f' = LazyList.append f (LazyList.rev (LazyList.drop j r))
        (i, f', j, r')
    else (lf, f, lr, r)
    
let empty = (0, LazyList.empty, 0, LazyList.empty)
let isEmpty (lf, f, lr, r) = lf + lr = 0

let cons x (lf, f, lr, r) = check (lf + 1, LazyList.cons x f, lr, r)
let head (lf, f, lr, r) =
    match f with
    | LazyList.Nil -> LazyList.head r
    | LazyList.Cons(x, _) -> x

let tail (lf, f, lr, r) =
    match f with
    | LazyList.Nil -> empty
    | LazyList.Cons(x, xs) -> check (lf - 1, xs, lr, r)
    
let snoc y (lf, f, lr, r) = check(lf, f, lr + 1, LazyList.cons y r)
let last (lf, f, lr, r) =
    match r with
    | LazyList.Nil -> LazyList.head f
    | LazyList.Cons(x, _) -> x
let init (lf, f, lr, r) =
    match r with
    | LazyList.Nil -> empty
    | LazyList.Cons(y, yr) -> check(lf, f, lr - 1, yr) 