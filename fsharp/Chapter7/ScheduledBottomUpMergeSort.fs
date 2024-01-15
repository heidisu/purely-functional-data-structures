module Chapter7.ScheduledBottomUpMergeSort

open FSharpx.Collections

type 'a Schedule = 'a LazyList list

type 'a Sortable = int * ('a LazyList * 'a Schedule) list

let rec streamToList = function
    | LazyList.Nil -> []
    | LazyList.Cons(x, xs) -> x :: streamToList xs

let rec exec1: 'a Schedule -> 'a Schedule = function
    | [] ->
        printfn $"exec1: []"
        []
    | LazyList.Nil :: sched ->
        printfn $"exec1: %A{sched}"
        exec1 sched
    | LazyList.Cons(x, xs) :: sched ->
        let res = xs :: sched
        printfn $"exec1: %A{res}"
        res

let exec2 ((segs, sched) : ('a LazyList * 'a Schedule)) = (segs, exec1 (exec1 sched))

(*
n = 11 binært: 1011
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ]
[1, 2], [3, 4], [5, 6], [7, 8], [9, 10], [11]
[1, 2, 3, 4] [5, 6, 7, 8], [9, 10], [11]
[1, 2, 3, 4, 5, 6, 7, 8], [9, 10], [11]

s_0 = [12]
s_1 = [11]
s_2 = [9,10]

((s_0 * s_1) * s_2)
*)

let rec mrg xs ys =
    match xs, ys with
    | LazyList.Nil, ys -> ys
    | xs, LazyList.Nil -> xs
    | LazyList.Cons(x, xs'), LazyList.Cons(y, ys') ->
        if x < y then LazyList.cons x (mrg xs' ys)
        else LazyList.cons y (mrg xs ys')

let empty = (0, [])
let add (x: 'a) ((size, segs): 'a Sortable) : 'a Sortable =
    let rec addSeg xs segs size rsched: ('a LazyList * 'a Schedule) list =
        if size % 2 = 0 then (xs, List.rev rsched) :: segs
        else
            let (xs', []) :: segs' = segs
            let xs'' = mrg xs xs'
            addSeg xs'' segs' (size / 2) (xs'' :: rsched)
    let segs' = addSeg (LazyList.cons x LazyList.empty) segs size []
    (size + 1, List.map exec2 segs')
            