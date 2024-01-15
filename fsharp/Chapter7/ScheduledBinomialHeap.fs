module Chapter7.ScheduledBinomialHeap

open FSharpx.Collections

type 'a Tree = Node of 'a * 'a Tree list

type 'a Digit =
    | Zero
    | One of 'a Tree

type 'a Schedule = 'a Digit LazyList List
type 'a Heap = 'a Digit LazyList
type 'a HeapS = 'a Digit LazyList * 'a Schedule

let exec: 'a Schedule -> 'a Schedule =
    function
    | [] -> []
    | LazyList.Cons (One t, _) :: sched ->
        printfn $"exec: %A{t}"
        sched
    | LazyList.Cons (Zero, job) :: sched ->
         printfn $"exec: Zero"
         job :: sched

let empty: 'a HeapS = (LazyList.empty, [])

let link t1 t2 : 'a Tree =
    match t1, t2 with
    | Node (x1, c1), Node (x2, c2) ->
        if x1 < x2 then
            Node(x1, t2 :: c1)
        else
            Node(x2, t1 :: c2)

let rec insTree: 'a Tree * 'a Heap -> 'a Heap =
    function
    | t, LazyList.Nil -> LazyList.cons (One t) LazyList.empty
    | t, LazyList.Cons (Zero, ds) -> LazyList.cons (One t) ds
    | t, LazyList.Cons (One t', ds) -> LazyList.cons Zero (insTree ((link t t'), ds))

let insert (x: 'a) ((ds, sched): 'a HeapS) : 'a HeapS =
    let ds' = insTree (Node(x, []), ds)
    (ds', exec (exec (ds' :: sched)))

let rec mrg = function
    | ds1, LazyList.Nil -> ds1
    | LazyList.Nil, ds2 -> ds2
    | LazyList.Cons (Zero, ds1), LazyList.Cons(d, ds2) -> LazyList.cons d  (mrg (ds1, ds2))
    | LazyList.Cons (d, ds1), LazyList.Cons(Zero, ds2) -> LazyList.cons d (mrg (ds1, ds2))
    | LazyList.Cons (One t1, ds1), LazyList.Cons(One t2, ds2) -> LazyList.cons Zero (insTree (link t1 t2, mrg (ds1, ds2)))

// mrgWithList (rev c, dsf)
// mrg (listToStream (map ONE (rev c)), ds')
let rec mrgWithList: 'a Tree list * 'a Heap -> 'a Heap = function
    (* ts er alltid mindre eller lik ds *)
    | [], ds -> ds
    | t :: ts, LazyList.Cons (Zero, ds) -> LazyList.cons (One t) (mrgWithList (ts, ds))
    | t :: ts, LazyList.Cons (One t', ds) -> LazyList.cons Zero (insTree (link t t', mrgWithList (ts, ds)))