﻿
open Chapter5

let queue = List.fold (fun acc x -> Queue.snoc acc x) Queue.empty [1..10]

printfn "%A" queue

printfn "%A" (Queue.tail queue)

(*
let deque = Deque.snoc (Deque.cons 2 (Deque.cons 1 Deque.empty)) 3

printfn "%A" deque

printfn "%A" (Deque.head deque)
printfn "%A" (Deque.head (Deque.tail deque))
printfn "%A" (Deque.head (Deque.tail (Deque.tail deque)))
printfn "%A" (Deque.cons 4 deque)
*)

let empty = SplayHeap.E
let sh =
    empty
    |> SplayHeap.insert 1
    |> SplayHeap.insert 3
    |> SplayHeap.insert 2
    |> SplayHeap.insert 7
    |> SplayHeap.insert 5
    |> SplayHeap.insert 0

printfn $"%A{sh}"