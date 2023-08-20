
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

printfn $"%A{SplayHeap.toList sh}"

let sortList = [6; 4; 7; 2; 9; 10; 0] |> SplayHeap.sorted
printfn $"%A{sortList}"

let sortSortedInc = [1; 2; 3; 4] |> SplayHeap.sorted
let sortSortedDesc = [4; 3; 2; 1] |> SplayHeap.sorted

let pheap = PairingHeap.insert 7 (PairingHeap.insert 3 (PairingHeap.insert 5 (PairingHeap.insert 1 (PairingHeap.insert 2 PairingHeap.empty))))

printfn $"%A{pheap}"


let pheap' = PairingHeap.deleteMin pheap
printfn $"%A{pheap'}"

let binTree = PairingHeap.toBinary pheap
printfn $"%A{binTree}"