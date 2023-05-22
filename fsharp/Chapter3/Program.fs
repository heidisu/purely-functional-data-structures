open Chapter3.Heap

let tree = Heap.insert 5 (Heap.insert 8 (Heap.insert 4 (Heap.insert 6 Heap.empty)))

printfn "%A" tree