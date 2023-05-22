open Chapter3.Heap

let tree = Heap.insert 5 (Heap.insert 8 (Heap.insert 4 (Heap.insert 6 Heap.empty)))

printfn "%A" tree

let tree2 = Heap.exercise_3_2 5 (Heap.exercise_3_2 8 (Heap.exercise_3_2 4 (Heap.exercise_3_2 6 Heap.empty)))

printfn "%A" tree2