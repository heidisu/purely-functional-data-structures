
open Chapter5

let queue = List.fold (fun acc x -> Queue.snoc acc x) Queue.empty [1..10]

printfn "%A" queue