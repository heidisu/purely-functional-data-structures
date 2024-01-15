module Chapter8.Program
open Chapter8

(*
let set =
    [1 .. 20]
    |> List.fold (fun t e -> RedBlackSet.insert e t) RedBlackSet.empty

printfn $"Set %A{set}"

let slettet =
    [1 .. 20]
    |> List.fold (fun s e ->
        let ns = RedBlackSet.delete e s
        printfn $"Slettet: %A{e} \tnytt sett: %A{ns}"
        ns
        ) set
        *)

(*
let queue =
    [1 .. 20]
    |> List.fold (fun s e ->
            let ns = HoodMelvilleQueue.snoc s e
            printfn $"Snoc: %A{e}\tResultat: %A{ns}"
            ns
        ) HoodMelvilleQueue.empty

let tails =
    [1 .. 20]
    |> List.fold (fun s e ->
            let ns = HoodMelvilleQueue.tail s
            printfn $"Tail: #%A{e}\tResultat: %A{ns}"
            ns     
            ) queue
*)

let dequeue =
    [1 .. 20]
    |> List.fold (fun s e ->
            let ns = BankersDequeue.snoc e s
            printfn $"Snoc:%A{e}\tResultat: %A{ns}"
            ns
        ) BankersDequeue.empty           