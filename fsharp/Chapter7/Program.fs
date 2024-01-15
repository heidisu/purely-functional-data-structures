// For more information see https://aka.ms/fsharp-console-apps
open System
open Chapter7
open Chapter7.RealTimeQueue
open Chapter7.ScheduledBinomialHeap

let q = snoc RealTimeQueue.empty 1
let q2 = snoc q 2
let q3 = snoc q2 3
let q4 = snoc q3 4
let q5 = snoc q4 5
let q6 = snoc q5 6
let q7 = snoc q6 7

(*
printfn "%A" q
printfn "%A" q2
printfn "%A" q3
printfn "%A" q4
printfn "%A" q5
printfn "%A" q6
printfn "%A" q7
*)
let s = ScheduledBinomialHeap.empty

(*
[1 .. 10]
|> List.fold(fun s i ->
    printfn "Binomial: %A\n  Heap: %A\n  Schedule: %A" (Convert.ToString(i - 1, 2)) (fst s) (snd s)
    insert i s
    ) s
|> printfn "%A"
*)

[1 ..10]
|> List.fold(fun s i ->
    printfn "Binomial: %A\n  Sortable: %A\n" (Convert.ToString(i - 1, 2)) (snd s)
    ScheduledBottomUpMergeSort.add i s
    ) ScheduledBottomUpMergeSort.empty
|> printfn "%A"

