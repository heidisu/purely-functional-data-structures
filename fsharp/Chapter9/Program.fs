open Chapter9
open Chapter9.ListAndNumbers
open Chapter9.SparseByWeight
open Chapter9.RandomAccessList
open Chapter9.ZerolessRandomAccessList

let liste = Cons (3, Cons (2, Cons (1, Nil)))
let natThree = Succ(Succ(Succ ListAndNumbers.Zero))
printfn "%A" liste
printfn "%A" natThree

let zero = [ ]

let five = zero |> Dense.inc |> Dense.inc |> Dense.inc |> Dense.inc |> Dense.inc
let six = Dense.inc five
let sum = Dense.add five six
(*
printfn "%A" five
printfn "%A" six
printfn "%A" sum
*)
let four = SparseByWeight.zero |> inc |> inc |> inc |> inc
let three = four |> dec
let seven = add four three

let ten = add seven three

(*
printfn "%A" four
printfn "%A" three
printfn "%A" seven
printfn "%A" (inc seven)
printfn "%A" ten
*)

let ral1 = RandomAccessList.cons 1 (RandomAccessList.cons 2 (RandomAccessList.cons 3 []))
let ral2 = RandomAccessList.cons 4 ral1
printfn "%A" ral1
printfn "%A" ral2

(*
tail [Zero; Zero; One (Node (4, Node (2, Leaf 4, Leaf 1), Node (2, Leaf 2, Leaf 3)))]
    unconsTree (Zero :: ts)
        unconsTree ts = unconsTree (Zero :: ts')
            unconsTree [One (Node (4, Node (2, Leaf 4, Leaf 1), Node (2, Leaf 2, Leaf 3)))]
            (Node (4, Node (2, Leaf 4, Leaf 1), Node (2, Leaf 2, Leaf 3)), [])
        (Node (2, Leaf 4, Leaf 1), [One (Node (2, Leaf 2, Leaf 3))])
    (Leaf 4, [One (Leaf 1),  One (Node (2, Leaf 2, Leaf 3))])
[One (Leaf 1),  One (Node (2, Leaf 2, Leaf 3))]
*)
printfn "%A" (RandomAccessList.tail ral2)

printfn "Lookup: %A" (RandomAccessList.lookup 3 ral2)

printfn "drop: %A" (RandomAccessList.drop 1 ral2)

printfn "oppg 9.2 %A" (create 5 3)

let zthree = ZerolessRandomAccessList.inc (inc (inc []))
let zfour = ZerolessRandomAccessList.inc (inc (inc (inc [])))
let ztwo = ZerolessRandomAccessList.inc (inc [])

printfn "%A" zthree
printfn "%A" zfour
printfn "%A" (add zthree zfour)
printfn "%A" (add ztwo ztwo)
