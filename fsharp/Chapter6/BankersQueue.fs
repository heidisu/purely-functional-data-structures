module Chapter6.BankersQueue

type 'a Queue = int * 'a seq * int * 'a seq

let empty = (0, Seq.empty, 0, Seq.empty)

let isEmpty (lenf, _, _, _) = lenf = 0



