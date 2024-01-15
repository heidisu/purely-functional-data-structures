module Chapter9.SparseByWeight

type Nat = int list

let zero  = []

let rec carry w l =
    match l with
    | [] -> [ w ]
    | x :: xs -> if w < x then w :: l else carry (2 * w) xs

let rec borrow w l  =
    match l with
    | x :: xs -> if w = x then xs else  w :: borrow  (2 * w) l
    | _ -> failwith "cannot borrow"

let inc ws = carry 1 ws
let dec ws = borrow 1 ws

let rec add a b =
    match a, b with
    | [], b -> b
    | a, [] -> a
    | x :: xs, y :: ys ->
        if x < y then x :: add xs b
        else if y < x then y :: add a ys
        else carry (2 * x) (add xs ys)

