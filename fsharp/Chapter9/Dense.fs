module Chapter9.Dense

type Digit = Zero | One
type Nat = Digit list

let zero = []
let rec inc (n: Nat) =
    match n with
    | [] -> [ One ]
    | Zero :: xs -> One :: xs
    | One :: xs -> Zero :: inc xs

let rec dec (n: Nat) =
    match n with
    | [ One ] -> []
    | One :: ds -> Zero :: ds
    | Zero :: ds -> One :: dec ds

let rec add (m: Nat) (n: Nat) =
    match (m, n) with
    | [], n -> n
    | m, [] -> m
    | x :: xs, Zero :: ys -> x :: add xs ys
    | Zero :: xs, y :: ys -> y :: add xs ys
    | One :: xs, One :: ys -> Zero :: inc (add xs ys)