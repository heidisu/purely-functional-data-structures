module Chapter9.SegmentedBinaryNumbers

// type DigitBlock = Zeros of int | Ones of int
// type Nat = DigitBlock list

type Digits = Zero | Ones of int | Two
type Nat = Digits list

(*
let zeros i xs = 
    match (i, xs) with
    | (_, []) -> []
    | (0, xs) -> xs
    | (i, Zeros j :: ys) -> Zeros (i + j) :: ys
    | (i, xs) -> Zeros i :: xs
*)
let ones i xs = 
    match (i, xs) with
    | (0, xs) -> xs
    | (i, Ones j :: ys) -> Ones (i + j) :: ys
    | (i, xs) -> Ones i :: xs

(*
let rec inc xs =
    match xs with
    | [] -> [Ones 1]
    | Zeros i :: ys -> ones 1 (zeros (i - 1) ys)
    | Ones i :: ys -> Zeros i :: inc ys

let rec dec xs = 
    match xs with
    | Zeros i :: ys -> Ones i :: dec ys
    | Ones i :: ys -> zeros 1 (ones (i - 1) ys)
*)


let simpleInc xs = 
    match xs with
    | [] -> [ Ones 1]
    | Zero :: xs -> ones 1 xs
    | Ones i :: xs -> Two :: ones (i - 1) xs

let fixup xs = 
    match xs with
    | Two :: xs -> Zero :: simpleInc xs
    | Ones i :: Two :: xs -> Ones i :: Zero :: simpleInc xs
    | xs -> xs

let inc xs = xs |> simpleInc |> fixup