module Chapter9.ListAndNumbers

type 'a Liste = Nil | Cons of 'a * 'a Liste

let tail (Cons(s, xs)) = xs

let rec append l ys =
    match l with
    | Nil -> ys
    | Cons (s, xs) -> Cons(s, append xs ys)

type Nat = Zero | Succ of Nat

let pred (Succ n) = n

let rec plus m n =
    match m with
    | Zero -> n
    | Succ m' -> Succ(plus m' n)

