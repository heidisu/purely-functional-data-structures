module Chapter9.RandomAccessList

type 'a Tree = Leaf of 'a | Node of int * 'a Tree * 'a Tree
type 'a Digit = Zero | One of 'a Tree
type 'a RList = 'a Digit list

let size t =
    match t with
    | Leaf _ -> 1
    | Node (w, _, _) -> w

let link t1 t2 = Node (size t1 + size t2, t1, t2)
    
let rec consTree t ts =
    match ts with
    | [] -> [One t]
    | Zero :: ts -> One t :: ts
    | One t' :: ts -> Zero :: consTree (link t t') ts
    
let cons x ts = consTree (Leaf x) ts

let rec unconsTree l =
    match l with
    | [ One t ] -> (t, [])
    | One t :: ts -> (t, Zero :: ts)
    | Zero :: ts ->
            let (Node (_, t1, t2), ts') = unconsTree ts
            (t1, One t2 :: ts')

let head ts =
    let (Leaf x, _) = unconsTree ts
    x

let tail ts =
    let (_, ts') = unconsTree ts
    ts'

let rec lookupTree t =
    match t with
    | (0, Leaf x) -> x
    | (i, Node(w, t1, t2)) ->
        if i < w / 2 then lookupTree (i, t1)
        else lookupTree (i - (w / 2), t2)

let rec lookup i t =
    match (i, t) with
    | (i,  Zero :: ts) -> lookup i ts
    | (i, One t :: ts) ->
        if i < size t then lookupTree (i, t) else lookup (i - size t) ts

let rec updateTree i y t =
    match (i, t) with
    | (0, Leaf x) -> Leaf y
    | (i, Node(w, t1, t2)) ->
        if i < w / 2 then Node (w, updateTree i y t1, t2)
        else Node (w, t1, updateTree (i - w / 2) y t2)

let rec update i y t =
    match t with
    | Zero :: ts -> Zero :: update i y ts
    | One t :: ts ->
        if i < size t then One (updateTree i y t) :: ts
        else One t :: update (i - size t) y ts

// Oppgave 9.1
let rec dropTree n t xs = 
    match t with
    | Leaf x -> xs
    | Node (i, t1, t2) -> 
        if n <= i / 2 then dropTree n t1 (One t2::xs) // splitter i venstre tre, høyre tre er komplett av rank 2^(i -1)
        else 
            // splitter videre på høyre side, t1 forsvinner så erstatter med Zero
            match xs with
            | [] ->  dropTree (n - i / 2) t2 []
            | _ -> dropTree (n - i / 2) t2 (Zero :: xs)


let rec drop n l =
    match l with
    | Zero :: xs -> drop n xs
    | [ One t] -> dropTree n t []
    | One t :: xs ->
        let i = size t
        if n > i
        then drop (n - i) xs
        else dropTree i t (Zero :: xs) 
        // hvis lista ikke er tom må nåværende plass erstattes med Zero
        // dropTree fører til at det legges til trær av lavere rank 

(*
oppgave 2.5

let rec complete x d = 
    match d with
    | 0 -> E
    | n -> let subtree = complete x (n - 1)
                in T(subtree, x, subtree)

let create2 x m =
    let vt = complete x m
    let ht = T(vt, x, vt)
    T (vt, x, ht)

let rec create x m =
    match m with
    | 0 -> E
    | m ->
        let half = m / 2
        if m % 2 = 0 then
            let t = complete x half in T(t, x, t)
        else create2 x half

*)
let create n a = 
    let rec aux n t = 
        match n with
        | 0 -> []
        | _  ->
            if n % 2 = 1 then One t:: (aux (n / 2) (Node (2 * size t, t, t)))
            else Zero :: (aux (n / 2) (Node (2 * size t, t, t)))
    aux n (Leaf a)
