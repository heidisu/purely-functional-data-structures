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

(*
let rec drop n l =
    match l with
    | Zero :: xs -> drop n xs
    | One t :: xs ->
        match t with
        | Leaf x -> drop (n - 1) xs
        | Node (i, t1, t2) ->
            if i < n then drop (n - i) xs
            else if i = n then xs
            else
 *)