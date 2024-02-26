module Chapter9.ZerolessRandomAccessList

type ZDigit = ZOne | ZTwo
type ZNat  = ZDigit list

let rec inc n =
    match n with
    | [] -> [ZOne]
    | ZOne :: xs -> ZTwo :: xs
    | ZTwo :: xs -> ZOne :: inc xs

let rec dec n =
    match n with
    | [ZOne] -> []
    //| [ZTwo] -> [ZOne]
    | ZOne :: xs -> ZTwo :: dec xs
    | ZTwo :: xs -> ZOne :: xs

let rec add m n =
    match (m, n) with
    | [], n -> n
    | m, [] -> m
    | ZOne :: xs, ZOne :: ys -> ZTwo :: add xs ys
    | ZTwo :: xs, ZTwo :: ys -> ZTwo :: inc (add xs ys)
    | x :: xs, y :: ys -> ZOne :: inc (add xs ys) // One , Two eller Two, One = 3

type 'a ZTree = ZLeaf of 'a | ZNode of int * 'a ZTree * 'a ZTree
type 'a ZLDigit = ZLOne of 'a ZTree | ZLTwo of 'a ZTree * 'a ZTree
type 'a ZList = 'a ZLDigit list

let size t =
    match t with
    | ZLeaf _ -> 1
    | ZNode (w, _, _) -> w

let link t1 t2 = ZNode (size t1 + size t2, t1, t2)
    
let rec consTree t ts =
    match ts with
    | [] -> [ZLOne t]
    | ZLOne t' :: ts -> ZLTwo (t, t') :: ts
    | ZLTwo (t', t'') :: ts -> ZLOne t :: consTree (link t' t'') ts
    
let cons x ts = consTree (ZLeaf x) ts

let rec unconsTree l =
    match l with
    | [ ZLOne t ] -> (t, [])
    | ZLOne t :: ts ->
            let (ZNode (_, t1, t2), ts') = unconsTree ts
            (t, ZLTwo (t1, t2) :: ts')
    | ZLTwo (t, t') :: ts -> (t, ZLOne t' :: ts)

let head ts =
    match ts with
    | ZLOne (ZLeaf x) :: xs -> x 
    | ZLTwo (ZLeaf x, _) :: xs -> x

let tail ts =
    let (_, ts') = unconsTree ts
    ts'

let rec lookupTree t =
    match t with
    | (0, ZLeaf x) -> x
    | (i, ZNode(w, t1, t2)) ->
        if i < w / 2 then lookupTree (i, t1)
        else lookupTree (i - (w / 2), t2)

let rec lookup i t =
    match (i, t) with
    | (i,  ZLTwo (t1, t2) :: ts) ->
        if i < size t1 then lookupTree (i, t1)
        else if i < size t1 + size t2 then lookupTree (i - size t1, t2)
        else lookup (i - size t1 - size t2) ts 
    | (i, ZLOne t :: ts) ->
        if i < size t then lookupTree (i, t) else lookup (i - size t) ts

let rec updateTree i y t =
    match (i, t) with
    | (0, ZLeaf x) -> ZLeaf y
    | (i, ZNode(w, t1, t2)) ->
        if i < w / 2 then ZNode (w, updateTree i y t1, t2)
        else ZNode (w, t1, updateTree (i - w / 2) y t2)

let rec update i y t =
    match t with
    | ZLTwo (t1, t2) :: ts ->
        if i < size t1 then ZLTwo ((updateTree i y t1), t2) :: ts 
        else if i < size t1 + size t2 then ZLTwo(t1, updateTree (i - size t1) y t2) :: ts
        else ZLTwo (t1, t2) :: update (i - size t1 - size t2) y ts 
    | ZLOne t :: ts ->
        if i < size t then ZLOne (updateTree i y t) :: ts
        else ZLOne t :: update (i - size t) y ts
