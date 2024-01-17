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
    | [ZTwo] -> [ZOne]
    | ZOne :: xs -> ZTwo :: dec xs
    | ZTwo :: xs -> ZOne :: xs

let rec add m n =
    match (m, n) with
    | [], n -> n
    | m, [] -> m
    | ZOne :: xs, ZOne :: ys -> ZTwo :: add xs ys
    | ZTwo :: xs, ZTwo :: ys -> ZTwo :: inc (inc (add xs ys))
    | x :: xs, y :: ys -> ZTwo :: inc (add xs ys)
