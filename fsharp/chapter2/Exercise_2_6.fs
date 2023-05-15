module Chapter2.Exercise_2_6

type MElem<'k, 'v> when 'k : comparison = 'k * 'v

type MTree<'k, 'v> when 'k : comparison  = 
    | ME 
    | MT of MTree<'k, 'v> * MElem<'k, 'v> * MTree<'k, 'v>

module FiniteMap = 
    let empty = ME
    let rec bind k v (mt: MTree<'k, 'v>) =
        match mt with
        | ME -> MT(ME, (k, v), ME)
        | MT (a, (x, y), b) ->
            if k < x then MT (bind k v a, (x, y), b)
            else if k > x then MT (a, (x, y), bind k v b)
            else MT (a, (k, v), b)

    let rec lookup k (mt: MTree<'k, 'v>) =
        match mt with
        | ME -> failwith "No such key"
        | MT(a, (x, y), b) -> 
            if k < x then  lookup k a
            else if k > x then lookup k b
            else y