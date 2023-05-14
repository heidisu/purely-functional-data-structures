module Chapter2.Trees

type Elem<'a when 'a : comparison> = 'a

type Tree<'a when 'a : comparison> = 
    | E 
    | T of Tree<'a> * Elem<'a> * Tree<'a>

module Tree = 
    let rec memb x t = 
        match t with
        | E -> false
        | T (a, y, b) -> 
            if x < y then memb x a
            else if x > y then memb x b
            else true
    
    let rec insert x t = 
        match t with 
        | E -> T (E, x, E)
        | T (a, y, b) ->
            if x < y then T (insert x a, y, b)
            else if x > y then T(a, y, insert x b)
            else T(a, y, b)

    let rec oppg_2_2 x t = 
        match t with
        | E -> false
        | T (a, y, b) -> 
            if x < y then memb x a
            else if x > y then memb x b
            else true

    let rec oppg_2_5_a x d = 
        match d with
        | 0 -> T(E, 'x', E)
        | n -> let subtree = oppg_2_5_a x (n - 1)
                    in T(subtree, 'x', subtree)
module Oppg_2_6 = 
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
    