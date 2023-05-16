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
            
    