module Chapter2.Stacks

    type 'a Stack = Stack of 'a list
    
    module Stack =
        let empty = Stack []
        let isEmpty (Stack l) = List.isEmpty l
        let cons (x: 'a) (Stack s) = Stack (x :: s)
        let head (Stack l) = List.head l
        let tail (Stack l) = List.tail l |> Stack

    type 'a CustomStack = Nil | Cons of 'a * 'a CustomStack 
    module CustomStack = 
        let empty  = Nil
        let isEmpty = function Nil -> true | _ -> false
        let cons x s = Cons (x, s)
        let head = function 
            | Nil -> failwith "Emtpy" 
            | Cons (x, s) -> x
        let tail = function
            | Nil -> failwith "Empty"
            | Cons (x, s) -> s
        
        let rec add s1 s2 = 
            match s1 with
            | Nil -> s2
            | Cons (x, xs) -> Cons (x, add xs s2)
        
        let rec update (stack: 'a CustomStack) i y = 
            match stack, i with
            | Nil, _ -> failwith "Error"
            | Cons (x, xs), 0 -> Cons(y, xs)
            | Cons (x, xs), i -> Cons(x, update xs (i - 1) y) 