open Chapter2.Stacks
open Chapter2.Trees
// For more information see https://aka.ms/fsharp-console-apps

let x = Cons(1, (Cons (2, (Cons (3, CustomStack.empty)))))
let y = Cons(4, (Cons (5, (Cons (6, CustomStack.empty)))))


printfn "%A" (CustomStack.add x y)

let z = CustomStack.update x 2 7 
printfn "%A" z

let lst = [1; 2; 3; 4]
printfn "%A" <| Oppg_2_1.suffix lst

let subtree1 = T (T (E, 'a', E) ,'b', T(E, 'c', E))
let subtree2 = T (T (E, 'f', E) ,'g', T(E, 'h', E))

let tree : char Tree = T (subtree1, 'd', subtree2)

printfn "%A" (Tree.memb 'r' tree)
printfn "%A" (Tree.memb 'g' tree)

let t2 = Tree.insert 'e' tree
printfn "%A" t2

let dtree = Tree.oppg_2_5_a 'x' 3 
printfn "%A" dtree