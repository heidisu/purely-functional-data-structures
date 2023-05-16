open Chapter2
open Chapter2.Stacks
open Chapter2.Trees

let x = Cons(1, (Cons (2, (Cons (3, CustomStack.empty)))))
let y = Cons(4, (Cons (5, (Cons (6, CustomStack.empty)))))

printfn "%A" (CustomStack.add x y)

let z = CustomStack.update x 2 7 
printfn "%A" z

let lst = [1; 2; 3; 4]
printfn "%A" <| Exercise_2_1.suffix lst

let subtree1 = T (T (E, 'a', E) ,'b', T(E, 'c', E))
let subtree2 = T (T (E, 'f', E) ,'g', T(E, 'h', E))

let tree : char Tree = T (subtree1, 'd', subtree2)

printfn "%A" (Tree.memb 'r' tree)
printfn "%A" (Tree.memb 'g' tree)
printfn "%A" (Exercise_2_2.betterMember 'r' tree)
printfn "%A" (Exercise_2_2.betterMember 'g' tree)

let t2 = Tree.insert 'e' tree
printfn "%A" t2
let t3 = Exercise_2_3.insert 'e' tree
printfn "%A" t3
let t4 = Exercise_2_4.insert 'e' tree
printfn "%A" t4

let dtree = Exercise_2_5.complete 'x' 3 
printfn "%A" dtree

printfn "2.5 b"
let balancedTree = Exercise_2_5.create 'x' 3 
printfn "%A" balancedTree

let balancedTree2 = Exercise_2_5.create 'x' 4 
printfn "%A" balancedTree2