module Chapter10.TrieOfTrees

type 'a Tree = E | T of 'a * 'a Tree * 'a Tree
type 'a TMap = Trie of 'a EM Option * Map<char, 'a TMap>
and 'a EM = Elem of 'a | Mapp of 'a TMap

let empty = Trie(None, Map.empty)

(*
fun lookupMap (E, TRIE (NONE, m)) = raise NOTFOUND
    | lookupMap (E, TRIE (SOME (MAP x), m)) = x
    | lookupMap (T (k, a, b), TRIE (v, m)) =
    lookupMap (a, M.lookup (k, v))*)
let rec lookupMap t tr =
    match (t, tr) with
    | (E, Trie(None, m)) -> failwith "not found"
    | (E, Trie(Some (Mapp x), m)) -> x
    | (T(k, a, b), Trie(v, m)) -> lookupMap a (Map.find k m)

let rec lookup t tr = 
    match (t, tr) with
    | (E, Trie(None, _)) -> failwith "Not found"
    | (E, Trie(Some (Elem x), _)) -> x
    | (T(k, a, b), Trie(v, m)) ->
        lookup b (lookupMap a (Map.find k m))

let rec bindMap t x tr = 
    match (t, tr) with
    | (E, Trie(_, m)) -> Trie(Some(Mapp x), m)
    | (T(k, a, b), Trie(v, m)) ->
        let tt = Map.find k m
        let t = lookupMap a tt 
        let t' = bindMap b x t
        let tt' = bindMap a t' tt
        Trie(v, Map.add k tt' m)
        


let rec bind t x tr = 
    match (t, tr) with
    | (E, Trie(_, m)) -> Trie(Some (Elem x), m)
    | (T(k, a, b), Trie(v, m)) ->
        let tt = Map.find k m
        let t = lookupMap a tt 
        let t' = bind b x t
        let tt' = bindMap a t' tt
        Trie(v, Map.add k tt' m)
