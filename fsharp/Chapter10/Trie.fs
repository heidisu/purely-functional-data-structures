module Chapter10.Trie

type 'a BMap = Trie of 'a Option * Map<char, 'a BMap>

let empty = Trie(None, Map.empty)

let rec lookup word tr = 
    match (word,  tr) with
    | ([], Trie(None, _)) -> failwith "Not found"
    | ([], Trie(Some x, _)) -> x
    | (k :: ks, Trie(v, m)) -> lookup ks (Map.find k m)

let rec bind word x tr = 
    match (word, tr) with
    | ([], Trie(_, m)) -> Trie(Some x, m)
    | (k :: ks, Trie(v, m)) -> 
        let t = Map.tryFind k m |> Option.defaultValue empty
        let t' = bind ks x t
        Trie(v, Map.add k t' m)
