module MyTree where

data Tree = Leaf | Node Int Tree Tree deriving Show

{-|

-}
treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) = 
    1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

{-|
    Function treeSum sums the node values through the entire Tree
-}
treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node value leftSubtree rightSubtree) = 
    value + treeSum leftSubtree + treeSum rightSubtree  

{-|

-}
isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal = 
    let leftSorted = isSortedTree leftSubtree minVal x
        rightSorted = isSortedTree rightSubtree x maxVal
    in x >= minVal && x < maxVal && leftSorted && rightSorted

{-|
    Function addNewMax add a new max element to tree in tht rightmost Node
-}
addNewMax :: Tree -> Tree
addNewMax Leaf = Node 0 Leaf Leaf -- input tree with no node
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf) --this is the rightmost node
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2) -- intermediate node, go down right subtree

{-|
    Function treeToList converts a Tree into a List of Int
-}
treeToList :: Tree -> [Int]
treeToList Leaf = []
treeToList (Node value leftSubtree rightSubtree) = 
    value : (treeToList leftSubtree) ++ (treeToList rightSubtree)

{-|
    Function insertIntoTree sort a tree according the following rule:
    for any Node storing value x, all values in its left subtree are < x,
     and all values in its right subtree are >= x.
-}
insertIntoTree :: Int -> Tree -> Tree
insertIntoTree value Leaf = Node value Leaf Leaf
insertIntoTree value (Node nodeValue leftSubtree rightSubtree)
    | value < nodeValue = Node nodeValue (insertIntoTree value leftSubtree) rightSubtree
    | otherwise         = Node nodeValue leftSubtree (insertIntoTree value rightSubtree)

populateTree :: Tree -> [Int] -> Tree
populateTree t []       = t
populateTree t (v:vs)   = populateTree (insertIntoTree v t) vs

{-|
    Function sortTree sort a tree according the following rule:
    for any Node storing value x, all values in its left subtree are < x,
     and all values in its right subtree are >= x.
-}
sortTree :: Tree -> Tree
sortTree t = populateTree Leaf (treeToList t)

insertIntoTreeInOrder :: Int -> Tree -> Tree
insertIntoTreeInOrder v t = insertIntoTree v (sortTree t)


{-
sortTree :: Tree -> Tree
sortTree Leaf = Leaf
sortTree (Node x (Node xl t1l t1r) Leaf) = Node (min x xl) (sortTree (Node (max x xl) t1l t1r)) Leaf
sortTree (Node x Leaf (Node xr t2l t2r)) = Node (min x xr) Leaf (sortTree (Node (max x xr) t2l t2r))
sortTree (Node x (Node xl t1l t1r) (Node xr t2l t2r)) = 
    Node (min x (min xl xr)) (sortTree (Node (min x (max xl xr)) t1l t1r)) (sortTree (Node (max x (max xl xr)) t2l t2r))
-}


main :: IO()
main = do
    let t = Node 3 Leaf Leaf
    --let arvore = Node 3 (Node 5 Leaf Leaf) (Node 7 (Node 4 (Node 1 Leaf Leaf) Leaf) (Node 10 Leaf Leaf))
    let arvore = Node 2 (Node 3 (Node 5 Leaf Leaf) (Node 7 Leaf Leaf)) (Node 1 Leaf (Node 2 Leaf Leaf))
    let sorted = isSortedTree arvore minBound maxBound
    print(show(arvore))
    print(show(sorted))
    let novaArvore = sortTree (insertIntoTree 0 arvore)
    let sortedNovaArvore = isSortedTree novaArvore minBound maxBound
    print(show(novaArvore))
    print(show(sortedNovaArvore))
--    let lista = treeToList arvore
--    let depth = treeDepth arvore
--    let sum = treeSum arvore
--    let listaNova = treeToList novaArvore
--    let sorted = isSortedTree novaArvore minBound maxBound
--    let arvoreOrdenada = sortTree arvore
--    print(show(lista))
--    print(show(depth))
--    print(show(sum))

--    print(show(listaNova))
--    print(show(sorted))
--    print(show(arvoreOrdenada))