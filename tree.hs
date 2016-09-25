
data Tree = Leaf | Node Int Tree Tree deriving Show


treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) =
  1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)


treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node value leftSubtree rightSubtree) =
  value + treeSum leftSubtree +  treeSum rightSubtree


isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
  let leftSorted = isSortedTree leftSubtree minVal x
      rightSorted = isSortedTree rightSubtree x maxVal
  in x >= minVal && x < maxVal && leftSorted && rightSorted


addNewMax :: Tree -> Tree
addNewMax Leaf = Node 0 Leaf Leaf
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x + 1) Leaf Leaf)
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2)


insertValue :: Tree -> Int -> Tree
insertValue (Node value Leaf Leaf) newValue
  | newValue < value = Node value (Node newValue Leaf Leaf) Leaf
  | otherwise = Node value Leaf (Node newValue Leaf Leaf)
insertValue (Node value leftSubtree rightSubtree) newValue
  | newValue < value = Node value (insertValue leftSubtree newValue) rightSubtree
  | otherwise = Node value leftSubtree (insertValue rightSubtree newValue)


toList :: Tree -> [Int]
toList Leaf = []
toList (Node value leftSubtree rightSubtree) =
  value : (toList leftSubtree) ++ (toList rightSubtree)
