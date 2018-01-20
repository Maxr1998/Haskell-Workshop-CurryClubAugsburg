module Tree where

data Tree a = Null | Fork a (Tree a) (Tree a)
    deriving (Show,Eq)

tree1 = Fork 3 (Fork 1 Null (Fork 2 Null Null)) (Fork 4 Null Null)
tree2 = Fork 1 Null (Fork 2 Null (Fork 3 Null (Fork 4 Null (Fork 5 Null Null))))


{- 20a -}
numberOfLeaves Null = 0
numberOfLeaves (Fork a Null Null) = 1
numberOfLeaves (Fork x l r) = numberOfLeaves l + numberOfLeaves r

{- 20b -}
maxTreeHeight Null = 0
maxTreeHeight (Fork x l r) = 1 + max (maxTreeHeight l) (maxTreeHeight r)

{- 20c -}
treeInOrder Null = []
treeInOrder (Fork x l r) = treeInOrder l ++ [x] ++ treeInOrder r

treePreOrder Null = []
treePreOrder (Fork x l r) = [x] ++ treeInOrder l ++ treeInOrder r

treePostOrder Null = []
treePostOrder (Fork x l r) = treeInOrder l ++ treeInOrder r ++ [x]

