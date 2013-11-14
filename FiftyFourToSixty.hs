import Control.Monad (liftM2)
import Data.List

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

cbalTree :: Int -> [Tree Char]
cbalTree n
  | n == 0 = [Empty]
  | n == 1 = [Branch 'x' Empty Empty]
  | otherwise = let num1 = (n - 1) `quot` 2
                    num2 = (n - 1) - num1
                    leftTrees = cbalTree num1
                    rightTrees = cbalTree num2
                    chdTrees = liftM2 (,) leftTrees rightTrees
                in map (\chd -> Branch 'x' (fst chd) (snd chd)) chdTrees ++
                   map (\chd -> Branch 'x' (snd chd) (fst chd)) chdTrees

mirrorTree :: Tree a -> Tree Char
mirrorTree Empty = Empty
mirrorTree (Branch x lTree rTree) = Branch 'x' (mirrorTree rTree)
                                    (mirrorTree lTree)

sameTree :: Tree a -> Tree Char
sameTree Empty = Empty
sameTree (Branch x lch rch) = Branch 'x' (sameTree lch) (sameTree rch)

symmetricTree :: (Eq a) => Tree a -> Bool
symmetricTree Empty = True
symmetricTree (Branch x lTree rTree) = (mirrorTree lTree) ==
                                       (sameTree rTree)

addBST :: (Ord a) => a -> Tree a -> Tree a
addBST x Empty = Branch x Empty Empty
addBST x (Branch now lchd rchd) = if x < now
                                  then Branch now (addBST x lchd) rchd
                                  else Branch now lchd (addBST x rchd)

constructBST :: (Ord a) => [a] -> Tree a
constructBST [] = Empty
constructBST (x:xs) = foldl' (\tree x -> addBST x tree)
                      (Branch x Empty Empty)
                      xs

symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = filter symmetricTree $ cbalTree n


treeHeight :: Tree a -> Integer
treeHeight Empty = 0
treeHeight (Branch x lch rch) = let lheight = treeHeight lch
                                    rheight = treeHeight rch
                                in if lheight > rheight
                                   then lheight + 1
                                   else rheight + 1

hbalTree :: a -> Integer -> [Tree a]
hbalTree x height
  | height < 0 = []
  | height == 0  = [Empty]
  | height == 1 = [Branch x Empty Empty]
  | otherwise = let min1Tree = hbalTree x (height - 1)
                    min2Tree = hbalTree x (height - 2)
                    chdPairs = liftM2 (,) min1Tree min1Tree ++
                               liftM2 (,) min1Tree min2Tree ++
                               liftM2 (,) min2Tree min1Tree
                in map (\pair -> Branch x (fst pair) (snd pair)) chdPairs
                       
                                  
hbalTreeMaxNode :: Integer -> Integer
hbalTreeMaxNode x = 2 ^ x - 1

hbalTreeMinNode :: Integer -> Integer
hbalTreeMinNode h
  | h == 0 = 0
  | h == 1 = 1
  | otherwise = hbalTreeMinNode (h - 1) + hbalTreeMinNode (h - 2) + 1

treeNodesNum :: Tree a -> Integer
treeNodesNum Empty = 0
treeNodesNum (Branch x Empty Empty) = 1
treeNodesNum (Branch x lch rch) = treeNodesNum lch + treeNodesNum rch + 1

hbalTreeNodes :: a -> Integer -> [Tree a]
hbalTreeNodes x n
  | n < 0 = []
  | n == 0 = [Empty]
  | n == 1 = [Branch x Empty Empty]
  | otherwise = let hs = (flip filter) [1..n] (\i ->
                                               hbalTreeMinNode i <= n &&
                                               n <= hbalTreeMaxNode i)
                in concat $ (flip map) hs (\h -> let trees = hbalTree x h
                                                 in (flip filter) trees
                                                    (\tree ->
                                                      treeNodesNum tree == n
                                                    )
                                          )
                   
