import Control.Monad
import Data.List
import Data.Maybe

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Eq, Show)

countLeaves :: Tree a -> Integer
countLeaves Empty = 0
countLeaves (Branch x Empty Empty) = 1
countLeaves (Branch x lch rch) = countLeaves lch + countLeaves rch

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch x lch rch) = leaves lch ++ leaves rch

internals :: Tree a -> [a]
internals Empty = []
internals (Branch x Empty Empty) = []
internals (Branch x lch rch) = [x] ++ internals lch ++ internals rch

atLevel :: Tree a -> Integer -> [a]
atLevel Empty _ = []
atLevel (Branch x lch rch) n = if n == 1 then [x]
                               else atLevel lch (n - 1) ++
                                    atLevel rch (n - 1)

completeBinaryTree :: a -> Int -> Tree a
completeBinaryTree x n
  | n == 0 = Empty
  | n == 1 = (Branch x Empty Empty)
  | otherwise = let lch = if odd (n - 1) then (n - 1) `quot` 2 + 1
                          else (n - 1) `quot` 2;
                    rch = n - 1 - lch
                in Branch x (completeBinaryTree x lch)
                   (completeBinaryTree x rch)
                   
isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree Empty = True
isCompleteBinaryTree tree = let idxs = helper 1 tree;
                                cbtIdxs = [1..length idxs]
                            in all (\idx -> idx `elem` cbtIdxs) idxs
  where helper idx Empty = []
        helper idx (Branch x lch rch) = [idx] ++
                                        helper (idx*2) lch ++
                                        helper (idx*2+1) rch

-- 64
data LayoutPos = LayoutPos { x::Integer, y::Integer }
instance Show LayoutPos where
  show pos = show (x pos, y pos)

type IndexTree a = (Integer, Tree a)
inorderIndexTree :: Tree a -> [IndexTree a]
inorderIndexTree Empty = []
inorderIndexTree tree = helper 1 tree
  where helper idx Empty = []
        helper idx tree@(Branch x lch rch) = helper (idx*2) lch ++
                                             [(idx, tree)] ++
                                             helper (idx*2 + 1) rch
indexY :: Integer -> Integer
indexY idx = if idx == 1 then 1
                  else indexY (idx `quot` 2) + 1

indexX :: Integer -> [IndexTree a] -> Integer
indexX idx infixList = let maybeX = findIndex (\iTree -> fst iTree == idx)
                                    infixList
                       in case maybeX of Just x -> toInteger x

layout :: (Eq a) => Tree a -> Tree (a, LayoutPos)
layout tree = let infixList = inorderIndexTree tree
              in helper infixList 1 tree
  where helper infixList idx Empty = Empty
        helper infixList idx tree@(Branch x lch rch) =
          (Branch (x, LayoutPos (indexX idx infixList + 1) (indexY idx))
           (helper infixList (idx*2) lch)
           (helper infixList (idx*2+1) rch) )

tree64 :: Tree Char
tree64 = Branch 'n'
         (Branch 'k'
          (Branch 'c'
           (Branch 'a' Empty Empty)
           (Branch 'h'
            (Branch 'g'
             (Branch 'e' Empty Empty)
             Empty
            )
            Empty
           )
          )
          (Branch 'm' Empty Empty)
         )
         (Branch 'u'
          (Branch 'p'
           Empty
           (Branch 's'
            (Branch 'q' Empty Empty)
            Empty
           )
          )
          Empty
         )

-- 65
treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Branch x lch rch) = 1 + max (treeHeight lch) (treeHeight rch)

treeLeftMostInfo :: Tree a -> (Int, Int)
treeLeftMostInfo tree = helper tree 1 1
  where helper (Branch x Empty _) idx h = (idx, h)
        helper (Branch x lch rch) idx h = helper lch (idx*2) (h+1)

layoutWide :: Tree a -> Tree (a, LayoutPos)
layoutWide Empty = Empty
layoutWide tree = let height = treeHeight tree;
                      (lidx, ldepth) = treeLeftMostInfo tree;
                      margins = reverse $ take height [2^x|x<-[1..height]];
                      (rootx, rooty) = (foldr (\x acc -> acc + x `quot` 2)
                                        1 (tail $ take ldepth margins),
                                        1)
                  in helper tree (LayoutPos rootx rooty) 1 margins
  where helper Empty pos h margins = Empty
        helper tree@(Branch e lch rch) pos@(LayoutPos x y) h margins = let margin = margins !! h
                                                       in Branch (e, pos)
                                                          (helper lch (LayoutPos (x - margin `quot` 2) (y + 1)) (h+1) margins)
                                                          (helper rch (LayoutPos (x + margin `quot` 2) (y + 1)) (h+1) margins)

tree65 :: Tree Char
tree65 = Branch 'n'
         (Branch 'k'
          (Branch 'c'
           (Branch 'a' Empty Empty)
           (Branch 'e'
            (Branch 'd' Empty Empty)
            (Branch 'g' Empty Empty)
           )
          )
          (Branch 'm' Empty Empty)
         )
         (Branch 'u'
          (Branch 'p'
           Empty
           (Branch 'q' Empty Empty)
          )
          Empty
         )                                                          
