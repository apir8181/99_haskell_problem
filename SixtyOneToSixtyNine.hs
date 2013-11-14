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
        helper (Branch e Empty Empty) pos h margins = Branch (e, pos)
                                                      Empty Empty
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


-- 66

-- 67
stringToTree :: String -> Tree Char
stringToTree "" = Empty
stringToTree s
  | length s == 1 = Branch (s!!0) Empty Empty
  | otherwise = let c = s !! 0;
                    subString = init . tail . tail $ s;
                    (_, _, idx) = foldl'
                                  (\(num, idx, res) x->
                                    case x of '(' -> (num+1, idx+1, res)
                                              ')' -> (num-1, idx+1, res)
                                              ',' -> if num == 0
                                                     then (num, idx+1, idx)
                                                     else (num, idx+1, res)
                                              c -> (num, idx+1,res)
                                                          
                                  ) (0, 0, -1) subString
                    lString = take idx subString;
                    rString = drop (idx+1) subString
                in Branch c (stringToTree lString) (stringToTree rString)
-- 68
treeToPreorder :: Tree a -> [a]
treeToPreorder Empty = []
treeToPreorder (Branch x lch rch) = [x] ++ treeToPreorder lch ++
                                    treeToPreorder rch
treeToInorder :: Tree a -> [a]
treeToInorder Empty = []
treeToInorder (Branch x lch rch) = treeToInorder lch ++ [x] ++
                                   treeToInorder rch

-- take [a, a + b] in the List
subList :: [a] -> Int -> Int -> [a]
subList xs a b = take b (drop a xs)

preInTree :: (Ord a) => [a] -> [a] -> Tree a
preInTree [] [] = Empty
preInTree pre ino = let c = pre !! 0;
                        maybeIdx = elemIndex c ino;
                        inoIdx = case maybeIdx of Just x -> x
                                                  Nothing -> error ""
                        lino = take inoIdx ino;
                        rino = drop (inoIdx+1) ino;
                        lpre = subList pre 1 (length lino);
                        rpre = drop (length lino + 1) pre
                    in Branch c (preInTree lpre lino) (preInTree rpre rino)

-- problem 69
ds2Func :: [Tree Char] -> Char -> [Tree Char]
ds2Func stack c = if c == '.'
                  then let now = head stack
                           newHead = case now of Branch x Empty Empty -> Branch x (Branch '.' Empty Empty) Empty
                                                 Branch x lch Empty -> Branch x lch (Branch '.' Empty Empty)
                           newStack = newHead : (tail stack)
                       in handleNewStack newStack
                  else let newStack = (Branch c Empty Empty) : stack
                       in newStack
  where handleNewStack stack = if length stack == 1
                               then stack
                               else let fstElem = head stack
                                        sndElem = stack !! 1
                                    in case fstElem of Branch x lch Empty -> stack
                                                       Branch x lch rch -> case sndElem of Branch x Empty Empty -> let newElem = Branch x fstElem Empty
                                                                                                                   in newElem : (drop 2 stack)
                                                                                           Branch x lch Empty -> let newElem = Branch x lch fstElem
                                                                                                                     newStack = newElem : (drop 2 stack)
                                                                                                                 in handleNewStack newStack

dTree2Tree :: Tree Char -> Tree Char
dTree2Tree Empty = Empty
dTree2Tree (Branch '.' Empty Empty) = Empty
dTree2Tree (Branch x lch rch) = Branch x (dTree2Tree lch) (dTree2Tree rch)

ds2Tree :: String -> Tree Char
ds2Tree "" = Empty
ds2Tree str = dTree2Tree $ head $ foldl' (\stack x -> ds2Func stack x) [] str

tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch x lch rch) = [x] ++ (tree2ds lch) ++ (tree2ds rch)
                                         
