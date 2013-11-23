module SeventyToSeventyNine (
  MultiTree,
  getNodeNum,
  stringToMultiTree,
  multiTreeToString,
  ipl,
  bottomUp,
  multiTreeToLispString,
  lispStringToMultiTree
) where

import Data.List (intercalate)
data MultiTree a = Node a [MultiTree a] deriving (Eq, Show)

-- test data
tree1 = Node 'a' []

tree2 = Node 'a' [Node 'b' []]

tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 = Node 'a' [Node 'f' [Node 'g' []], Node 'c' [], Node 'b' [Node 'd' [], Node 'e' []]]

-- 70
getNodeNum :: MultiTree a -> Integer
getNodeNum (Node x xs) = 1 + (sum (map getNodeNum xs))

getChildren :: MultiTree a -> [MultiTree a]
getChildren (Node x xs) = xs

getNodeElem :: MultiTree a -> a
getNodeElem (Node x xs) = x

stringToMultiTree :: String -> MultiTree Char
stringToMultiTree s = parseChar s []
  where parseChar [] stack = head stack
        parseChar (x:xs) stack =
          if x == '^' 
            then if length stack >= 2
                   then let nowTree = head stack
                            lastTree = stack !! 1
                            oldStack = drop 2 stack
                            children = getChildren lastTree
                            elem = getNodeElem lastTree
                            newTree = Node elem 
                                      (children ++ [nowTree])
                        in parseChar xs (newTree:oldStack)
                   else parseChar xs stack
            else let newTree = Node x []
                 in parseChar xs (newTree:stack)

multiTreeToString :: MultiTree Char -> String
multiTreeToString (Node x xs) = 
  x : concat (map multiTreeToString xs) ++ "^"

-- 71
ipl :: MultiTree a -> Integer
ipl tree = sum (helper tree)
  where helper (Node x []) = [0]
        helper (Node x xs) = map (+1) (xs >>= helper)

-- 72
postfix :: MultiTree Char -> String
postfix (Node x []) = [x]
postfix (Node x xs) = (xs >>= postfix) ++ [x]

bottomUp :: MultiTree Char -> String
bottomUp = postfix

-- 73
multiTreeToLispString :: MultiTree Char -> String
multiTreeToLispString (Node x []) = [x]
multiTreeToLispString (Node x xs) = 
  "(" ++ [x] ++ " " ++ 
  intercalate " " (map multiTreeToLispString xs) ++ ")"

lispStringPair :: String -> [(Int, Int)]
lispStringPair str = helper interIdx
  where getSpaces acc idx num [] = acc
        getSpaces acc idx num (c:cs) = 
          case c of
            '(' -> getSpaces acc (idx+1) (num+1) cs
            ')' -> getSpaces acc (idx+1) (num-1) cs
            ' ' -> if num == 0 
                     then getSpaces (idx:acc) (idx+1) num cs
                     else getSpaces acc (idx+1) num cs
            otherwise -> getSpaces acc (idx+1) num cs
        interIdx = let spacesIdx = reverse $ getSpaces [] 0 0 str
                       separatorIdx = spacesIdx >>= 
                                      (\x -> [x-1,x+1])
                   in [0] ++ separatorIdx ++ [length str - 1]
        helper xs = case xs of [] -> []
                               (a:b:cs) -> (a,b) : helper cs
                     

subString :: String -> Int -> Int -> String
subString str sidx eidx = take (eidx-sidx+1) $ drop sidx str

lispStringToMultiTree :: String -> MultiTree Char
lispStringToMultiTree (x:[]) = Node x []
lispStringToMultiTree str =
  let subStr = subString str 3 (length str - 2)
      pairs = lispStringPair subStr
      lispSubTreeStrs = map (\(x, y) -> subString subStr x y) pairs
      rootElem = str !! 1
      children = map lispStringToMultiTree lispSubTreeStrs
  in Node rootElem children

