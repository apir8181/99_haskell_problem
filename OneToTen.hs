module OneToTen (
  pack
) where

-- 1.Find the last element of a list
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast (x:[]) = Just x
myLast (x:xs) = myLast xs

-- 2. Find the last but one element of a list
myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast (x:y:[]) = Just x
myButLast (x:xs) = myButLast xs

-- 3. Find the K'th element of a list. The first element in the
-- list is number 1.
elementAt :: [a] -> Int -> Maybe a
elementAt xs n
  | length xs < n = Nothing
  | otherwise = Just (xs !! (n - 1))

-- 4. Find the number of elements of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 5. Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 6. Find out whether a list is a palindrome.
-- A palindrome can be read forward or backward;
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:y:[]) = x == y
isPalindrome xs = if head xs == last xs
                  then isPalindrome $ init $ tail xs
                  else False

-- 7.Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))

-- 8.Elminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress $ dropWhile (==x) xs)

-- 9.Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed
-- in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack xs@(x:_) = if length dropList /= 0
                then takeList : (pack dropList)
                else [takeList]
  where takeList = takeWhile (==x) xs
        dropList = dropWhile (==x) xs

-- 10.Run-length encoding of a list
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\xs -> (length xs, head xs)) $ pack xs
