import OneToTen
import Data.List

-- 11.Modify the resule of problem 10 in such a way that if an
-- element has no duplicates it is simply copied into the result
-- list. Only elements with duplicates are transferred as (N E)
-- lists.
data EncodeElem a = Multiple Int a | Single a deriving(Show)
encodeModified :: (Eq a) => [a] -> [EncodeElem a]
encodeModified xs = map func xss
                    where func = (\xs ->
                                   if length xs == 1
                                   then Single $ head xs
                                   else Multiple (length xs) 
                                        (head xs)
                                 )
                          xss = pack xs

-- 12.Decode a run-length encoded modified list.
decodeModified :: (Eq a) => [EncodeElem a] -> [a]
decodeModified xs = foldl' func [] xs
  where func as b = case b of Multiple num e ->
                                as ++ replicate num e
                              Single e -> as ++ [e]

-- 13.Run-lnegth encoding of a list(direct solution)
encodeDirectHelper :: (Eq a) => a -> Int -> [a]
                      -> [EncodeElem a]
encodeDirectHelper c count []
  | count == 1 = [Single c]
  | count > 1 = [Multiple count c]
  | count == 0 = []
encodeDirectHelper c count (x:xs)
  | c == x = encodeDirectHelper c (count + 1) xs
  | otherwise = if count == 1
                then [Single c] ++ encodeDirectHelper x 1 xs
                else [Multiple count c] ++
                     encodeDirectHelper x 1 xs
       
encodeDirect :: (Eq a) => [a] -> [EncodeElem a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirectHelper x 1 xs

-- 14.Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli xs = foldr func [] xs
  where func x acc = x:x:acc

-- 15.Replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli xs n
  | n > 0 = foldr func [] xs
  | otherwise = error "Int should be greate than 0"
  where func x acc = replicate n x ++ acc

-- 16.Drop every N'th element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery xs n
  | n > 0 = dropEveryHelper 0 n xs
  | otherwise = error "Int a should be greater than 0"
  where dropEveryHelper index number xs =
          case xs of
            [] -> []
            (x:xs) ->
              if (index + 1) `mod` number == 0
              then dropEveryHelper (index + 1) number xs
              else x : dropEveryHelper (index + 1) number xs

-- 17.Split a list into two parts; the length of the first
-- part is given.
split :: [a] -> Int -> ([a], [a])
split xs n
  | n >= length xs = (xs, [])
  | otherwise = (take n xs, drop n xs)

-- 18.Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice xs i j
  | i > j || i <= 0 || i > length xs = []
  | otherwise = take (j - i + 1) (drop (i - 1) xs)

-- 19.Rotate a list N places to left.
rotate :: [a] -> Int -> [a]
rotate xs n = left ++ right
  where size = length xs
        leftRotNum = (n `mod` size + size) `mod` size
        (right, left) = split xs leftRotNum

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs
  | n > length xs || n <= 0 = error "List index out of range"
  | otherwise = (last left, init left ++ right)
  where (left, right) = split xs n
