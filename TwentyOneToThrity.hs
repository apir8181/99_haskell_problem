import System.Random
import Data.List as List
import Control.Monad

-- 21.Insert an element at a given pposition into a list
insertAt :: a -> [a] -> Int -> [a]
insertAt e xs index
  | index > length xs || index <= 0 = error "Index out of range"
  | otherwise = take (index - 1) xs ++ [e] ++ drop (index - 1) xs

-- 22.Create a list containing all integers within a given range
range :: Int -> Int -> [Int]
range i j
  | i > j = []
  | otherwise = [i..j]

-- 23.Extract a given number of randomly selected elements from
-- a list.
rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect xs n = do
  gen <- getStdGen
  return [xs !! x| x <- take n $ randomRs (0, length xs - 1) gen]

-- 24.draw N different random numbers from the set 1..M
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m
  | n <= m = diffSelect' n [1..m]
  | otherwise = error "m must GT than n or EQ with n"

diffSelect' :: Int -> [Int] -> IO [Int]
diffSelect' n xs
  | n == 0 = return []
  | otherwise = do
    h <- randomRIO (0, length xs - 1)
    let remaining = take h xs ++ (drop (h + 1) xs)
    t <- diffSelect' (n - 1) remaining
    return (xs!!h:t)

-- 25.Generate a random permutation of elements of a list.
rand_permu :: String -> IO String
rand_permu = rand_permu_helper

rand_permu_helper :: String -> IO String
rand_permu_helper [] = return []
rand_permu_helper xs = do
  h <- randomRIO (0, length xs - 1)
  let remaining = take h xs ++ (drop (h + 1) xs)
  t <- rand_permu_helper remaining
  return (xs!!h:t)

type Comb a = [a]
type Combs a = [Comb a]
-- 26.Generate the combinations of K distinct objects chosen from the N
-- elements of a list.
combinations :: Int -> [a] -> Combs a
combinations n xs
   | n < 0 = error "n must be greater than 0"
   | n > length xs = error ("n must LE than length of the list or" ++
                            "EQ with the list")
   | n == length xs = [xs]
   | n == 0 = [[]]
   | otherwise = takeHeadList ++ notTakeHeadList
                 where takeHeadList = map (\ys -> head xs : ys)
                                      $ combinations (n - 1) (tail xs)
                       notTakeHeadList = combinations n $ tail xs

 -- 27(a).Group the elements of a set into disjoint subsets.
remainComb :: (Eq a) => [a] -> Comb a -> [a]
remainComb xs comb = foldr (\x acc -> if not (x `elem` comb) then x:acc
                                      else acc) [] xs
                       
myGroup :: (Eq a) => [Int] -> [a] -> [Combs a]
myGroup [] _ = []
myGroup (n:ys) xs = let combs = combinations n xs
                        combsRemain = map (remainComb xs) combs
                        pairs = zip combs combsRemain
                        results = map (\pair ->
                                        let next = (myGroup ys (snd pair))
                                        in if length next == 0
                                           then [[fst pair]]
                                           else map (fst pair:) next
                                      )
                                  pairs
                    in concat results


 -- 28(a).Sorting a list of lists according to length of sublists
lsort :: (Ord a) => [[a]] -> [[a]]
lsort [] = []
lsort xss = leftList ++ [mid] ++ rightList
  where mid = head xss
        lessList = filter (\xs -> length xs <= length mid) $ tail xss
        greaterList = filter (\xs -> length xs > length mid) $ tail xss
        leftList = if length lessList == 0 then [] else lsort lessList
        rightList = if length greaterList == 0 then []
                    else lsort greaterList

-- 28(b).Sorting a list of lists according to frequency length of sublists
lfsort :: (Ord a) => [[a]] -> [[a]]
lfsort [] = []
lfsort xss = ls ++ [mid] ++ rs
  where lenFreqs = length_freq xss
        mid = head xss
        ls = lfsort $
             filter (\xs ->
                      length_freq_search lenFreqs (length xs) <=
                      length_freq_search lenFreqs (length mid)) (tail xss)
        rs = lfsort $
             filter (\xs ->
                      length_freq_search lenFreqs (length xs) >
                      length_freq_search lenFreqs (length mid)) (tail xss)
                   

length_freq :: [[a]] -> [(Int, Int)]
length_freq [] = []
length_freq xss = foldr func [] lengthList
  where lengthList = sort . map (\xs -> length xs) $ xss
        func len [] = [(len, 1)]
        func len accs = if fst (head accs) == len
                        then (len, snd (head accs) + 1) : tail accs
                        else (len, 1) : accs

length_freq_search :: [(Int, Int)] -> Int -> Int
length_freq_search lengthFreqs len =
  foldr (\x acc -> if fst x == len then snd x else acc) 0 lengthFreqs
