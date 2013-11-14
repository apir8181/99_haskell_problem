import Data.List as L

isPrime :: Int -> Bool
isPrime n
  | n < 0 = isPrime . negate $ n
  | n == 0 || n == 1 = False
  | otherwise = not $ any (\x -> n `mod` x == 0) [2..end]
                where end = truncate . sqrt . fromIntegral $ n

myGCD :: Int -> Int -> Int
myGCD a b
  | a < 0 = myGCD (negate a) b
  | b < 0 = myGCD a (negate b)
  | a == 0 && b == 0 = error "no gcd for 0, 0"
  | a == 0 = myGCD b a
  | b == 0 = a
  | otherwise = myGCD b (a `mod` b)

coprime :: Int -> Int -> Bool
coprime a b
  | a <= 0 || b <= 0 = error "coprime a b, a and b should greater than 0"
  | otherwise = gcd a b == 1

totient :: Int -> Int
totient a
  | a <= 0 = error "totient a, a should be greater than 0"
  | otherwise = foldr (\x acc -> if x == True then acc + 1 else acc)
                0 (map (coprime a) [1..(a - 1)])

primeFactors :: Int -> [Int]
primeFactors a
  | a <= 0 = error "error"
  | otherwise = let f = leastFactor a in if f == a then [a]
                                         else f : primeFactors (a `quot` f)
                where leastFactor n = leastFactorHelper n
                                      (leastFactorList n)
                      leastFactorHelper n (x:xs) = 
                        if n `mod` x == 0 then x
                        else leastFactorHelper n xs
                      leastFactorList n = [2..(truncate . sqrt .
                                               fromIntegral $ n)] ++ [n]
    
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult a
  | a <= 0 = error "error"
  | otherwise = map (\xs -> (head xs, length xs)) $ L.group $
                  primeFactors a

phi :: Int -> Int
phi a
  | a <= 0 = error "error"
  | otherwise = foldr (\x acc -> let p = fst x; m = snd x in
                        (p-1) * p^(m-1) * acc) 1 $ primeFactorsMult a

primesR :: Int -> Int -> [Int]
primesR a b
  | a <= 0 || b <= 0 = error "error"
  | otherwise = foldr (\x acc -> if isPrime x then x:acc else acc) [] [a..b]

goldbachPartition :: Int -> (Int, Int)
goldbachPartition n
  | n < 2 = error "error"
  | odd n = error "goldbachPartition require even number"
  | otherwise = let xs = 1:primesR 2 n in helperfunc n xs
  where helperfunc n (x:xs) = if isPrime (n - x) || n - x == 1
                              then (x, n - x)
                              else helperfunc n xs


goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b
  | a <= 0 || b <= 0 = error "error"
  | otherwise = map goldbachPartition $ filter even [a..b]
