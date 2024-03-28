{-

primes :: Int -> [Int]
primes n = take n (p n)

p :: Int -> [Int]
p 0 = [2..]
p n = take (n) (p (n-1)) ++ f x (xs)
    where (x:xs) = drop (n-1) (p (n-1))

nthp :: Int -> Int
nthp n = p n !! n

f :: Int -> [Int] -> [Int]
f n [] = []
f 2 xs = g 2 xs [2] -- sum = 1*2
f 3 xs = g 3 xs [3] -- sum = 1*3
f 5 xs = g 5 xs [7,3] -- sum = 2*5
f 7 xs = g 7 xs [12,7,4,7,4,7,12,3] -- sum = 8*7
f n xs = take (n-1) xs ++ f n (drop n xs)

-- [2,3,4,5,6,7,8,9,10,11,12,13,14,15]
-- [2] [3,5,7,9,11,13,15]
-- [2,3] [5,7,11,13]

g :: Int -> [Int] -> [Int] -> [Int]
g n xs [] = f n xs
g n xs (a0:as) = take (a0-1) xs ++ g n (drop a0 xs) as

prop_g7 :: Int -> Bool
prop_g7 n = and $ take n $ zipWith (==) (g 7 [12] [6..]) (f 7 [6..])

-}

data MaybeInts = P [Maybe Int]

instance Show MaybeInts where
    show (P xs) = show (cut xs)

cut :: [Maybe Int] -> [Int]
cut [] = []
cut (Just x:xs) = x : cut xs
cut (Nothing:xs) = cut xs

primes' :: Int -> MaybeInts
primes' n = P (aux n n)

aux :: Int -> Int -> [Maybe Int]
aux n m | length (cut (take n (p' m))) < m = (aux (n+1) m)
        | otherwise = (take n (p' m))

p' :: Int -> [Maybe Int]
p' 0 = map may [2..]
p' n = f' n (map may [2..])

f' :: Int -> [Maybe Int] -> [Maybe Int]
f' n [] = []
f' n xs = f' (n-1) (take (n-1) xs ++ [Nothing] ++ f' n (drop n xs))

may :: Int -> Maybe Int
may n = Just n