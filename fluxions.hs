import Data.Text (replace, unpack, pack)

newtype Fluxion n = F ([n], Int)
-- F ([a₀,a₁,...,aₖ],n) represents the fluxion (a₀ε⁰+a₁ε¹+...+aₖεᵏ)ωⁿ
-- this is able to represent all possible finitely long fluxions
-- see fluxions.txt for more information on fluxions

instance (Show n, Ord n, Num n) => Show (Fluxion n) where
    show (F ([],n)) = "0"
    show (F (xs,n)) = (tail
                        . tail
                        . tail
                        . unpack
                        . replace (pack "  ") (pack " 1 ")
                        . replace (pack " 1") (pack " ")
                        . replace (pack "^1") (pack "")
                        . replace (pack "ε^-") (pack "∞^")
                        . replace (pack "ε^0") (pack "")
                        . pack)
                        $ concatMap (\(x,i) ->
                        if x == 0 then ""
                        else (if x < 0 then " - " ++ show (-x) ++ "ε^" ++ show i
                        else " + " ++ show x ++ "ε^" ++ show i)) (zip xs [-n..])

instance (Eq n, Num n) => Eq (Fluxion n) where
    F (xs,n) == F (ys,m) = uncurry (==) (doubleNormalise (xs,n) (ys,m))

instance Num n => Num (Fluxion n) where
    F (xs,n) + F (ys,m) | n == m = F (losslessZipWith (+) xs ys,n)
                        | n < m = F (0:xs,n+1) + F (ys,m)
                        | n > m = F (xs,n) + F (0:ys,m+1)
    F (xs,n) - F (ys,m) | n == m = F (losslessZipWith (-) xs ys,n)
                        | n < m = F (0:xs,n+1) - F (ys,m)
                        | n > m = F (xs,n) - F (0:ys,m+1)
    F (xs,n) * F (ys,m) | n == m = F ((matrixFold (+) . cartProduct (*) . separate . losslessZipWith (,) xs) ys,2*n)
                        | n < m = F (0:xs,n+1) * F (ys,m)
                        | n > m = F (xs,n) * F (0:ys,m+1)
    abs (F (xs,n)) = F (xs,0)
    signum (F (xs,n)) = F ([1],n)
    fromInteger n = F ([fromInteger n],0)

instance (Ord n, Num n) => Ord (Fluxion n) where
    compare x y = if leq x y then LT else GT

---

losslessZipWith :: (Num a, Num b) => (a -> b -> c) -> [a] -> [b] -> [c]
losslessZipWith f xs ys = zipWith f (xs ++ replicate (length ys - length xs) 0) (ys ++ replicate (length xs - length ys) 0)

-- takes a matrix and folds across the diagonals
matrixFold :: Num a => (a -> a -> a) -> [[a]] -> [a]
matrixFold _ [] = []
matrixFold _ [xs] = xs
matrixFold f ((x:xs):(ys:xss)) = x : matrixFold f (losslessZipWith f xs ys : xss)

cartProduct :: (a -> a -> b) -> ([a],[a]) -> [[b]]
cartProduct f (xs,ys) = map (flip map ys . f) xs

separate :: [(a,b)] -> ([a],[b])
separate = foldr (\(x,y) (xs,ys) -> (x:xs,y:ys)) ([],[])

---

fluxionLEq :: Fluxion Int -> Fluxion Int -> Bool
fluxionLEq (F ([],n)) (F ([],m)) = True
fluxionLEq (F ([],n)) (F (y:ys,m))
    | y /= 0 = y > 0
    | otherwise = fluxionLEq (F ([],0)) (F (ys,m-1))
fluxionLEq (F (x:xs,n)) (F ([],m))
    | x /= 0 = x < 0
    | otherwise = fluxionLEq (F (xs,n-1)) (F ([],0))
fluxionLEq (F (x:xs,n)) (F (y:ys,m))
    | x == 0 = fluxionLEq (F (xs,n-1)) (F (y:ys,m))
    | y == 0 = fluxionLEq (F (x:xs,n)) (F (ys,m-1))
    | n < m = y > 0
    | n > m = x < 0
    | n == m = case () of
       () | x < y -> True
          | x > y -> False
          | otherwise -> fluxionLEq (F (xs,n-1)) (F (ys,m-1))

-- equivalent to fluxionLEq written by cosmo bobak (https://github.com/cosmobobak):

-- takes two fluxions to representations with the same "n" and equal-length lists
doubleNormalise :: Num n => ([n], Int) -> ([n], Int) -> (([n], Int), ([n], Int))
doubleNormalise ([], n) ([], m) = doubleNormalise ([0], n) ([0], m)
doubleNormalise r@(xs, n) l@(ys, m)
  -- if the offsets are the same, we just need to pad the shorter list with zeros
  | n == m = ((xs ++ replicate xsMakeUp 0, n), (ys ++ replicate ysMakeUp 0, m))
  -- if the left fluxion has a smaller offset, we need to left-pad it with zeros
  | n < m = doubleNormalise (replicate (m - n) 0 ++ xs, m) l
  -- if the right fluxion has a smaller offset, we need to left-pad it with zeros
  | otherwise = doubleNormalise r (replicate (n - m) 0 ++ ys, n)
  where
    xsMakeUp = max (length ys - length xs) 0
    ysMakeUp = max (length xs - length ys) 0

leq :: (Ord n, Num n) => Fluxion n -> Fluxion n -> Bool
leq (F x) (F y) = uncurry (\(xs, _) (ys, _) -> xs <= ys) (doubleNormalise x y)

prop_leq :: Fluxion Int -> Fluxion Int -> Bool
prop_leq x y = leq x y == fluxionLEq x y

--- 

raise :: (b -> b -> c) -> (a -> b) -> (a -> b) -> a -> c
raise op f g x = f x `op` g x

{-
-- characteristic function of a fluxion
p :: Fluxion Int -> Fluxion Int -> Fluxion a
p (F (zs,n)) = raise (*) (^ n) (`p'` zs)
    where
        p' x (n:ns) = F ([n],0) + (F ([p' x ns],0)) / (F ([x],0))
-}

d :: Num n => (Fluxion n -> Fluxion n) -> (n -> Fluxion n)
d f x = f (F ([x],0) + F ([0,1],0)) - f (F ([x],0))

lim :: (Num n,Eq n) => Fluxion n -> Maybe n
lim (F ([],n)) = Just 0
lim (F (x:xs,0)) = Just x
lim (F (0:xs,n)) = lim (F (xs,n-1))
lim (F (xs,n)) | n < 0 = Just 0
               | otherwise = Nothing