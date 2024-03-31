import Data.Text (replace, unpack, pack)
import Data.Ratio (numerator, denominator)

newtype Fluxion n = F ([n], Int)
-- F ([a₀,a₁,...,aₖ],n) represents the fluxion (a₀ε⁰+a₁ε¹+...+aₖεᵏ)ωⁿ
-- this is able to represent all possible finitely long fluxions
-- see fluxions.txt for more information on fluxions

data RationalFluxion n = (Fluxion n) :/ (Fluxion n)

instance (Eq n, Num n) => Eq (Fluxion n) where
    F (xs,n) == F (ys,m) = uncurry (==) (doubleNormalise (xs,n) (ys,m))

instance (Ord n, Num n) => Ord (Fluxion n) where
    x <= y = leq x y

instance (Show n, Ord n, Num n) => Show (Fluxion n) where
    show (F ([],n)) = "0"
    show (F (xs,n)) = (tail
                        . tail
                        . tail
                        . unpack
                        . replace (pack " .") (pack " 1.")
                        . replace (pack "  ") (pack " 1 ")
                        . replace (pack " 1") (pack " ")
                        . replace (pack "^1") (pack "")
                        . replace (pack "ε^-") (pack "∞^") -- it's not letting me print ω
                        . replace (pack "ε^0") (pack "")
                        . pack)
                        $ concatMap (\(x,i) ->
                        if x == 0 then ""
                        else (if x < 0 then " - " ++ show (-x) ++ "ε^" ++ show i
                        else " + " ++ show x ++ "ε^" ++ show i)) (zip xs [-n..])

instance (Eq n, Num n) => Num (Fluxion n) where
    F (xs,n) + F (ys,m) | n == m = F (losslessZipWith (+) xs ys,n)
                        | n < m = F (0:xs,n+1) + F (ys,m)
                        | n > m = F (xs,n) + F (0:ys,m+1)
    F (xs,n) - F (ys,m) | n == m = F (losslessZipWith (-) xs ys,n)
                        | n < m = F (0:xs,n+1) - F (ys,m)
                        | n > m = F (xs,n) - F (0:ys,m+1)
    F (xs,n) * F (ys,m) | n == m = F ((matrixFold (+)
                                    . cartProduct (*)
                                    . separate)
                                    (losslessZipWith (,) xs ys)
                                    , 2*n)
                        | n < m = F (0:xs,n+1) * F (ys,m)
                        | n > m = F (xs,n) * F (0:ys,m+1)
    abs (F (xs,n)) = F (xs,0)
    signum (F (0:xs,n)) = signum (F (xs,n-1))
    signum (F (xs,n)) = F ([1],n)
    fromInteger n = F ([fromInteger n],0)

instance (Show n, Ord n, Num n) => Show (RationalFluxion n) where
    show (x :/ y) = '(' : show x ++ ") / (" ++ show y ++ ")"

instance (Eq n, Num n) => Eq (RationalFluxion n) where
    (x :/ y) == (z :/ w) = x * w == z * y

instance (Ord n, Num n) => Ord (RationalFluxion n) where
    (x :/ y) <= (z :/ w) = (x * w - z * y) * (y * w) <= 0

instance (Eq n, Num n) => Num (RationalFluxion n) where
    (x :/ y) + (z :/ w) = (x * w + z * y) :/ (y * w)
    (x :/ y) - (z :/ w) = (x * w - z * y) :/ (y * w)
    (x :/ y) * (z :/ w) = (x * z) :/ (y * w)
    abs (x :/ y) = abs x :/ abs y
    signum (x :/ y) = signum x :/ signum y
    fromInteger n = fromInteger n :/ 1

instance (Eq n, Num n) => Fractional (RationalFluxion n) where
    (x :/ y) / (z :/ w) = (x * w) :/ (y * z)
    recip (x :/ y) = y :/ x
    fromRational n = fromInteger (numerator n) :/ fromInteger (denominator n)

---

-- takes a function and two lists, 
-- extends the shorter list with zeros and then zips them with the function
losslessZipWith :: (Num a, Num b) => (a -> b -> c) -> [a] -> [b] -> [c]
losslessZipWith f xs ys = zipWith f
    (xs ++ replicate (length ys - length xs) 0)
    (ys ++ replicate (length xs - length ys) 0)

-- takes a matrix and folds across the diagonals
matrixFold :: Num a => (a -> a -> a) -> [[a]] -> [a]
matrixFold _ [] = []
matrixFold _ [xs] = xs
matrixFold f ((x:xs):(ys:xss)) = x : matrixFold f (losslessZipWith f xs ys : xss)

-- creates a matrix of all possible f-combinations of the elements of xs and ys
cartProduct :: (a -> a -> b) -> ([a],[a]) -> [[b]]
cartProduct f (xs,ys) = map (flip map ys . f) xs

-- does the obvious from it's type signature
separate :: [(a,b)] -> ([a],[b])
separate = foldr (\(x,y) (xs,ys) -> (x:xs,y:ys)) ([],[])

---

-- for looking at them <3
rawShow :: Fluxion n -> ([n], Int)
rawShow (F x) = x

pMap :: (a -> b) -> (a,a) -> (b,b)
pMap f (x,y) = (f x, f y)

---

-- two implementations of <= for fluxions:

-- mine (worse):
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

-- cosmo (https://github.com/cosmobobak)'s (better):

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

-- test to see if they are equivalent
prop_leq :: Fluxion Int -> Fluxion Int -> Bool
prop_leq x y = leq x y == fluxionLEq x y

--- 

-- (raise (/)) is the same as /' in fluxions.txt
raise :: (b -> b -> c) -> (a -> b) -> (a -> b) -> a -> c
raise op f g x = f x `op` g x

{-
-- characteristic function of a fluxion
p :: Fluxion Int -> Fluxion Int -> Fluxion a
p (F (zs,n)) = raise (*) (^ n) (`p'` zs)
    where
        p' x (n:ns) = F ([n],0) + (F ([p' x ns],0)) / (F ([x],0))
-}

-- the same as d in fluxions.txt
d :: (Eq n, Num n) => (Fluxion n -> Fluxion n) -> (n -> Fluxion n)
d f x = f (F ([x],0) + F ([0,1],0)) - f (F ([x],0))

-- the same as ℜ in fluxions.txt
real :: Num n => Fluxion n -> n
real (F ([],_)) = 0
real (F (x:xs,n)) | n < 0 = 0
                  | n == 0 = x
                  | n > 0 = real (F (xs,n-1))

-- the same as lim in fluxions.txt, but gives Nothing when ±ω
lim :: (Num n,Eq n) => Fluxion n -> Maybe n
lim (F ([],n)) = Just 0
lim (F (x:xs,0)) = Just x
lim (F (0:xs,n)) = lim (F (xs,n-1))
lim (F (xs,n)) | n < 0 = Just 0
               | otherwise = Nothing

kanskje :: (a -> b) -> Maybe a -> Maybe b
kanskje f (Just x) = Just (f x)
kanskje _ Nothing = Nothing

diff :: (Eq n, Num n, Fractional n) => (Fluxion n -> Fluxion n) -> n -> Maybe n
diff f n = kanskje real (divide (raise (:/) (d f) (d id) n))

divide :: (Eq n, Num n, Fractional n) => RationalFluxion n -> Maybe (Fluxion n)
divide (F (xs,n) :/ F ([x],0)) = Just (F (map (/ x) xs,n))
divide (F (xs,n) :/ F (0:ys,m)) = divide (F (xs,n) :/ F (ys,m-1))
divide (F (xs,n) :/ F (ys,0)) | last ys /= 0 = Nothing
                              | otherwise = divide (F (xs,n) :/ F (init ys,0))
divide (F (xs,n) :/ F (ys,m)) = divide (F (xs,n-m) :/ F (ys,0))