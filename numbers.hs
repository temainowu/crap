module Numbers where

import Prelude hiding (div, mod, exp, (/), (^), (%))

class (Ord a,Num a) => Number a where
    (/) :: a -> a -> a
    (%) :: a -> a -> a
    (^) :: a -> a -> a
    _ ^ zero = one
    zero :: a
    one :: a
    neg :: a -> a
    isInf :: a -> Bool

data N = O | S N
    deriving (Eq)

instance Num N where
    x + O = x
    x + (S y) = S (x + y)
    x - O = x
    O - _ = error "negative number"
    (S x) - (S y) = x - y
    _ * O = O
    x * (S y) = x + (x * y)
    abs = id
    signum O = O
    signum _ = S O
    fromInteger 0 = O
    fromInteger n | n < 0 = error "negative number"
                  | otherwise = S (fromInteger (n-1))

instance Show N where
    show n | n < base  = [chars `at` n]
           | otherwise = concat (map show (toBase base n))

instance Ord N where
    O <= _ = True
    S x <= O = False
    S x <= S y = x <= y || S x <= y

instance Number N where
    _ / O = error "division by 0"
    O / _ = O
    x / y = S ((x - y) / y)
    _ % O = error "modulo 0"
    O % _ = O
    x % y | x < y     = x
          | otherwise = (x - y) % y
    _ ^ O = S O 
    x ^ (S y) = x * (x ^ y)
    zero = O
    one = S O
    neg = error "negative number"
    isInf _ = False

data N' = J N | Inf
    deriving (Eq)

instance Num N' where
    x + Inf = Inf
    Inf + x = Inf
    J x + J y = J (x + y)
    _ - Inf = error "negative number"
    Inf - x = Inf
    J x - J y = J (x - y)
    x * Inf = Inf
    Inf * x = Inf
    J x * J y = J (x * y)
    abs = id
    signum Inf = Inf
    signum (J x) = J (signum x)
    fromInteger n | n < 0 = error "negative number"
                  | otherwise = J (fromInteger n)

instance Show N' where
    show Inf = "∞"
    show (J n) = show n

instance Ord N' where
    Inf <= _ = False
    _ <= Inf = True
    J x <= J y = x <= y

instance Number N' where
    _ / Inf = J O
    Inf / _ = Inf
    J x / J O = Inf
    J x / J y = J (x / y)
    _ % Inf = J O
    Inf % _ = Inf
    J x % J O = Inf
    J x % J y = J (x % y)
    _ ^ Inf = Inf
    Inf ^ _ = Inf
    J x ^ J y = J (x ^ y)
    zero = J O
    one = J (S O)
    neg = error "negative number"
    isInf x = x == Inf

data I = Z | P N | N N
    deriving (Eq)

instance Num I where
    x + Z = x
    (P x) + (P y) = P (S (x + y))
    x + (N y) = x - P y
    x + y = y + x
    x - Z = x
    (P x) - (P y) | x > y = P ((x - y) - one)
                  | x < y = N ((y - x) - one)
                  | otherwise = Z
    x - (N y) = x + P y
    x - y = neg (y - x)
    x * Z = Z 
    (P x) * (P y) = opNI (*) (P x) (P y)
    x * (N y) = neg (x * P y)
    x * y = y * x
    abs (N x) = P x 
    abs x = x 
    signum Z = Z
    signum (P x) = P O
    signum (N x) = N O
    fromInteger 0 = Z
    fromInteger n | n > 0 = P (fromInteger (n-1))
                  | n < 0 = N (fromInteger (n+1))

instance Show I where
    show Z = "0"
    show (P x) = show (S x)
    show (N x) = '-' : show (S x)

instance Ord I where
    Z   <= Z   = True
    Z   <= P _ = True
    Z   <= N _ = False
    P _ <= Z   = False
    P x <= P y = S x <= S y
    P _ <= N _ = False
    N _ <= Z   = True
    N _ <= P _ = True 
    N x <= N y = S y <= S x

instance Number I where
    _ / Z = error "division by 0"
    Z / _ = Z
    P x / P y = opNI (/) (P x) (P y)
    x   / N y = neg (x / P y)
    N x / y   = neg (P x / y)
    x % Z = x
    Z % _ = Z
    P x % P y = opNI (%) (P x) (P y)
    x   % N y = neg (neg x % P y)
    N x % P y = P y - (P x % P y)
    P x ^ P y = opNI (^) (P x) (P y)
    _   ^ N y = error "negative exponent"
    Z   ^ _   = Z
    x   ^ y   = x * (x ^ (y - one))
    zero = Z
    one = P O
    neg Z = Z 
    neg (P x) = N x 
    neg (N x) = P x
    isInf _ = False

data I' = Z' | P' N' | N' N'
    deriving (Eq)

instance Num I' where
    x + Z' = x
    (P' x) + (P' y) = P' (x + y + one)
    x + (N' y) = x - P' y
    x + y = y + x
    x - Z' = x
    (P' x) - (P' y) | x > y = P' ((x - y) - one)
                    | x < y = N' ((y - x) - one)
                    | otherwise = Z'
    x - (N' y) = x + P' y
    x - y = neg (y - x)
    x * Z' = Z' 
    (P' x) * (P' y) = P' ((x * y) + x + y)
    x * (N' y) = neg (x * P' y)
    x * y = y * x
    abs (N' x) = P' x 
    abs x = x 
    signum Z' = zero
    signum (P' Inf) = P' Inf 
    signum (N' Inf) = N' Inf
    signum (P' x) = one
    signum (N' x) = neg one
    fromInteger 0 = zero
    fromInteger n | n > 0 = P' (fromInteger (n-1))
                  | n < 0 = N' (fromInteger (n+1))

instance Show I' where
    show (P' x) = show (x + one)
    show (N' x) = '-' : show (x + 1)

instance Ord I' where
    Z'   <= Z'   = True
    Z'   <= P' _ = True
    Z'   <= N' _ = False
    P' _ <= Z'   = False
    P' x <= P' y = x <= y
    P' _ <= N' _ = False
    N' _ <= Z'   = True
    N' _ <= P' _ = True 
    N' x <= N' y = y <= x

instance Number I' where
    x / Z' = P' Inf * signum x
    Z' / _ = Z'
    P' x / P' y = opN'I' (/) (P' x) (P' y)
    x / N' y = neg (x / P' y)
    N' x / y = neg (P' x / y)
    x % Z' = x
    Z' % _ = Z'
    P' x % P' y = opN'I' (%) (P' x) (P' y)
    P' x % N' y = neg (neg (P' x % P' y))
    N' x % P' y = neg (neg (N' x % N' y))
    P' x ^ P' y = opN'I' (^) (P' x) (P' y)
    x    ^ N' y | isInf x = Z'
                | otherwise = error "negative exponent"
    zero ^ _   = zero
    -- N' x ^ P' (J (S y))   = N' x * (N' x ^ P' (J y))
    zero = Z'
    one = P' (J (S O))
    neg Z' = Z'
    neg (P' x) = N' x
    neg (N' x) = P' x
    isInf (P' Inf) = True
    isInf (N' Inf) = True
    isInf _ = False

toBase :: N -> N -> [N]
toBase b = reverse . f b
    where
        f b n | n < b = [n]
        f b n = n % b : (f b ((n - (n % b)) / b))

fromBase :: N -> [N] -> N
fromBase b = f b . reverse
    where
        f b [] = O
        f b (x:xs) = x + b * f b xs

opNI :: (N -> N -> N) -> I -> I -> I
opNI op (P x) (P y) = P (((x + one) `op` (y + one)) - one)

opN'I' :: (N' -> N' -> N') -> I' -> I' -> I'
opN'I' op (P' x) (P' y) = P' (((x + one) `op` (y + one)) - one)

chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" :: [Char]

base = S(S(S(S(S(S O))))) :: N

nats = O : map S nats :: [N]

ints = P O : zipple (map P (tail nats)) (map N (tail nats)) :: [I]

anteNI :: N -> I 
anteNI O = Z
anteNI (S x) = P x

anteN'N :: N' -> N
anteN'N (J x) = x

anteI'I :: I' -> I
anteI'I Z' = Z
anteI'I (P' x) = P (anteN'N x)
anteI'I (N' x) = N (anteN'N x)

zipple :: [a] -> [a] -> [a]
zipple [] _ = []
zipple (x:xs) [] = [x]
zipple (x:xs) (y:ys) = x : y : zipple xs ys

len :: [a] -> N
len [] = O
len (_:xs) = S (len xs)

at :: [a] -> N -> a
at [] _ = error "empty list"
at (x:_) O = x
at (_:xs) (S n) = at xs n