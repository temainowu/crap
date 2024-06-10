data A = K | I
    deriving (Show, Eq)

newtype M = M [A]
    deriving (Show)

instance Eq M where
    x /= y = not (x == y)
    x == y = unwrap (reduceM x) == unwrap (reduceM y)

{-
every element of M is equal to one of the following 14 elements (|M| = 14):

k
ik
kik
ikik
kikik
ikikik
i
ki
iki
kiki
ikiki
kikiki
ikikiki
-}

instance Semigroup M where
    M xs <> M ys = M (xs ++ ys)

instance Monoid M where
    mempty = M []

instance Ord M where
    x >= y = y <= x
    x > y = y < x
    x < y = x <= y && x /= y
    x <= y | not (reduced x && reduced y) = reduceM x <= reduceM y
    M [] <= M [] = True
    M (K:xs) <= M (K:ys) = M xs <= M ys -- 1
    M (I:xs) <= M (I:ys) = M ys <= M xs -- 2
    M (I:K:I:xs) <= y = M xs <= y       -- 3
    x <= M (K:ys) = x <= M ys           -- 4

-- M is only partially ordered
-- x and y are incomparable if the parity of the number of Is in x and y is different

reduceM :: M -> M
reduceM (M []) = M []
reduceM (M (K:K:xs)) = reduceM (M (K:xs)) -- K is idempotent
reduceM (M (I:I:xs)) = reduceM (M xs) -- I is its own inverse
reduceM (M (K:I:K:I:K:I:K:xs)) = reduceM (M (K:I:K:xs)) -- ?
reduceM (M (x:xs)) = M [x] <> reduceM (M xs)
unwrap :: M -> [A]
unwrap (M xs) = xs

reduced :: M -> Bool
reduced (M xs) = xs == unwrap (reduceM (M xs))