import Prelude hiding ((*))

data A = K | I
    deriving (Show, Eq)

newtype M = M [A]
    deriving (Show)

instance Eq M where
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
    M xs <> M ys = reduceM (M (xs ++ ys))

instance Monoid M where
    mempty = M []

instance Ord M where
    x <= y | not (reduced x && reduced y) = reduceM x <= reduceM y
    M [] <= M [] = True
    M (K:xs) <= M (K:ys) = M xs <= M ys -- 1
    M (I:xs) <= M (I:ys) = M ys <= M xs -- 2
    M (I:K:I:xs) <= y = M xs <= y       -- 3
    x <= M (K:ys) = x <= M ys           -- 4

-- M is only partially ordered
-- two elements are incomparable if the parity of the number of Is in them is different

consM :: A -> M -> M
consM x (M xs) = M (x:xs)

reduceM :: M -> M
reduceM (M xs) = (length xs * simplifyM) (M xs)
    where
        infixl 8 *
        (*) :: Int -> (a -> a) -> a -> a
        0 * _ = id
        n * f = (n-1) * f . f
        
simplifyM :: M -> M
simplifyM (M []) = M []
simplifyM (M (K:K:xs)) = simplifyM (M (K:xs)) -- K is idempotent
simplifyM (M (I:I:xs)) = simplifyM (M xs) -- I is its own inverse
simplifyM (M (K:I:K:I:K:I:K:xs)) = simplifyM (M (K:I:K:xs)) -- ?
simplifyM (M (x:xs)) = consM x (simplifyM (M xs))

reduced :: M -> Bool
reduced (M xs) = xs == unwrap (simplifyM (M xs))

unwrap :: M -> [A]
unwrap (M xs) = xs