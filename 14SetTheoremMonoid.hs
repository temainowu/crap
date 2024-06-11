import Prelude hiding ((*))

data A = K | I
    deriving (Show, Eq)

newtype M = M [A]

instance Show M where
    show (M []) = "ε"
    show (M [K]) = "k"
    show (M [I]) = "-"
    show (M (K:xs)) = "k" ++ show (M xs)
    show (M (I:xs)) = "-" ++ show (M xs)

newtype ShowSets = S [[M]]

instance Show ShowSets where
    show (S []) = ""
    show (S (x:xs)) = show x ++ "\n" ++ show (S xs)

instance Eq M where
    x == y = unwrap (reduceM x) == unwrap (reduceM y)

{-
every element of M is equal to one of the following 14 elements (|M| = 14):
ε       (idempotent)
k       (idempotent)
ikik    (idempotent)
kikik   (idempotent)
iki     (idempotent)
kiki    (idempotent)
ikikiki (idempotent)

kik     (= kik<>kik<>kik)
ikikik  (= ikikik<>ikikik<>ikikik)
ikiki   (= ikiki<>ikiki<>ikiki)
kikiki  (= kikiki<>kikiki<>kikiki)

i       (own inverse)
ik      (weird)
ki      (weird)

- any of {i,ik,ki} when <>ed with any other produces something idempotent
- the idempotent elements form a submonoid
- the only subsets of M that form groups are {ε} and {ε,i}
-}

instance Semigroup M where
    M xs <> M ys = reduceM (M (xs ++ ys))

instance Monoid M where
    mempty = M []

instance Ord M where
    x <= y | not (reduced x && reduced y) = reduceM x <= reduceM y
           | x == y = True
    M (K:xs) <= M (K:ys) = M xs <= M ys
    M (I:xs) <= M (I:ys) = M ys <= M xs
    M xs <= M ys | not (null xs || null ys) && last xs == last ys = M (init xs) <= M (init ys)
    M (I:K:I:xs) <= y = M xs <= y -- neither of these rules are reversible
    x <= M (K:ys) = x <= M ys     -- x <= ky if x <= y but if x <= ky then x might be > y

{-
M is only partially ordered
two elements are incomparable if the parity of the number of Is in them is different
the following is, to my knowledge, as much ordering as can be done:
(a,b<c means a<c and b<c but none of a<b, b<a, or a=b hold)
iki, ikikiki < ikik,  ε, kiki < k,  kikik
ik,  ikikik  < ikiki, i, kik  < ki, kikiki
-}

consM :: A -> M -> M
consM x (M xs) = M (x:xs)

reduceM :: M -> M
reduceM (M xs) = (length xs * simplifyM) (M xs)
    where
        (*) :: Int -> (a -> a) -> a -> a
        0 * _ = id
        n * f = ((n-1) * f) . f

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

m :: [M]
m = [M [], M [K], M [I,K], M [K,I,K], M [I,K,I,K], M [K,I,K,I,K], M [I,K,I,K,I,K], M [I], M [K,I], M [I,K,I], M [K,I,K,I], M [I,K,I,K,I], M [K,I,K,I,K,I], M [I,K,I,K,I,K,I]]

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

isMonoid :: [M] -> Bool
isMonoid xs = mempty `elem` xs && and [x <> y `elem` xs {- && (x <> y) <> z == x <> (y <> z) -} | x <- xs, y <- xs, z <- xs]

submonoids' :: [[M]]
submonoids' = filter isMonoid (powerset m)

submonoids :: [[M]]
submonoids = map (map M) [[[]],[[],[I,K,I,K,I,K,I]],[[],[I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[K,I,K,I]],[[],[K,I,K,I],[I,K,I,K,I,K,I]],[[],[K,I,K,I],[K,I,K,I,K,I]],[[],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[I,K,I]],[[],[I,K,I],[I,K,I,K,I,K,I]],[[],[I,K,I],[I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[I,K,I],[K,I,K,I],[I,K,I,K,I,K,I]],[[],[I,K,I],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[K,I],[K,I,K,I],[K,I,K,I,K,I]],[[],[K,I],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[K,I],[I,K,I],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[I]],[[],[K,I,K,I,K]],[[],[K,I,K,I,K],[K,I,K,I]],[[],[I,K,I,K]],[[],[I,K,I,K],[I,K,I,K,I,K,I]],[[],[I,K,I,K],[I,K,I],[I,K,I,K,I,K,I]],[[],[I,K,I,K],[I,K,I,K,I,K]],[[],[I,K,I,K],[I,K,I,K,I,K],[I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[I,K,I,K],[I,K,I,K,I,K],[I,K,I],[I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[I,K,I,K],[K,I,K,I,K]],[[],[I,K,I,K],[K,I,K,I,K],[K,I,K,I],[I,K,I,K,I,K,I]],[[],[I,K,I,K],[K,I,K,I,K],[I,K,I],[K,I,K,I],[I,K,I,K,I,K,I]],[[],[K,I,K],[K,I,K,I,K]],[[],[K,I,K],[K,I,K,I,K],[K,I,K,I],[K,I,K,I,K,I]],[[],[K,I,K],[K,I,K,I,K],[K,I],[K,I,K,I],[K,I,K,I,K,I]],[[],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K]],[[],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K],[I,K,I],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K],[K,I],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K],[K,I],[I,K,I],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K],[I],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[I,K],[I,K,I,K],[I,K,I,K,I,K]],[[],[I,K],[I,K,I,K],[I,K,I,K,I,K],[I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[I,K],[I,K,I,K],[I,K,I,K,I,K],[I,K,I],[I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[I,K],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K]],[[],[I,K],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[I,K],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K],[I,K,I],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[K]],[[],[K],[K,I,K,I,K]],[[],[K],[K,I,K,I,K],[K,I,K,I]],[[],[K],[I,K,I,K],[K,I,K,I,K]],[[],[K],[I,K,I,K],[K,I,K,I,K],[K,I,K,I],[I,K,I,K,I,K,I]],[[],[K],[I,K,I,K],[K,I,K,I,K],[I,K,I],[K,I,K,I],[I,K,I,K,I,K,I]],[[],[K],[K,I,K],[K,I,K,I,K]],[[],[K],[K,I,K],[K,I,K,I,K],[K,I,K,I],[K,I,K,I,K,I]],[[],[K],[K,I,K],[K,I,K,I,K],[K,I],[K,I,K,I],[K,I,K,I,K,I]],[[],[K],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K]],[[],[K],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[K],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K],[I,K,I],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[K],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K],[K,I],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[K],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K],[K,I],[I,K,I],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[K],[I,K],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K]],[[],[K],[I,K],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[K],[I,K],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K],[I,K,I],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[K],[I,K],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K],[K,I],[I,K,I],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]],[[],[K],[I,K],[K,I,K],[I,K,I,K],[K,I,K,I,K],[I,K,I,K,I,K],[I],[K,I],[I,K,I],[K,I,K,I],[I,K,I,K,I],[K,I,K,I,K,I],[I,K,I,K,I,K,I]]]

ofOrder :: [[M]] -> Int -> [[M]]
mons `ofOrder` n = filter ((== n) . length) mons

distributionOf :: [[M]] -> [Int]
distributionOf mons = map (length . (mons `ofOrder`)) [1 .. 14]
-- [1,7,10,8,5,10,5,0,1,5,5,2,1,1] (submonoids) 61
-- [1,7,6,4,0,0,0,0,0,0,0,0,0,0]   (commutative submonoids) 18
-- [1,1,0,0,0,0,0,0,0,0,0,0,0,0]   (groups) 2

commutes :: [M] -> Bool
commutes xs = and [x <> y == y <> x | x <- xs, y <- xs]

commutativeSubmonoids :: [[M]]
commutativeSubmonoids = filter commutes submonoids

{-
commutative submonoids:
1.
[ε]
[ε,k]
[ε,k-k-k]
[ε,k,k-k-k]
[ε,k,k-k,k-k-k]
[ε,k-k,k-k-k]
2.
[ε]
[ε,k-k-]
[ε,k-,k-k-,k-k-k-]
[ε,k-k-,k-k-k-]
3.
[ε]
[ε,-k-k]
[ε,-k,-k-k,-k-k-k]
[ε,-k-k,-k-k-k]
4.
[ε]
[ε,-k-]
[ε,-k-k-k-]
[ε,-k-,-k-k-k-]
[ε,-k-,-k-k-,-k-k-k-]
[ε,-k-k-,-k-k-k-]
5.
[ε,-]

these form 5 classes
class 1 contains {ε,k,kik,kikik}
classes 2 to 4 are derived from class 1 by combining elements with - on either side
class 5 is weird and different and shall be ignored

class 2 and 3 are isomorphic by reflection
class 1 and 4 are isomorphic by conjugation by -
-}