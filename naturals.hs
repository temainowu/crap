data N = N [N]

instance Enum N where
    toEnum 0 = N [toEnum 0]
    toEnum n = toEnum (N [toEnum n])
    fromEnum (N xs) = length xs - 1

instance Eq N where
    (==) (N xs) (N ys) = length xs == length ys

instance Foldable N where
    foldr f z (N xs) = foldr f z xs

x = N [x] :: N 

suc :: N -> N
suc (N n) = N (n ++ [suc (N n)])

data N' = X | S N'

instance Enum N' where
    toEnum 0 = X
    toEnum n = S (toEnum (n-1))
    fromEnum X = 0
    fromEnum (S n) = 1 + fromEnum n

instance Eq N' where
    (==) X X = True
    (==) (S n) (S m) = n == m
    (==) _ _ = False

instance Foldable N' where
    foldr f z X = z
    foldr f z (S n) = f n (foldr f z n)

instance Elem N' where
    elem X _ = True
    elem _ X = False
    elem (S n) (S m) = elem n m

instance Ord N' where
    compare X X = EQ
    compare X (S n) = LT
    compare (S n) X = GT
    compare (S n) (S m) = compare n m

suc' :: N' -> N'
suc' n = S n