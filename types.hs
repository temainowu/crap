import Data.List (nub,(\\),sort)

zipple :: [a] -> [a] -> [a]
zipple [] _ = []
zipple (x:xs) [] = [x]
zipple (x:xs) (y:ys) = x : y : zipple xs ys

maybe :: Type -> Type
maybe u = addNew 0 u

addNew :: Int -> Type -> Type 
addNew n u | show n `elem` toList u = addNew (n-1) u
           | otherwise = E (show n) :|: u

list :: Type -> Type
list Empty = Empty
-- list ((toList x) :|: xs) = clean (fromList [x : ps | ps <- toList (list xs)] :|: list xs)

int :: Type
int = fromList (map show (zipple [0..] [(-1)..]))

par :: String -> String
par s = '(' : s ++ ")"

removeEdges :: [a] -> [a]
removeEdges xs = aux True xs
    where
        aux :: Bool -> [a] -> [a]
        aux True [] = []
        aux True (x:y:[]) = []
        aux True (x:xs) = aux False xs
        aux False [] = []
        aux False (x:y:[]) = [x]
        aux False (x:xs) = x : aux False xs

data Path = L | R | Found | NotFound
    deriving (Eq,Show)

data Type = Empty | E String | Type :>: Type | Type :*: Type | Type :|: Type 

instance Eq Type where
    Empty == Empty = True
    E x == E y = x == y
    (x :>: y) == (z :>: w) = x == z && y == w
    t@(x :*: y) == t'@(z :*: w) = andPhrase t `subList` andPhrase t' && andPhrase t' `subList` andPhrase t
    t@(x :|: y) == t'@(z :|: w) = orPhrase t `subList` orPhrase t' && orPhrase t' `subList` orPhrase t
    _ == _ = False

instance Show Type where

    show t = show0 t ++ " }-{ " ++ show1 t
        where
            show0 :: Type -> String
            show0 Empty = "{}"
            show0 (E x) = x
            show0 (x :>: y) = par (show0 x ++ " -> " ++ show0 y)
            show0 t@(x :*: y) = par (removeEdges (show (map show0 (andPhrase t))))
            show0 t@(x :|: y) = show (map show0 (orPhrase t))

            show1 :: Type -> String
            show1 Empty = "{}"
            show1 (E x) = x
            show1 (x :>: y) = par (show1 x ++ " :>: " ++ show1 y)
            show1 (x :*: y) = par (show1 x ++ " :*: " ++ show1 y)
            show1 (x :|: y) = par (show1 x ++ " :|: " ++ show1 y)

instance Ord Type where
    compare Empty Empty = EQ
    compare Empty _ = LT
    compare _ Empty = GT
    compare (E x) (E y) = compare x y
    compare (x :>: y) (z :>: w) = compare x z <> compare y w
    compare (x :*: y) (z :*: w) = compare (andPhrase (x :*: y)) (andPhrase (z :*: w))
    compare (x :|: y) (z :|: w) = compare (orPhrase (x :|: y)) (orPhrase (z :|: w))
    compare _ _ = EQ

subList :: Eq a => [a] -> [a] -> Bool
subList as bs = and [ a `elem` bs | a <- as ]

size :: Type -> Int
size Empty = 0
size (E x) = 1
size (x :>: y) = size y - size x
size (x :*: y) = size x * size y
size (x :|: y) = size x + size y

andPhrase :: Type -> [Type]
andPhrase Empty = []
andPhrase (x :*: y) = andPhrase x ++ andPhrase y
andPhrase x = [x]

orPhrase :: Type -> [Type]
orPhrase Empty = []
orPhrase (x :|: y) = orPhrase x ++ orPhrase y
orPhrase x = [x]

find :: String -> Type -> [[Path]]
find x t = filter ((== Found) . last) (isAt x t)
    where 
        isAt :: String -> Type -> [[Path]]
        isAt x Empty = [[NotFound]]
        isAt x (E y) | x == y    = [[Found]]
                     | otherwise = [[NotFound]]
        isAt x (y :>: z) = map (L:) (isAt x y) ++ map (R:) (isAt x z)
        isAt x (y :*: z) = map (L:) (isAt x y) ++ map (R:) (isAt x z)
        isAt x (y :|: z) = map (L:) (isAt x y) ++ map (R:) (isAt x z)

anyEq :: Eq a => [a] -> Bool
anyEq [] = False
anyEq (x:xs) = x `elem` xs || anyEq xs

takeOr :: Int -> Type -> Type
takeOr n t = conjOr (take n) t

takeAnd :: Int -> Type -> Type
takeAnd n t = conjAnd (take n) t

sortType :: Type -> Type
sortType Empty = Empty
sortType (E x) = E x
sortType (x :>: y) = sortType x :>: sortType y
sortType (x :*: y) = conjAnd sort (x :*: y)
sortType (x :|: y) = conjOr sort (x :|: y)

unOrPhrase :: [Type] -> Type
unOrPhrase = foldr (:|:) Empty

unAndPhrase :: [Type] -> Type
unAndPhrase = foldr (:*:) (E "")

conjOr :: ([Type] -> [Type]) -> Type -> Type
conjOr f = unOrPhrase . f . orPhrase

conjAnd :: ([Type] -> [Type]) -> Type -> Type
conjAnd f = unAndPhrase . f . andPhrase

allStrs :: Type -> [String]
allStrs Empty = []
allStrs (E x) = [x]
allStrs (x :>: y) = allStrs x ++ allStrs y
allStrs (x :*: y) = allStrs x ++ allStrs y
allStrs (x :|: y) = allStrs x ++ allStrs y

clean :: Type -> Type
clean t | cleaner t == t = sortType t
        | otherwise = clean (cleaner t)
        where
            cleaner :: Type -> Type
            cleaner t                 | anyEq (orPhrase t) = cleaner (conjOr nub t)
            cleaner Empty             = Empty
            cleaner (E x)             = E x
            cleaner (Empty :>: y)     = cleaner y
            cleaner (x :>: Empty)     = Empty
            cleaner (x :>: y)         = cleaner x :>: cleaner y
            cleaner (E _ :*: y)       = cleaner y
            cleaner (x :*: E _)       = cleaner x
            cleaner (Empty :*: y)     = Empty
            cleaner (x :*: Empty)     = Empty
            cleaner ((x :*: y) :*: z) = cleaner x :*: cleaner (y :*: z)
            cleaner (x :*: y)         = cleaner x :*: cleaner y
            cleaner (Empty :|: y)     = cleaner y
            cleaner (x :|: Empty)     = cleaner x
            cleaner ((x :|: y) :|: z) = cleaner x :|: cleaner (y :|: z)
            cleaner (x :|: y)         = cleaner x :|: cleaner y

            rename :: [[[Path]]] -> Type -> Type
            rename [] t = t
            rename (x:xs) t = rename xs (aux 1 x t)
                where
                    aux :: Int -> [[Path]] -> Type -> Type
                    aux n [] t = t
                    aux n (x:xs) t = rename' n x (aux (n+1) xs t)

                    rename' :: Int -> [Path] -> Type -> Type
                    rename' n [Found] (E x) = E (x ++ replicate n '\'')
                    rename' n (L:xs) (x :>: y) = rename' n xs x :>: y
                    rename' n (R:xs) (x :>: y) = x :>: rename' n xs y
                    rename' n (L:xs) (x :*: y) = rename' n xs x :*: y
                    rename' n (R:xs) (x :*: y) = x :*: rename' n xs y
                    rename' n (L:xs) (x :|: y) = rename' n xs x :|: y
                    rename' n (R:xs) (x :|: y) = x :|: rename' n xs y


split :: Type -> (Type,Type)
split Empty = (Empty,Empty)
split (E x) = (E x,Empty)
split (x :>: y) = (x, y)
split (x :*: y) = (x, y)
split (x :|: y) = (x, y)

either :: Type -> Type -> Type
either x y = x :|: y

fromList :: [String] -> Type
fromList [] = Empty
fromList (x:xs) = clean (E x :|: fromList xs)

toList :: Type -> [String]
toList Empty = []
toList (x :|: xs) = toList x ++ toList xs
toList x = [show x]