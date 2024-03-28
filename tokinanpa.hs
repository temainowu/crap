import Prelude hiding (compare)
import Data.List (transpose)

data PrettyList a = PrettyList [a]

instance Show a => Show (PrettyList a) where
    show (PrettyList []) = ""
    show (PrettyList (x:xs)) = show x ++ '\n' : show (PrettyList xs)

tan_wan = "wan"

o :: [a] -> PrettyList a
o = PrettyList

nanpa :: String -> (String -> String) -> [String]
nanpa x f = x : map f (nanpa x f)

lon_nasin :: (String -> String) -> (String -> String)
lon_nasin f = (reverse . f . reverse)

tawa :: [x] -> Int -> [x]
xs `tawa` n = take n xs

pu1 :: String -> String
pu1 "naw" = "ut"
pu1 "ut" = "etum"
pu1 "etum" = "etum" 

pu :: String -> String
pu ('u':'t':' ':'u':'t':' ':'a':'k':'u':'l':' ':'a':'k':'u':'l':' ':'a':'k':'u':'l':' ':'e':'t':'u':'m':' ':'e':'t':'u':'m':' ':'e':'t':'u':'m':' ':'e':'t':'u':'m':xs) = "ela" ++ xs
pu ('u':'t':' ':'u':'t':' ':'a':'k':'u':'l':' ':'a':'k':'u':'l':' ':'a':'k':'u':'l':xs) = "etum" ++ xs
pu ('u':'t':' ':'u':'t':xs) = "akul" ++ xs
pu ('n':'a':'w':xs) = "ut" ++ xs
pu xs = "naw " ++ xs 

pona :: String -> String
pona ('u':'t':' ':'u':'t':xs) = "akul" ++ xs
pona ('n':'a':'w':xs) = "ut" ++ xs
pona xs = "naw " ++ xs 

r :: Int -> String -> String
r 0 _ = []
r n s = s ++ r (n-1) s

check :: String -> Bool
check [] = True
check (' ':'n':'e':_) = True
check _ = False

begins :: Eq a => [a] -> [a] -> Bool
begins _ [] = True
begins [] _ = False
begins (x:xs) (y:ys) = x == y && begins xs ys 

aux :: Int -> String -> String
aux n xs | begins xs ("ne" ++ r n " ut") && check (drop (3*n+2) xs) = aux (n+1) (drop (3*n+3) xs)
         | check xs = r (n-1) "ut " ++ "ut" ++ xs
         | otherwise = r n "ut " ++ xs

mi :: String -> String
mi xs@('u':'t':_) = "naw ne " ++ xs
mi ('n':'a':'w':' ':xs) = aux 1 xs
mi ('n':'a':'w':_) = "ut" 

syllables :: String -> Int
syllables [] = 0
syllables "naw" = 1
syllables "ut" = 1
syllables "ne" = 1
syllables "akul" = 2
syllables "etum" = 2
syllables "ela" = 2
syllables xs | null y    = syllables x 
             | otherwise = syllables x + syllables (tail y)
    where (x,y) = break (==' ') xs

isBetter :: Int -> [Int] -> Bool
isBetter n (x:xs) | all (>0) (take n (x:xs)) = True
                  | otherwise = isBetter n xs

inf :: (Int -> [a]) -> [a]
inf f = [ last (f n) | n <- [1..] ]

check0 n = isBetter n (deltaSigma pu mi)

isBetter' n i (x:xs) | all (>0) (take (n+1) (x:xs)) = isBetter' (n+1) (i+1) (x:xs)
                     | all (>0) (take n (x:xs)) = (i+1-n,n) : isBetter' (n+1) (i+1) (x:xs)
                     | otherwise = isBetter' n (i+1) xs

suli :: (String -> String) -> (String -> String) -> [(Int,Int)]
suli f = isBetter' 1 0 . deltaSigma f

maxes :: Ord a => [a] -> [a]
maxes (x:y:xs) | max x y == x = maxes (x:xs)
               | max x y == y = y : maxes (y:xs)

mins :: Ord a => [a] -> [a]
mins (x:y:xs) | min x y == x = mins (x:xs)
              | min x y == y = y : mins (y:xs)

nasin :: (String -> String) -> [String]
nasin f = nanpa tan_wan (lon_nasin f) `tawa` 100

deltaSigma :: (String -> String) -> (String -> String) -> [Int]
deltaSigma f g = inf (compare f g) 

compare :: (String -> String) -> (String -> String) -> Int -> [Int]
compare f g n = zipWith (-) (sigma f n) (sigma g n) :: [Int]

sigma :: (String -> String) -> Int -> [Int]
sigma f n = take n (map syllables (nanpa "naw" f)) :: [Int]

ss n = sum (take n (deltaSigma pu mi))
sm n = maxes (take n (deltaSigma pu mi))

seme = mins (deltaSigma pu mi) !! 27

aaa = maxes (deltaSigma pu mi) !! 9

-- (deltaSigma pu mi) !! 10000 == 151
-- suli = [(7,1),(15,3),(255,4),(511,8),(767,12),(1023,61),(1535,79),(2047,255)...]