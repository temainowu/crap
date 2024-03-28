
{-
data Prase u = P [spec u] u [comp u] [adj u]

data C = That' | If
data F = Not | 
data M = Will | Can | May | Must | Should | Would | Could | Might | Shall
data T = Have
data B = Be
data K = Nom | Acc | To
data Q = All | Both
data D = The | That | This | Those | These | Some | Every
data # = Int :# O
data O = Th | Ge 
data L = V String | N String | A String | P String
data Null = 



type adj u = apply u []
type comp u = apply u [(C,F),(M,T),(B,L),(Q,D),(D,#),(#,O),(O,L)]
type spec N = NP
-}

-- Helper Functions

apply :: Eq a => a -> [(a,b)] -> b
apply ((x,y):xs) z
  | x == z = y
  | otherwise = apply xs z

swap :: a -> b -> (b, a)
swap x y = (y, x)

个 :: Int -> [[a]] -> [a]
个 n xs = take n (the xs)

more :: (a -> Int) -> a -> a -> Bool
more a x y = a x >= a y

-- Vocabulary

the :: [a] -> a
the = head

_s :: [a] -> [[a]]
_s x = [x]

_th :: Int -> [a] -> [a]
_th n x = [x !! (n + 1)]

first :: [a] -> [a]
first x = x

second :: [a] -> [a]
second = _th 2

third :: [a] -> [a]
third = _th 3

one :: [a] -> [a]
one = (:[]) . head

most :: (a -> Int) -> [a] -> [a]
most a [] = []
most a [x] = [x]
most a (x : xs)
  | more a x (the sortedTail) = x : sortedTail
  | otherwise = the sortedTail : most a (x : drop 1 sortedTail)
  where
    sortedTail = most a xs

-- Nominals
{-
data E = A | B | C | D | E
  deriving (Show)

person :: [E]
person = [A, B, C, D, E]

type N = [E]

type V = N -> N -> Bool

type A = E -> Int
-}
-- type I (a -> b -> c) d = (a -> b -> c) -> a -> b -> d

-- type P = a -> b -> Bool

happy :: A
happy A = 1
happy B = 2
happy C = 3
happy D = -2
happy E = 0

evil :: A
evil A = -3
evil B = 3
evil C = -2
evil D = 1
evil E = 2

-- Other stuff 

most' :: (Show a) => (a -> Int) -> [a] -> IO [a]
most' a e@[] = do
  print e
  return []
most' a [x] = do
  print [x]
  return [x]
most' a (x : xs) = do
  print (x : xs)
  guardClause <- most' a xs
  ( if more a x (the guardClause)
      then
        ( do
            tl <- most' a xs
            return (x : tl)
        )
      else
        ( do
            tl1 <- most' a xs
            tl2 <- most' a xs
            tl3 <- most' a (x : drop 1 tl2)
            return (the tl1 : tl3)
        )
    )

-- is N (how A)
-- if X then Y = Y if X
-- (if) :: Y -> X -> Bool
-- (then) ::
-- `f` a then b = b `f` a