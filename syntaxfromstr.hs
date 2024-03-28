-- type 'pali' followed by your sentence in "" and then 
-- the numbers for the syntactic category of each word in the sentence also in ""
-- using the correspondencies below:

-- 0. sentence final particle            
-- 1. la
-- 2. li/o/subject mi and sina
-- 3. preverb
-- 4. e/preposition
-- 5. pi/nanpa
-- 6. en
-- 7. noun/adjective/adverb/pronoun/verb

-- Example: pali "mi pali e ni la mi pali e ni" "2 3 4 7 1 2 3 4 7"
-- The output will be a syntax tree of the sentence.

-- pali "" "" 

-- note: a string of 3 preverbs or an adverb modifying a preverb breaks the parser.

data Tree u = N (Tree u) (Tree u) | L u | X

toList :: Tree String -> [String]
toList (N t1 t2) = (("__" ++ head (toList t1)) 
                   : map ("| " ++) (tail (toList t1))) ++
                   (("L_" ++ head (toList t2)) 
                   : map ("  " ++) (tail (toList t2)))
toList (L s) = [s]
toList X = []

data PrettyList a = PrettyList [a]

instance Show a => Show (PrettyList a) where
    show (PrettyList []) = ""
    show (PrettyList (x:xs)) = show x ++ '\n' : show (PrettyList xs)

f :: Int -> [(Int,String)] -> [(Int,String)] -> Tree String
f n []     []     = X
f n [x]    []     = L (snd x)
f n []     ys     | n `notElem` (map fst ys) = f (n+1) [] ys
f n [x]    ys     | fst x == n && n `notElem` (map fst ys) = N (L (snd x)) (f (n+1) [] ys)
f n (x:xs) [y]    | fst y == n = N (f n [] (x:xs)) (L (snd y))
f n (x:xs) (y:ys) | fst y == n && n `notElem` (map fst ys) = N (f n [] (x:xs)) (f n [] (y:ys))
f n xs     (y:ys) = f n (xs ++ [y]) ys
f n (x:xs) []     = f n [] (x:xs)

pali :: String -> String -> PrettyList String
pali x = PrettyList . toList . (f 0 []) . label x

label :: String -> String -> [(Int,String)]
label [] _ = []
label xs ys = (read (takeWhile (/= ' ') ys), takeWhile (/= ' ') xs) : label (drop 1 (dropWhile (/= ' ') xs)) (drop 1 (dropWhile (/= ' ') ys))
