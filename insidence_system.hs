type IS x y = ([x], x -> x -> Bool, x -> y, [y])

data Thing = A | B | C | D | E
    deriving (Eq, Ord, Show)

isVowel :: Thing -> Bool
isVowel A = True
isVowel E = True
isVowel _ = False

myIS = ([A,B,C,D,E], (==), isVowel, [True, False]) :: IS Thing Bool

project :: Thing -> Int
project A = 1
project E = 2
project B = 1
project C = 2
project D = 3

star :: Thing -> Thing -> Bool
star x y = project x == project y

myIS2 = ([A,B,C,D,E], star, isVowel, [True, False]) :: IS Thing Bool