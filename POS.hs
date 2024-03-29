module POS where

import Prelude hiding (all,pi)

type N a = [a]
type Q a = N a -> V a -> Bool
type QP a = V a -> Bool
type Cop a = N a -> V a
type I a = V a -> V a
type V a = a -> Bool
type V1 a = [(a,a)]
type K a = V1 a -> QP a -> V a

makeQ :: ([Bool] -> Bool) -> Q a
makeQ f xs p = f [ p x | x <- xs ]

all :: Q a
all = makeQ and

some :: Q a
some = makeQ or

most :: Q a
most = makeQ (\xs -> length (filter id xs) > length (filter not xs))

no :: Q a
no = makeQ (not . or)

are :: Eq a => Cop a
are xs x = elem x xs

er :: V1 a -> N a 
er = map fst

ee :: V1 a -> N a
ee = map snd

e :: Eq a => K a
v `e` qp = are (er (filter (\(_,x) -> qp (are [x])) v))

pi :: Eq a => V1 a -> QP a -> V1 a
v `pi` qp = filter (\(_,x) -> qp (are [x])) v

the :: [a] -> [a]
the = (:[]) . head

cats = ["kitka","graham","jasper"] :: N String

dogs = ["eryk"] :: N String

people = ["anna","cosmo"] :: N String

happy = ["kitka","jasper"] :: N String

love = [("kitka","graham"),("graham","kitka"),("jasper","kitka"),("anna","eryk"),("anna","cosmo"),("cosmo","anna"),("anna","kitka"),("anna","jasper"),("cosmo","kitka")] :: V1 String

s0 = most cats (are happy)
s1 = all people (love `e` some cats)
s2 = all people (love `e` all cats)
s3 = some people (love `e` some dogs)
s4 = all people (are (er (love `pi` some cats)))

prop_nominalise :: Eq a => QP a -> V1 a -> QP a -> Bool
prop_nominalise s v o = s (v `e` o) == s (are (er (v `pi` o)))
