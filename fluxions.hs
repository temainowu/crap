import OptimalityTheory.Phones
import OptimalityTheory.OT (Lexeme, Constraint, Grammar, toLexeme)

type Fluxion = ([Int], Int)
-- Fluxions are used to represent the harmony of a form
-- the smaller the fluxion, the more harmonic the form
-- ([a₀,a₁,...,aₖ],n) represents the fluxion (a₀ε⁰+a₁ε¹+...+aₖεᵏ)ωⁿ
-- this is able to represent all possible finitely long integer fluxions
-- see fluxions.txt for more information on fluxions

(≻) :: Lexeme -> Lexeme -> Grammar -> String -> Bool
x ≻ y = \g i -> fluxionLEq (eval g (toLexeme i) x) (eval g (toLexeme i) y)

{- not possible because Constraint can't be an instance of Eq
(⪢) :: Constraint -> Constraint -> Grammar -> Bool
x ⪢ y = \g -> find g x < find g y

find :: Eq a => [a] -> a -> Int
find xs y = head [i | (i,x) <- zip [0..] xs, y == x]
-}


eval :: Grammar -> Lexeme -> Lexeme -> Fluxion
eval g i o = (map (\ f -> f i o) g,0)

-- Fluxions

fluxionLEq :: Fluxion -> Fluxion -> Bool
fluxionLEq ([],n) ([],m) = True
fluxionLEq ([],n) (y:ys,m)
    | y /= 0 = y > 0
    | otherwise = fluxionLEq ([],0) (ys,m-1)
fluxionLEq (x:xs,n) ([],m)
    | x /= 0 = x < 0
    | otherwise = fluxionLEq (xs,n-1) ([],0)
fluxionLEq (x:xs,n) (y:ys,m)
    | x == 0 = fluxionLEq (xs,n-1) (y:ys,m)
    | y == 0 = fluxionLEq (x:xs,n) (ys,m-1)
    | n < m = y > 0
    | n > m = x < 0
    | n == m = case () of
       () | x < y -> True
          | x > y -> False
          | otherwise -> fluxionLEq (xs,n-1) (ys,m-1)

-- equivalent to fluxionLEq written by cosmo bobak (https://github.com/cosmobobak):

-- takes two fluxions to representations with the same "n" and equal-length lists
doubleNormalise :: ([Int], Int) -> ([Int], Int) -> (([Int], Int), ([Int], Int))
doubleNormalise ([], n) ([], m) = doubleNormalise ([0], n) ([0], m)
doubleNormalise r@(xs, n) l@(ys, m)
  -- if the offsets are the same, we just need to pad the shorter list with zeros
  | n == m = ((xs ++ replicate xsMakeUp 0, n), (ys ++ replicate ysMakeUp 0, m))
  -- if the left fluxion has a smaller offset, we need to left-pad it with zeros
  | n < m = doubleNormalise (replicate (m - n) 0 ++ xs, m) l
  -- if the right fluxion has a smaller offset, we need to left-pad it with zeros
  | otherwise = doubleNormalise r (replicate (n - m) 0 ++ ys, n)
  where
    xsMakeUp = max (length ys - length xs) 0
    ysMakeUp = max (length xs - length ys) 0

leq :: Fluxion -> Fluxion -> Bool
leq x y = uncurry (\(xs, _) (ys, _) -> xs <= ys) (doubleNormalise x y)

prop_leq :: Fluxion -> Fluxion -> Bool
prop_leq x y = leq x y == fluxionLEq x y