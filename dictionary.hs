{-# LANGUAGE GADTs #-}

data Indexed u where 
    (:/:) :: Show u => u -> String -> Indexed u

newtype D u = O [Indexed u]
    deriving (Show)

instance Show (Indexed u) where
    show (u :/: a) = "(ijo " ++ show u ++ ", lon " ++ show a ++ ")"


ind :: Indexed u -> String
ind (_ :/: a) = a

val :: Indexed u -> u
val (u :/: _) = u

(!) :: D u -> String -> u
(O d) ! x = (\[x]->x) [val e | e <- d, ind e == x]

weka :: D u -> String -> D u
(O d) `weka` x = O [e | e <- d, ind e /= x]

jo :: D u -> Indexed u -> D u
(O []) `jo` e = O [e]
(O (x:xs)) `jo` e
    | ind x == ind e = O (e:xs)
    | otherwise = O (x:e:xs)

dic = O ["mi":/:"wan","olin":/:"tu","e":/:"tu wan","sina":/:"tu tu"]