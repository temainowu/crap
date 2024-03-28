data Tree u = T | N u | (Tree u) :^: (Tree u) | P (Tree u,Tree u)

instance Num (Tree u) where
    x + y = x :^: y
    T * x = x
    N x * N y = P (N x,N y)
    P x * P y = P (P x,P y)
    P x * N y = P (P x,N y)
    N x * P y = P (N x,P y)
    x * T = x
    x * N y = N y * x
    x * P y = P y * x
    x * (y :^: z) = (x * y) * z
    abs x = x
    signum x = x
    fromInteger x = T
    negate x = x

data PrettyList a = PrettyList [a]

instance Show a => Show (PrettyList a) where
    show (PrettyList []) = []
    show (PrettyList (x:xs)) = show x ++ ('\n' : show (PrettyList xs))

instance Show u => Show (Tree u) where
    show = show . PrettyList . toList

toList :: Show u => Tree u -> [String]
toList (x :^: y) = (("+-" ++ head (toList x)) 
                     : map ("| " ++) (tail (toList x))) ++
                     (("+-" ++ head (toList y)) 
                     : map ("  " ++) (tail (toList y)))
toList (P x) = [show x] 
toList (N x) = [show x]
toList T = []