data Number = N ([Bool],[Bool])
    deriving (Eq, Ord)

instance Show Number where
    show (N ([],[])) = ""
    show (N ([],ys)) = '.' : show (N (ys,[]))
    show (N (x:xs,ys)) = f x ++ show (N (xs,ys))
        where f = show . fromEnum

