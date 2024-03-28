main :: IO ()
main = return (1) >>= (return (2) >>= \ a b -> print (a,b))