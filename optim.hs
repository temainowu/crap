maxi :: [Int] -> Int
maxi [x] = x
maxi (x:xs) = max x (maxi xs)

f :: [Int] -> Int
f xs = (fromEnum . null . fst) (break (== (maxi xs)) xs)