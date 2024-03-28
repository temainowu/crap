import Data.Char

data Player = One | Two | Win Player
    deriving (Eq)

data Square = Square | Gap | J Player | Cross | Wall
    deriving (Eq)

instance Show Square where
    show (J One) = "Δ"
    show (J Two) = "ꖶ"
    show Square = "•"
    show Gap = " "
    show Cross = " "
    show Wall = "█"

data Move = U | D | L | R | UL | UR | DL | DR | WallV (Int,Int) | WallH (Int,Int)
    deriving (Eq)

instance Show Move where
    show U = "8"
    show D = "2"
    show L = "4"
    show R = "6"
    show UL = "7"
    show UR = "9"
    show DL = "1"
    show DR = "3"
    show (WallV (m,n)) = "5" ++ show m ++ show n
    show (WallH (m,n)) = "-" ++ show m ++ show n

instance Read Move where
    readsPrec _ x = [(toEnum (read x :: Int),"")]

instance Enum Move where
    toEnum 1 = DL
    toEnum 2 = D
    toEnum 3 = DR
    toEnum 4 = L
    toEnum 6 = R
    toEnum 7 = UL
    toEnum 8 = U
    toEnum 9 = UR
    toEnum x | x `div` 100 == 5 = WallV ((x-500) `div` 10, (x-500) `mod` 10)
             | x <= 0 = WallH ((-x) `div` 10, (-x) `mod` 10)
    toEnum _ = error "invalid move"
    fromEnum (WallH (m,n)) = -10*m-n
    fromEnum DL = 1
    fromEnum D = 2
    fromEnum DR = 3
    fromEnum L = 4
    fromEnum (WallV (m,n)) = 500+10*m+n
    fromEnum R = 6
    fromEnum UL = 7
    fromEnum U = 8
    fromEnum UR = 9

type Row = [Square]
type Board = [Row]

data Game = Game (Int,Int) Player Board

newtype ShowBoard = ShowBoard Board
newtype ShowRow = ShowRow Row

instance Show ShowBoard where
    show (ShowBoard xs) = f (-1) xs
        where
            f :: Int -> Board -> String
            f _ [] = []
            f (-1) xs = "     0   1   2   3   4   5   6   7\n" ++ f 0 xs
            f n (x:xs)
                | even n = "   " ++ show (ShowRow x) ++ "\n" ++ f (n+1) xs
                | otherwise = " " ++ show (n `div` 2) ++ " " ++ show (ShowRow x) ++ "\n" ++ f (n+1) xs


instance Show ShowRow where
    show (ShowRow []) = ""
    show (ShowRow (Wall:Wall:xs)) = (replicate 2 . head) (show Wall) ++ show (ShowRow (Wall:xs))
    show (ShowRow (x:xs)) = show x ++ " " ++ show (ShowRow xs)

instance Show Game where
    show (Game w One b) = show (ShowBoard b) ++ "\n   " ++ show (J One) ++ " to move (you have " ++ show (fst w) ++ " walls left)\n"
    show (Game w Two b) = show (ShowBoard b) ++ "\n   " ++ show (J Two) ++ " to move (you have " ++ show (snd w) ++ " walls left)\n"
    show (Game _ (Win One) b) = show (ShowBoard b) ++ "   player one wins!!!\n"
    show (Game _ (Win Two) b) = show (ShowBoard b) ++ "   player two wins!!!\n"

newtype Save = Save [Game]

instance Show Save where
    show _ = "You have left the game.\nTo continue playing this game type \"play it\"\nTo start a new game type \"main\""

-- Functions

other :: Player -> Player
other One = Two
other Two = One

boardOf :: Game -> Board
boardOf (Game _ _ b) = b

updateList :: a -> Int -> [a] -> [a]
updateList s 0 (t:ts) = s:ts
updateList s x (t:ts) = t : updateList s (x-1) ts

updateBoard :: Square -> (Int,Int) -> Board -> Board
updateBoard s _ [] = []
updateBoard s (x,0) (ts:tss) = updateList s x ts : tss
updateBoard s (x,y) (ts:tss) = ts : updateBoard s (x,y-1) tss

canSpendWall :: Player -> (Int,Int) -> Bool
canSpendWall One (w1,w2) = w1 > 0
canSpendWall Two (w1,w2) = w2 > 0
canSpendWall _ _ = False

spendWall :: Player -> (Int,Int) -> (Int,Int)
spendWall One (w1,w2) = (w1-1,w2)
spendWall Two (w1,w2) = (w1,w2-1)

moveToCoord :: (Int,Move) -> (Int,Int)
moveToCoord (0,_) = (0,0)
moveToCoord (n,d) = (n*dx,n*dy)
    where (dx,dy) = case d of
                    U -> (0,-1)
                    D -> (0,1)
                    L -> (-1,0)
                    R -> (1,0)
                    UL -> (-1,-1)
                    UR -> (1,-1)
                    DL -> (-1,1)
                    DR -> (1,1)
                    _ -> (0,0)

split :: Move -> (Move,Move)
split UL = (U,L)
split UR = (U,R)
split DL = (D,L)
split DR = (D,R)
split (WallV _) = (U,D)
split (WallH _) = (L,R)
split _ = error "cannor split straight move"

d1 :: Move -> Move
d1 = fst . split

d2 :: Move -> Move
d2 = snd . split

coords :: Move -> Player -> Board -> (Int,Int)
coords (WallV (m,n)) _ _ = (2*m + 1, 2*n + 1) -- wall coords to board coords
coords (WallH (m,n)) _ _ = (2*m + 1, 2*n + 1) -- wall coords to board coords
coords _ p b = head [ (x,y) | y <- [0..16], x <- [0..16], b ! (x,y) == J p ] -- find player on board

(+.+) :: (Int,Int) -> (Int,Int) -> (Int,Int)
(x1,y1) +.+ (x2,y2) = (x1+x2,y1+y2)

(!) :: Board -> (Int,Int) -> Square
b ! (x,y) = b !! y !! x

shift :: (Int,Int) -> (Int,Move) -> (Int,Int)
shift c m = c +.+ moveToCoord m

isStraight :: Move -> Bool
isStraight x | x `elem` [U,D,L,R] = True
isStraight _ = False

isDiagonal :: Move -> Bool
isDiagonal x | x `elem` [UL,UR,DL,DR] = True
isDiagonal _ = False

isWall :: Move -> Bool
isWall (WallV _) = True
isWall (WallH _) = True
isWall _ = False

doMove :: Move -> Game -> Game
doMove m g@(Game w p b)
    | isWall m, b ! c == Cross, check0 d1, check0 d2, canSpendWall p w = placeWall g c m
    | b ! c == J p = case () of
     () | check1 Square                                  -> movePlayer g c (2,m)
        | check1 (J (other p)), b ! shift c (3,m) == Gap -> movePlayer g c (4,m)
        | isDiagonal m, check2 d1 d2 || check2 d2 d1     -> movePlayer g c (2,m)
        | otherwise                                      -> g
    | otherwise = g
    where 
        -- "b ! c == Cross" and the two check0s ensure that the three locations where the wall is to be placed are empty
        check0 :: (Move -> Move) -> Bool -- check if move is valid wall placement
        check0 mtf = b ! shift c (1,mtf m) == Gap -- must not be a wall where wall is to be placed

        check1 :: Square -> Bool -- check if move is valid straight move
        check1 s = 
            isStraight m 
            && b ! shift c (2,m) == s -- must be a certain square at destination
            && b ! shift c (1,m) == Gap -- must not be a wall in direction of move

        check2 :: (Move -> Move) -> (Move -> Move) -> Bool -- check if move is valid diagonal move
        check2 mtf1 mtf2 = 
            isStraight m 
            && b ! shift c (2,mtf1 m)                    == J (other p) -- must be next to other player
            && b ! shift c (1,mtf1 m)                    == Gap -- must not be a wall between players
            && b ! shift (shift c (2,mtf1 m)) (1,mtf2 m) == Gap -- must not be a wall next to other player in direction of move
            && b ! shift c (3,mtf1 m)                    == Wall -- must be a wall behind other player
        movePlayer :: Game -> (Int,Int) -> (Int,Move) -> Game
        movePlayer (Game w p b) c d = Game w (other p) (updateBoard (J p) (shift c d) (updateBoard Square c b))
        placeWall :: Game -> (Int,Int) -> Move -> Game
        placeWall (Game w p b) c m = Game (spendWall p w) (other p) (updateBoard Wall (c +.+ moveToCoord (1,d1 m)) $ updateBoard Wall c $ updateBoard Wall (c +.+ moveToCoord (1,d2 m)) b)
        c :: (Int,Int)
        c = coords m p b

win :: Player -> Game -> Game
win p (Game w _ b) = Game w (Win p) b

move :: Game -> Move -> Game
move g m | J One `elem` (boardOf nextState !! (16 - snd (startCoors One))) = win One nextState
         | J Two `elem` (boardOf nextState !! (16 - snd (startCoors Two))) = win Two nextState
         | otherwise = nextState
         where nextState = doMove m g

zipple :: [a] -> [a] -> [a]
zipple [] _ = []
zipple (x:xs) [] = [x]
zipple (x:xs) (y:ys) = x : y : zipple xs ys

zipplicate17 :: a -> a -> [a]
zipplicate17 s1 s2 = take 17 $ zipple (repeat s1) (repeat s2)

isMove :: String -> Bool
isMove x = x `elem` map (show . (toEnum :: Int -> Move)) moveInts
    where
        moveInts :: [Int]
        moveInts = (sevens (-77))++[1..4]++[6..9]++(sevens 500)
        sevens :: Int -> [Int]
        sevens n = take 64 ([n..n+7] ++ sevens (n+10))

putStrLns :: [String] -> IO ()
putStrLns xs = foldr (>>) (return ()) (map putStrLn xs)

looksLike :: String -> Char -> Bool
looksLike [] _ = False
looksLike (x:_) c = toUpper x == toUpper c

startCoors :: Player -> (Int,Int)
startCoors One = (8,16)
startCoors Two = (8,0)

{-
startCoors :: Player -> (Int,Int)
startCoors One = (8,0)
startCoors Two = (8,16)

startCoors :: Player -> (Int,Int)
startCoors One = (0,16)
startCoors Two = (16,16)
-}

main :: IO (Save)
main = play (Save [])

play :: Save -> IO (Save)
play (Save []) = 
    putStrLns [
        "\nWelcome to Quoridor!",
        "type 'help' for help\n"
    ] >> (
        loop [] $
        Game (10,10) One $
        updateBoard (J One) (startCoors One) $
        updateBoard (J Two) (startCoors Two) $
        zipplicate17
        (zipplicate17 Square Gap)
        (zipplicate17 Gap Cross)
    ) >>= \gs -> return (Save gs)
play (Save (g:gs)) = 
    putStrLns [
        "\nWelcome to Quoridor!",
        "type 'help' for help\n"
        ] >> (loop gs g) >>= \gs -> return (Save gs)

loop :: [Game] -> Game -> IO [Game]
loop gs g =
    print g >> getInput
        where
            getInput :: IO [Game]
            getInput = putStr ">> " >> getLine >>= look

            look :: String -> IO [Game]
            look m | m `looksLike` 'q' = return (g:gs)
                   | m `looksLike` 'h' = 
                     putStrLns [
                        "numbers '1'-'4' & '6'-'9' represent moves in their direction on the numpad", 
                        "'5xy' places a vertical wall at coordinates (x,y)", 
                        "'-xy' places a horizontal wall at coordinates (x,y)",
                        "'Undo' undoes the last move",
                        "'Help' shows this help", 
                        "'Quit' quits game"
                        ] >> getInput
                   | m `looksLike` 'u' = 
                        case gs of 
                            (g_:gs_) -> loop gs_ g_
                            []       -> putStrLn "nothing to undo" >> getInput
                   | isMove m          = loop (g:gs) (move g (read m))
                   | otherwise         = getInput
            