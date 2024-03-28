import Data.Char

l = ["akesi","ala","ale","ali","anpa","ante","awen","esun","ijo",
     "ike","ilo","insa","jaki","jan","jelo","jo","kala","kalama",
     "kasi","kili","kiwen","ko","kon","kule","kulupu","kute","lape",
     "laso","lawa","len","lete","lili","linja","lipu","loje","luka",
     "lupa","ma","mama","mani","meli","mije","moku","moli","monsi",
     "mun","musi","mute","nanpa","nasa","nasin","nena","ni","nimi",
     "noka","olin","ona","open","pakala","pali","palisa","pan","pana",
     "pilin","pimeja","pipi","poka","poki","pona","pu","seli","selo",
     "seme","sewi","sijelo","sike","sin","sinpin","sitelen","soweli",
     "suli","suno","supa","suwi","telo","tenpo","toki","tomo","tu",
     "unpa","utala","walo","wan","waso","wawa","weka"
     ] ++ pv ++ p ++ nimikusuli ++ c
nimikusuli = ["namako","oko","kipisi","leko","monsuta","tonsi","jasima",
              "kijetesantakalu","soko","meso","epiku","kokosila","lanpan","misikeke"
              ]
pv = ["kama","lukin","wile","alasa","tawa","pini","ken","sona"]
c = ["taso","a","n","mu","kin","."]

p = ["tawa","lon","tan","kepeken","sama"]
pro = ["mi","sina"]
li = ["li"]
la = ["la"]
en = ["en","anu"]
e = ["e"]
et = ["pi"]
o = ["o"]
epsilon = [""]
nimiale = l ++ pro ++ li ++ la ++ e ++ c ++ en ++ et ++ o



states = [0..23] :: [Int]
ok = [0,6,9,10,20,21]
borrowingNode = [2,4,5,6,9,10,12,14,16,18,23]

transitions = [(0,1,epsilon),(1,2,l),(0,21,c),(21,1,epsilon),(1,3,pro),(2,1,la),
               (3,1,la),(3,2,en),(2,4,p),(4,4,l ++ pro),(4,2,l ++ pro),(2,2,l ++ pro),
               (3,5,epsilon),(2,5,li),(5,6,l),(5,19,pv),(19,19,pv),(19,6,epsilon),
               (5,7,p),(6,6,l ++ pro),(6,7,p),(6,21,c),(6,1,en ++ la),(6,8,e),
               (8,10,l ++ pro),(10,10,l ++ pro),(10,8,e),(10,7,p),(10,21,c),(10,1,en ++ la),
               (9,7,p),(7,9,l ++ pro),(9,9,l ++ pro),(9,1,c),(4,11,et),(11,12,l),
               (12,12,l ++ pro),(12,11,et),(12,4,l ++ pro),(2,13,et),(13,14,l),(14,14,l ++ pro),
               (14,13,et),(14,2,l ++ pro),(10,15,et),(15,16,l),(16,16,l ++ pro),(16,15,et),
               (16,10,l ++ pro),(9,17,et),(17,18,l),(18,18,l ++ pro),(18,17,et),(18,9,l ++ pro),
               (1,20,o),(2,20,o),(3,20,o),(20,5,epsilon),(20,21,c),(20,1,en ++ la),
               (6,22,et),(22,23,l),(23,23,l ++ pro),(23,22,et),(23,6,l ++ pro)
               ] :: [(Int,Int,[String])]

trans :: Int -> String -> Bool
trans n " " = n `elem` ok
trans n (' ':xs) = trans n xs
trans n xs | isUpper (head x) && n `elem` borrowingNode = trans n ('p':'u':y)
           | otherwise = or (map (`trans` (tail (y ++ " "))) ints) || or (map (`trans` xs) epsilonInts)
    where 
        ints = [b | (a,b,c) <- transitions, (n == a && x `elem` c)]
        epsilonInts = [b | (a,b,c) <- transitions, (n == a && "" `elem` c)]
        (x,y) = break (==' ') xs

check :: String -> Bool
check = trans 0