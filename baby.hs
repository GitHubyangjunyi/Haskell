doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y


doubleSmallNumber x = if x > 100
    then x
    else x * 2

--Triangle Matching
v = [(a, b, c) | c <- [1..10], a <- [1..10], b <- [1..10], a ^ 2 + b ^ 2 == c ^ 2, a + b + c == 24, a > b]


--Factorial
ff :: Integer -> Integer
ff n = product [1..n]

--Catchall Pattern
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN"
lucky x = "NOT"

luckyx :: Char -> String
luckyx '7' = "LUCKY NUMBER SEVEN"
luckyx 'x' = "X"
luckyx x = "NOT"

--As Pattern Matching
fir :: String -> String
fir "" = "Empty"
fir all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

--Guard
bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"

--Next Pattern Matching
bmiTellPattern :: Double -> String
bmiTellPattern bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
bmiTellPattern otherwise = "You're a whale, congratulations!"

--Where
bmiTellWhere :: Double -> Double -> String
bmiTellWhere weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

--Where Pattern Matching
bmiTellWherePattern :: Double -> Double -> String
bmiTellWherePattern weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)


initials :: String -> String -> String
initials firstname lastname = show f ++ ". " ++ show l ++ "."
    where (f:_) = firstname
          (_:l) = lastname


--Where Func
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmic w h | (w, h) <- xs]
    where bmic weight height = weight / height ^ 2




--Let
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + topArea * 2

leta = 4 * (let a = 9 in a + 1) + 2

letb = [let square x = x * x in (square 5, square 6, square 7)]

letc = (let a = 100; b = 200; c = 300 in a * b * c, let foo = "Hey "; bar = "there!" in foo ++ bar)

lett = (let (a, b, c) = (1, 2, 3) in a + b + c) * 100



calcBmisLet :: [(Double, Double)] -> [Double]
calcBmisLet xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]


--Case Of
headc :: [a] -> a
headc xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x


describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeListPattern :: [a] -> String
describeListPattern ls = "The list is " ++ what ls
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."


--Quick Sort
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger         = [a | a <- xs, a > x]
    in quickSort smallerOrEqual ++ [x] ++ quickSort larger



qs = quickSort [1, 3, 4, 12, 45, 6, 4, 11, 3, 666, 8, 11]
--subFour :: Int -> Int
subFour = (subtract 4)

quickSortf :: (Ord a) => [a] -> [a]
quickSortf [] = []
quickSortf (x : xs) =
    let smallerOrEqual = filter (<= x) xs
        larger         = filter (> x) xs
    in quickSortf smallerOrEqual ++ [x] ++ quickSortf larger



qsf = quickSortf [1, 3, 4, 12, 45, 6, 4, 11, 3, 666, 8, 11]

--
--let notNull x = not (null x) in filter notNull [[1, 2], []]



--
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map (sqrt) [1..]))) + 1


--sum (map sqrt [1..130])
--sum (map sqrt [1..131])



--ThreeTwoOne
--replicate 2 (product (map (*3) (zipWith max [1, 2] [4, 5])))

--replicate 2 . product . map (*3) $ zipWith max [1, 2] [4, 5]


















{-



-}