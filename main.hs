doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100  
                      then x  
                      else x*2

doubleSmallNumber' x = (if x > 100 
                        then x 
                        else x*2) 
                        + 1

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [2 | _ <- xs]


--------------------------------
{- String or [Char]
removeNonUppercase :: [Char] -> [Char] -}
removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

--Int
addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z

--Integer == Int without Max nor Min value
factorial' :: Int -> Int  
factorial' n = product [1..n]

--Float
circumference :: Float -> Float  
circumference r = 2 * pi * r

--Double == Float, with dougle the precision
circumference' :: Double -> Double
circumference' r = 2 * pi * r

--Bool

--Char

--------------------------------

--Tuples ()

--List []                           

--------------------------------

--Type Variable (generics) test
-- Typeclasses test
getFstParam :: (Eq a) => a -> a -> Bool
getFstParam x y = x == y

--Pattern Matching
lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"  

--Pattern Matching
--Recursiveness
factorial :: (Integral a) => a -> a    
factorial 0 = 1
factorial n = n * factorial (n - 1)


--Needs a "catch-all" pattern
charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"


addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors' a b = (fst a + fst b, snd a + snd b)


--Pattern Matching version
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

--Pattern Matching
first :: (a, b, c) -> a  
first (x, _, _) = x  
  
--Pattern Matching  
second :: (a, b, c) -> b  
second (_, y, _) = y  

--Pattern Matching
third :: (a, b, c) -> c  
third (_, _, z) = z

head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  --creates a runtime error
head' (x:_) = x

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length'' :: (Integral b) => [a] -> b  
length'' [] = 0  
length'' (_:xs) = 1 + length'' xs

--sum' :: (Num a) => [a] -> a  
--sum' [] = 0  
--sum' (x:xs) = x + sum' xs

sum' :: (Num a) => [a] -> a     
sum' = foldl1 (+)

--as pattern (@)
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

--guards
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"

--guards
bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                   = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a  
max' a b   
    | a > b     = a  
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT    


myCompare' :: (Ord a) => a -> a -> Ordering
myCompare' a b = if a > b
                 then GT
                 else
                 if a < b
                 then LT
                 else
                 EQ

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]  
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys    

elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

