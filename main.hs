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
factorial :: Integer -> Integer  
factorial n = product [1..n]

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


