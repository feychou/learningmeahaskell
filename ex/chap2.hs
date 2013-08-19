-- type declaration

removeNonUppercase :: [Char] -> [Char] -- maps from a string to a string
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z'] ]

addThree :: Int -> Int -> Int -> Int
-- 3 ints as parameters
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]
-- try it with 50! Integer is like, super big

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r
