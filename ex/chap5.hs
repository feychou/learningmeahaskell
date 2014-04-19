-- curried functions

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: (Num a, Ord a) => a -> Ordering  
compareWithHundred' = compare 100  
-- compare 100 returns a function that compares a number to 100
-- therefore we can omit the x param

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])
-- same applies for infix functions
-- use subtract and not the minus sign for subtraction

-- high order functions

applyTwice :: (a -> a) -> a -> a
-- parentheses mandatory (first param is a function)
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- possible usage: max [6, 3, 2] [3, 4, 9] returns [6, 4, 9]

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y
-- try with zipWith (flip' div) [2,2..] [10,8,6,4,2]
-- fun fun fun

-- map
addThree :: (Num a) => [a] -> [a]
addThree x = map (+3) x

listOfFirsts :: (Ord a, Ord b) => [(a, b)] -> [a]
listOfFirsts x = map fst x

-- filter
filterGreaterThan :: (Num a, Ord a) => a -> [a] -> [a]
filterGreaterThan x y = filter (>x) y

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted
-- quicksort with filter replacing list comprehension

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0
    
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n  = n:chain (n*3 + 1)
-- collatz sequences

numLongChains :: Int
numLongChains = length(filter isLong (map chain[1..100]))
    where isLong xs = length xs > 15
    
-- lambdas

numLongChains' :: Int  
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))
-- same function as before using lambda

addThreeNumbers :: (Num a) => a -> a -> a -> a
addThreeNumbers x y z = x + y + z

addThreeNumbers' :: (Num a) => a -> a -> a -> a
addThreeNumbers' = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> b -> a -> c  
flip'' f = \x y -> f y x

-- folds

-- left fold
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- right fold
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
-- prepend less expensive than append

-- so many ways to use folds!
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

{-| 
  How many elements does it take for the sum of 
  the roots of all natural numbers to exceed 1000?
-}
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
