--pattern matching

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you are out of luck pal."

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe x = "Not between 1 and 3"

sayMe' :: (Integral a) => a -> String
sayMe' x = "Not between 1 and 3"
sayMe' 1 = "One!"
sayMe' 2 = "Two!"
sayMe' 3 = "Three!"
-- every function calls conforms to the first pattern
-- this function will cause a warning popping up

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
-- pattern matching FAIL

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1)(x2, y2) = (x1 + x2, y1 + y2)
-- with pattern matching 

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z
--first, second and last implementation for triples

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dumbass! Did you even go to school?"
head' (x:_) = x
-- pattern matching against lists

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs
-- length implementation pattern matching + recursion

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
-- as pattern

-- guards

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "Eat more cake."
    | bmi <= 25.0 = "Just keep on eating as much cake as you do."
    | bmi <= 30.0 = "Eat less cake."
    | otherwise   = "Do not even think about cake."
-- body mass indey = weight / height squared

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | weight / height ^ 2 <= 18.5 = "Eat more cake."
    | weight / height ^ 2 <= 25.0 = "Just keep on eating as much cake as you do."
    | weight / height ^ 2 <= 30.0 = "Eat less cake."
    | otherwise                   = "Do not even think about cake."
-- body mass index calculated passing weight and height as params

bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height
    | bmi <= 18.5 = "Eat more cake."
    | bmi <= 25.0 = "Just keep on eating as much cake as you do."
    | bmi <= 30.0 = "Eat less cake."
    | otherwise   = "Do not even think about cake."
    where bmi = weight / height ^ 2
-- binding bmi function

bmiTell''' :: (RealFloat a) => a -> a -> String
bmiTell''' weight height
    | bmi <= skinny = "Eat more cake."
    | bmi <= normal = "Just keep on eating as much cake as you do."
    | bmi <= fat = "Eat less cake."
    | otherwise   = "Do not even think about cake."
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)
-- classy

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b
    
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT
    
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname
          
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2
-- takes a list of weight-height pairs
-- returns a list of BMIs

-- let bindings

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea
    
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- case expressions

head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty list!"
                       (x:_) -> x
