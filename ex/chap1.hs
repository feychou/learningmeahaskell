doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleUs' x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2
                        
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

is'ValidInFunctions = "Yeah"

-- list comprehensions

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
-- usage boomBangs [7..13]

length' xs = sum[1 | _ <- xs]

describeStuff adjectives nouns = [ adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]
-- usage describeStuff ["lazy","grouchy","scheming"] ["hobo","frog","pope"]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
-- strings are lists

listsOfEven xxs = [ [ x | x <- xs, even x ] | xs <- xxs ]
-- nested list comprehension example / usage listsOfEven [[1, 2, 3, 4], [5, 6, 7, 8]]

-- lists, tuples and triangles

triangles = [ (a, b, c) | c <- [1..10], b <- [1..10], a <- [1..10] ] 
-- all possible triangle with side <= 10

rightTriangles = [ (a, b, c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]
-- right triangles so that b is not larger than c (hypothenuse)
-- and side a is not larger than side b

rightTriangles' = [ (a, b, c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 , a+b+c == 24]
-- adding perimeter = 24 condition
