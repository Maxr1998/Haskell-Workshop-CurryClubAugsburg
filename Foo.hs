module Foo where
import Data.Char

{- 4) -}
listLower :: [Char] -> [Bool]
listLower = (map isLower)

{- 4b -}
onlyLower :: [Char] -> Bool
onlyLower = and . listLower

{- 4c -}
countLower :: [Char] -> Int
countLower = length . (filter isLower)

{- 5 -}
-- \x -> x x
-- funktioniert nicht, da "x x" keinen Funktion/keinen Operator besitzt.
-- Möglichkeit, die funktioniert:
square = \x -> x * x

{- 6 -}
fizzbuzz = map fizzstring [1..]
    where
    fizzstring n
        | mod n 3 == 0 && mod n 5 == 0 = "fizz buzz"
        | mod n 3 == 0                 = "fizz"
        | mod n 5 == 0                 = "buzz"
        | otherwise                    = show n

{- 7 -}
maximum' :: [Int] -> Int
maximum' list = foldl max 1 list
    where max = \x y -> if x > y then x else y

{- 8a -}
fib :: Int -> Int
fib n
    | n == 0    = 0
    | n == 1    = 1
    | otherwise = (fib (n - 2)) + (fib (n - 1))

{- 8b -}
{- Performance ↓↓↓ -}

{- 8c -}
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

{- 10 -}
head' :: [a] -> a
head' []       = error "Foo.last': empty list"
head' (x:rest) = x

init' :: [a] -> [a]
init' a = (take (length a - 1) a)

last' :: [a] -> a
last' []     = error "Foo.last': empty list"
last' (x:[]) = x
last' (x:xs) = last' xs

length' :: [a] -> Integer
length' []     = 0
length' (x:xs) = 1 + length' xs

--reverse' :: [a] -> [a]
--reverse' [] = []
--reverse' (x:xs) = reverse' xs : x

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

map' :: (a -> b) -> [a] -> [b]
map' a []       = []
map' a (x:rest) = a x : map' a rest

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' a xs []               = []
zipWith' a [] ys               = []
zipWith' a (x:rest1) (y:rest2) = a x y : zipWith' a rest1 rest2


