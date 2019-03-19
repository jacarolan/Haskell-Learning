
append :: Int -> [Int] -> [Int]
append a [] = [a]
append a (x:xs) = x : append a xs

isDiv :: Int -> Int -> Bool -- Returns true if arg1 | arg0
isDiv a 0 = False
isDiv a b = a `rem` b == 0

addIfDiv :: Int -> [Int] -> [Int]
addIfDiv 0 xs = xs
addIfDiv a xs = if ((isDiv a 5) || (isDiv a 3)) then (append a xs) else xs

addAll :: Int -> [Int] -> [Int]
addAll 0 xs = xs
addAll a xs = (addAll (a-1) (addIfDiv a xs))

sumAll :: [Int] -> Int
sumAll [] = 0
sumAll (x:xs) = (x + (sumAll(xs)))

main = print (sumAll (addAll 999 []))