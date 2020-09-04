{-
https://projecteuler.net/problem=14

The following iterative sequence is defined for the set of positive
 integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following 
sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1) 
contains 10 terms. Although it has not been proved yet (Collatz Problem),
 it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
-}

collatz :: Int -> [Int]
collatz 1 = [1]                 -- if we're on 1, we should halt
collatz n = n : collatz (f n)   -- if we're not yet on 1,
    where f x                   -- append n to the list and do collatz on n
            | even x    = x `div` 2   
            | otherwise = 3 * x + 1
-- maxCollatz takes an input n where it checks every collatz
-- sequences from 1 to n and returns a tuple where (index, length)
maxCollatz :: Int -> (Int, Int) 
maxCollatz n = foldr maxLength (0, 0) [1..n]
    where
        -- acc = (index of acc, length of acc)
        maxLength :: Int -> (Int, Int) -> (Int, Int) 
        maxLength x acc
            | length (collatz x) > snd acc = (x, length $ collatz x)
            | otherwise                    = acc

main :: IO ()
main = print $ maxCollatz 1000000
