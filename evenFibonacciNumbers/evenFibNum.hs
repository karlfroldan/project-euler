{-
Solving the even `Even Fibonacci Numbers` problem

Problem Statement:
Each new term in the Fibonacci sequence is generated 
by adding the previous two terms. By starting with 1 and 2, 
the first 10 terms will be:
    1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
By considering the terms in the Fibonacci sequence 
whose values do not exceed four million, find the sum 
of the even-valued terms.
-}

-- define the fibonacci sequence
fib :: Int -> Integer
fib = (map fib' [0..] !!) -- We get the nth fib(n) although, curried
    where fib' 1 = 1
          fib' 2 = 2
          fib' n = fib (n - 2) + fib (n - 1)

evenFibNum :: Integer -> [Integer]
evenFibNum n = takeWhile (<= n) fibEvenFiltered
    where fibList = map fib [1..]
          fibEvenFiltered = filter even fibList 
        

main :: IO ()
main = print $ (sum . evenFibNum) 4000000