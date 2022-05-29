-- https://projecteuler.net/problem=25

fib :: Int -> Integer 
fib = (map fib' [0..] !!)
    where fib' 1 = 1 
          fib' 2 = 1 
          fib' n = fib (n - 1) + fib (n - 2)

countDigits :: Integral a => a -> a
countDigits n 
    | n < 10    = 1 
    | otherwise = 1 + countDigits (n `div` 10)


first1000 :: Int -> Int 
first1000 n 
    | m < 1000  = first1000 (n + 1)
    | otherwise = n
    where m = countDigits . fib $ n

main :: IO ()
main = print . first1000 $ 1