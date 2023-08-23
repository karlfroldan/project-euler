import Data.MemoTrie (HasTrie, memo)

fib :: Integer -> Integer
fib = memo fib'
  where
    fib' 0 = 1
    fib' 1 = 2
    fib' n = fib (n - 2) + fib (n - 1)

evenUntil n = filter even . takeWhile (< n)

main :: IO ()
main = do
    print (sum $ evenUntil 4000000 (fmap fib [0 ..]))
