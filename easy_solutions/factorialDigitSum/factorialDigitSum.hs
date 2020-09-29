factorial :: (Integral a) => a -> a
factorial 1 = 1
factorial x = x * factorial (x - 1)

digitSum :: (Integral a) => a -> a 
digitSum x
    | x < 10    = x
    | otherwise = leastSig + digitSum noLeastSig
    where leastSig   = x `mod` 10
          noLeastSig = x `div` 10

main :: IO ()
main = print $ digitSum $ factorial 100