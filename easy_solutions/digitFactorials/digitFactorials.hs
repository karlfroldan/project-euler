factorial :: (Integral a) => a -> a
factorial 1 = 1
factorial x = x * factorial (x - 1)

digitFactorialSum :: (Integral a) => a -> a 
digitFactorialSum x
    | x < 10    = factorial x
    | otherwise = factorial leastSig + digitFactorialSum noLeastSig
    where leastSig   = x `mod` 10
          noLeastSig = x `div` 10

otherNumbers = []