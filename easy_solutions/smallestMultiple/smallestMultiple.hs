exchange :: (Num a, Ord a) => a -> a -> (a, a)
exchange a b 
    | a <= b    = (b, a)
    | otherwise = (a, b)

euclideanGcd :: (Integral a) => (a, a) -> a 
euclideanGcd (n, 0) = n 
euclideanGcd (n1, n2) = euclideanGcd $ exchange n2 n 
    where n = n1 `mod` n2

myLcm :: (Integral a) => (a, a) -> a 
myLcm (x, y) = (x * y) `div` euclideanGcd(x, y)

multiLcm :: (Integral a) => a -> a
multiLcm x = multiLcm' [1..x]
    where multiLcm' = foldl (\acc x -> myLcm (x, acc)) 1

main = do
    ln <- getLine
    let lcm' = (multiLcm . read) ln 
    print lcm'