import Data.List (union)

multiples :: (Integral a) => a -> [a] -> [a]
multiples s = filter (\x -> x `mod` s == 0)

main :: IO ()
main = do
    let allMultiples = multiples 3 [1 .. 999] `union` multiples 5 [1 .. 999]
    print (sum allMultiples)
