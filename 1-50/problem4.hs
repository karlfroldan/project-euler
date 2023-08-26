import Data.Foldable (maximumBy)

isPalindrome' :: (Integral a) => a -> a -> Bool
isPalindrome' n nDigits
    | n == 0 = True
    | nDigits == 1 = True
    | nHead == nLast = isPalindrome' n' (nDigits - 2)
    | otherwise = False
  where
    nHead = n `div` (10 ^ (nDigits - 1))
    nLast = n `mod` 10
    n' = (n `div` 10) `mod` (10 ^ (nDigits - 2))

isPalindrome :: (Integral a) => a -> Bool
isPalindrome n = isPalindrome' n nDigits
  where
    nDigits = countDigits n

countDigits 0 = 0
countDigits n = 1 + countDigits (n `div` 10)

main :: IO ()
main = print (maximumBy (\x y -> third x `compare` third y) palindromicProducts)

third :: (a, b, c) -> c
third (_, _, x) = x

threeDigitNums = [100 .. 999]
products = [(x, y, x * y) | x <- threeDigitNums, y <- threeDigitNums]
palindromicProducts = filter (\(x, y, p) -> isPalindrome p) products
