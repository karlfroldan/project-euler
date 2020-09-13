isPalindrome :: String -> Bool 
isPalindrome s = s == reverse s

palProduct :: Int 
palProduct = maximum [x * y | x <- [100..999]
                            , y <- [100..999]
                            , isPalindrome $ show (x * y)]

main :: IO ()
main = print palProduct