divides :: Int -> Int -> Bool 
a `divides` b = b `mod` a == 0

smallestMultiple :: Int -> Int
smallestMultiple x = let dividesAll a = all (a `divides`) [1..x]
                     in head [y | y<-[2..], dividesAll y]

main :: IO ()
main = print $ smallestMultiple 10