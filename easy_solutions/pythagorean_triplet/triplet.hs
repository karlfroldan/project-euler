--A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

import Text.Printf
import Control.Exception
import System.CPUTime

fst' :: (a, a, a) -> a
fst' (x,_,_) = x
snd' :: (a, a, a) -> a
snd' (_,x,_) = x
trd  :: (a, a, a) -> a
trd  (_,_,x) = x

prod :: Int -> Int
prod a = product tuple2List
    where tuple2List = [fst' triplet, snd' triplet, trd triplet]
          triplet = head [(x, y, z) | z <- [1..], 
                                      y <- [1..z-1], 
                                      x <- [1..y-1], 
                                      x^2 + y^2 == z^2,
                                      x + y + z == a]

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

main :: IO ()
main = do
    putStrLn "Starting..."
    time $ prod 1000 `seq` return ()
    putStrLn "Done."