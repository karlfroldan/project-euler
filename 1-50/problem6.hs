main :: IO ()
main = do
    let lim = 100
        x = f (^ 2) lim
        y = f id lim ^ 2
    print (y - x)

f g lim = sum (g <$> [1 .. lim])
