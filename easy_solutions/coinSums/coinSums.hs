data Currency = Pound Int | Pence Int deriving (Show, Ord, Eq)

pound2PenceInt :: Currency -> Int 
pound2PenceInt (Pound x) = x * 100

currencyOp :: Currency -> Currency -> (Int -> Int -> Int) ->Currency 
currencyOp (Pound x) (Pound y) op = Pound (x `op` y)
currencyOp (Pence x) (Pence y) op = Pence (x `op` y)
currencyOp (Pound x) (Pence y) op = Pence $ (x * 100) `op` y

currencies :: [Currency]
currencies = [Pence 1, Pence 2, Pence 5, Pence 10,
              Pence 20, Pence 50, Pound 1, Pound 2]

ways :: Currency -> Int 
ways (Pound total) = []