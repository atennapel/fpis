-- runhaskell E2_3_4_5.hs

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

-- in Haskell this is an operator (.): compose f g = f . g
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- do nothing
main :: IO ()
main = return ()
