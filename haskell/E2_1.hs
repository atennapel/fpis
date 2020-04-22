-- runhaskell E2_1.hs

-- Integer is a bigint and will not overflow
fib :: Integer -> Integer
fib n = fibR n 0 1
  where
    fibR 0 a _ = a
    fibR n a b = fibR (n - 1) b (a + b)

-- $ just wraps the rest of the line in ()
main :: IO ()
main = putStrLn $ show $ map fib [0..100]
