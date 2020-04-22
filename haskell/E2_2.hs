-- runhaskell E2_2.hs

isSorted :: [a] -> (a -> a -> Bool) -> Bool
isSorted as ordered = loop as
  where
    loop [] = True
    loop [x] = True
    loop (a : b : rest) = ordered a b && loop (b : rest)

main :: IO ()
main = putStrLn $ show $ (isSorted [1, 2, 3, 4, 5] (<=), isSorted [1, 3, 2, 5, 4] (<=))
