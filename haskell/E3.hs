-- runhaskell E3.hs

-- hide functions from the standard library
import Prelude hiding (drop, dropWhile, init)

data List t = Nil | Cons t (List t)
  deriving (Show)

tail :: List t -> List t
tail Nil = Nil
tail (Cons _ t) = t

setHead :: List t -> t -> List t
setHead Nil h = Nil
setHead (Cons _ t) h = Cons h t

drop :: List t -> Int -> List t
drop Nil _ = Nil
drop l 0 = l
drop (Cons _ t) n = drop t (n - 1)

dropWhile :: List t -> (t -> Bool) -> List t
dropWhile l f = loop l
  where
    loop Nil = Nil
    loop l@(Cons h t) = if f h then loop t else l

init :: List t -> List t
init Nil = Nil
init (Cons _ Nil) = Nil
init (Cons h t) = Cons h (init t)

main :: IO ()
main = return ()
