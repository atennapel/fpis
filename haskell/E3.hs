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
    loop (Cons h t) | f h = loop t
    loop l = l

init :: List t -> List t
init Nil = Nil
init (Cons _ Nil) = Nil
init (Cons h t) = Cons h (init t)

foldRight :: List a -> b -> (a -> b -> b) -> b
foldRight Nil z f = z
foldRight (Cons h t) z f = f h (foldRight t z f)

length :: List t -> Int
length l = foldRight l 0 (const (+ 1))

foldLeft :: List a -> b -> (b -> a -> b) -> b
foldLeft l z f = loop l z
  where
    loop Nil z = z
    loop (Cons h t) z = loop t (f z h)

sum, product :: List Int -> Int
sum l = foldLeft l 0 (+)
product l = foldLeft l 1 (*)

length2 :: List t -> Int
length2 l = foldLeft l 0 (\a _ -> a + 1)

reverse :: List t -> List t
reverse l = foldLeft l Nil (flip Cons)

foldLeft2 :: List a -> b -> (b -> a -> b) -> b
foldLeft2 l x f = (foldRight l id (\b g x -> g (f x b))) x

foldRight2 :: List a -> b -> (a -> b -> b) -> b
foldRight2 l x f = (foldLeft l id (\g b x -> g (f b x))) x

append :: List a -> List a -> List a
append a b = foldRight a b Cons

flatten :: List (List a) -> List a
flatten l = foldLeft l Nil append

main :: IO ()
main = return ()
