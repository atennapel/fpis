-- runhaskell E4_1_to_5.hs

import Prelude hiding (map, traverse, sequence)

data Option t = None | Some t

map :: (a -> b) -> Option a -> Option b
map f None = None
map f (Some x) = Some (f x)

getOrElse :: a -> Option a -> a
getOrElse x None = x
getOrElse _ (Some x) = x 

orElse :: Option a -> Option a -> Option a
orElse s@(Some _) _ = s
orElse _ s = s

flatMap :: (a -> Option b) -> Option a -> Option b
flatMap f None = None
flatMap f (Some x) = f x

map2 :: (a -> b -> c) -> Option a -> Option b -> Option c
map2 f (Some x) (Some y) = Some (f x y)
map2 _ None _ = None
map2 _ _ None = None

filter :: (a -> Bool) -> Option a -> Option a
filter f s@(Some x) | f x = s
filter f _ = None

mean :: [Double] -> Option Double
mean [] = None
mean xs = Some (sum xs / (fromIntegral $ length xs))

-- to use do-notation we have implement these type-classes
instance Functor Option where
  fmap = map
instance Applicative Option where
  pure = Some
  (<*>) = map2 id
instance Monad Option where
  (>>=) = flip flatMap

variance :: [Double] -> Option Double
variance xs = do
  m <- mean xs
  mean (fmap (\x -> (x - m) ** 2) xs)

sequence :: [Option a] -> Option [a]
sequence [] = Some []
sequence (x : xs) = do
  y <- x
  ys <- sequence xs
  return (y : ys)

traverse :: (a -> Option b) -> [a] -> Option [b]
traverse f [] = Some []
traverse f (x : xs) = do
  hd <- f x
  tl <- traverse f xs
  return (hd : tl)

sequence2 :: [Option a] -> Option [a]
sequence2 = traverse id

main :: IO ()
main = return ()
