-- runhaskell E4_6_7.hs

import Prelude hiding (map, traverse, sequence, Either, Left, Right)

data Either a b = Left a | Right b

map :: (a -> b) -> Either e a -> Either e b
map f (Left x) = Left x
map f (Right x) = Right (f x)

flatMap :: (a -> Either e b) -> Either e a -> Either e b
flatMap f (Left x) = Left x
flatMap f (Right x) = f x

orElse :: Either e a -> Either e a -> Either e a
orElse s@(Right x) _ = s
orElse _ x = x

map2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
map2 f (Right x) (Right y) = Right (f x y)
map2 _ (Left x) _ = Left x
map2 _ _ (Left x) = Left x

instance Functor (Either e) where
  fmap = map
instance Applicative (Either e) where
  pure = Right
  (<*>) = map2 id
instance Monad (Either e) where
  (>>=) = flip flatMap

traverse :: (a -> Either e b) -> [a] -> Either e [b]
traverse f [] = Right []
traverse f (x : xs) = do
  hd <- f x
  tl <- traverse f xs
  return (hd : tl)

sequence :: [Either e a] -> Either e [a]
sequence = traverse id

main :: IO ()
main = return ()
