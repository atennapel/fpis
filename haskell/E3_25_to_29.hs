-- runhaskell E3_25_to_29.hs

import Prelude hiding (map, maximum)

data Tree t = Leaf t | Branch (Tree t) (Tree t)
  deriving (Show)

size :: Tree t -> Int
size (Leaf _) = 1
size (Branch a b) = 1 + (size a) + (size b)

maximum :: Ord t => Tree t -> t
maximum (Leaf x) = x
maximum (Branch a b) = max (maximum a) (maximum b)

depth :: Tree t -> Int
depth (Leaf _) = 0
depth (Branch a b) = 1 + (max (depth a) (depth b))

map :: (a -> b) -> Tree a -> Tree b
map f (Leaf x) = Leaf (f x)
map f (Branch a b) = Branch (map f a) (map f b)

fold :: (b -> b -> b) -> (a -> b) -> Tree a -> b
fold f i (Leaf x) = i x
fold f i (Branch a b) = f (fold f i a) (fold f i b)

size2 :: Tree t -> Int
size2 = fold (\a b -> 1 + a + b) (const 1)

maximum2 :: Ord t => Tree t -> t
maximum2 = fold max id

depth2 :: Tree t -> Int
depth2 = fold (\a b -> 1 + (max a b)) (const 0)

map2 :: (a -> b) -> Tree a -> Tree b
map2 f = fold Branch (Leaf . f)

main :: IO ()
main = return ()
