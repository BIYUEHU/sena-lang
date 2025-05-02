{-# LANGUAGE RankNTypes #-}

foo :: (forall a. a -> a) -> Int
foo f = f 1

f = Nothing

newtype A1 = A1 (Int -> Int)

type A2 = Int -> Int

g :: A1 -> Int
g _ = 1

h :: A2
h _ = 1

result1 = g $ A1 h

result2 = h 1