import Prelude hiding (Monad, return, (>>=))

class (Applicative m) => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

{- Monad Laws
(return x) >>= f ==== f x
m >>= return ==== m
(m >>= f) >>= g ==== m >>= (\x -> f x >>= g)
-}

-- 1. Implement a Monad instance for the list constructor, []. Follow the types!

instance Monad [] where
  -- return :: a -> [a]
  return x = [x]

  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  xs >>= f = [y | x <- xs, y <- f x]

-- 2. Implement a Monad instance for ((->) e)

-- instance Monad ((->) e) where
-- return :: a -> (e -> a)
-- return = const

-- (>>=) :: (e -> a) -> (a -> (e -> b)) -> (e -> b)
--
