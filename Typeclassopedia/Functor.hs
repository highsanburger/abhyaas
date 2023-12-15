import Prelude hiding (Functor, fmap, (<$))

-- 1. Implement Functor instances for Either e and ((->) e).

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  (<$) = fmap . const

data Ether x y = Liberal x | Conservative y deriving (Show)

-- has kind * -> * -> * but only kinds of * -> * can be instances of Functor

instance Functor (Ether x) where
  -- fmap :: (a -> b) -> (Ether x a) -> (Ether x b)
  fmap _ (Liberal x) = Liberal x
  fmap g (Conservative y) = Conservative (g y)

  _ <$ Liberal x = Liberal x
  z <$ Conservative y = Conservative z

instance Functor ((->) e) where
  fmap = (.)

-- 2. Implement Functor instances for ((,) e) and for Pair, defined as

instance Functor ((,) e) where
  -- fmap :: (a -> b) -> (c,a) -> (c,b)
  fmap g (x, y) = (x, g y)

data Pair a = Pair a a deriving (Show)

instance Functor Pair where
  -- fmap  :: (a -> b) -> (Pair a) -> (Pair b)
  fmap g (Pair x y) = Pair (g x) (g y)

-- Explain their similarities and differences.

-- Pair can only have two values of same type but tuple can have any two type.
-- Fmap maps over both values of Pair but fmap only maps over seconds element of tuple.

-- 3. Implement a Functor instance for the type ITree, defined as
data ITree a = Leaf (Int -> a) | Node [ITree a]

instance Functor ITree where
  -- fmap :: (a -> b) -> (ITree a) -> (ITree b)
  fmap g (Leaf h) = Leaf (g . h)
  fmap g (Node subtrees) = Node (map (fmap g) subtrees)

{-
4. Give an example of a type of kind * -> * which cannot be made an instance of Functor (without using undefined).

data K a = K (a -> Int)

5. Is this statement true or false?
The composition of two Functors is also a Functor.
If false, give a counterexample; if true, prove it by exhibiting some appropriate Haskell code.

Since Functors form a category, it is True.

6. Although it is not possible for a Functor instance to satisfy the first Functor law but not the second (excluding undefined), the reverse is possible. Give an example of a (bogus) Functor instance which satisfies the second law but not the first.

Functor Laws :-
fmap id = id
fmap (g . h) = (fmap g) . (fmap h)
-}

data Bogus a where
  Bogus :: a -> Bogus a
  deriving (Show, Eq)

instance Functor Bogus where
  fmap g (Bogus x) = Bogus (g x)

-- Example function violating the first law
example1 :: Bogus Int -> Bool
example1 bx = fmap id bx /= bx

-- Example function satisfying the second law
example2 :: (Int -> Int) -> (Int -> Int) -> Bogus Int -> Bool
example2 g h bx = fmap (g . h) bx == (fmap g . fmap h) bx

-- Test the examples
main :: IO ()
main = do
  let bogusInstance = Bogus 42
  putStrLn $ "Example 1: " ++ show (example1 bogusInstance)
  putStrLn $ "Example 2: " ++ show (example2 (+ 1) (* 2) bogusInstance)

-- 7. Which laws are violated by the evil Functor instance for list shown above: both laws, or the first law alone? Give specific counterexamples.

-- Evil Functor instance
instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap _ [] = []
  fmap g (x : xs) = g x : g x : fmap g xs

{-
 violation of First Functor Law
 fmap id [1] ~~> [1,1]
 but, id [1] ~~> [1]

violation of Second Functor Law
♾️  > g = (+1)
♾️  > h = (*2)
♾️  > fmap (g . h) [1,2]
[3,3,5,5]
♾️  > (fmap g) . (fmap h) $ [1,2]
[3,3,3,3,5,5,5,5]
-}
