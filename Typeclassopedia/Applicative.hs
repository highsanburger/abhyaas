{-# LANGUAGE UndecidableInstances #-}

import Prelude hiding (Applicative, pure, (<*>))

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

{-
1. (Tricky) One might imagine a variant of the interchange law that says something about applying a pure function to an effectful argument. Using the above laws, prove that

LAWS :-

I   : pure id <*> v = v                             Identity
II  : pure f <*> pure x = pure (f x)                Homomorphism
III : u <*> pure y = pure (\f -> f y) <*> u         Interchange
IV  : u <*> (v <*> w) = pure (.) <*> u <*> v <*> w  Composition

pure f <*> x = pure (flip ($)) <*> x <*> pure f

Ans.
RHS = pure (flip ($)) <*> x <*> pure f

LHS = pure f <*> x
-}

-- 2. Implement an instance of Applicative for Maybe.

instance Applicative Maybe where
  -- pure :: a -> Maybe a
  pure = Just

  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Just g <*> Just x = Just (g x)
  Nothing <*> _ = Nothing

-- 3. Determine the correct definition of pure for the ZipList instance of Applicative—there is only one implementation that satisfies the law relating pure and (<*>).
--
newtype ZipList a = ZipList {getZipList :: [a]} deriving (Show)

instance Functor ZipList where
  fmap g (ZipList as) = ZipList (fmap g as)

instance (Functor ZipList) => Applicative ZipList where
  pure :: a -> ZipList a
  pure x = ZipList (repeat x)

  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)
