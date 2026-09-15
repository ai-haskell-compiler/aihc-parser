{- ORACLE_TEST pass -}
{-# LANGUAGE QuantifiedConstraints #-}

module QuantifiedConstraintContextListItems where

class Marker f

class (Marker f, forall a. Eq a => Eq (f a)) => Eq1Wrapper f

class (Marker f, p => q) => Implies f p q

data Wrap f a = Wrap (f a)

instance (Show a, forall b. Show b => Show (f b)) => Show (Wrap f a) where
  show (Wrap x) = show x
