{- ORACLE_TEST pass -}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module RulesPragmaForms where

{-# RULES
"map/map" [2] forall f g xs. map f (map g xs) = map (f . g) xs
"map/id"  [~1] forall xs. map id xs = xs
"fold/build" forall k z (g :: forall b. (a -> b -> b) -> b -> b) . foldr k z (build g) = g k z
"id/type" forall a. forall (x :: a). id @a x = x
"never" [~] forall (x :: Int). negate (negate x) = x
  #-}

{-# RULES "one" forall x. one x = x ; "two" forall y. two y = y #-}

build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

one, two :: Int -> Int
one = id
two = id
