{- ORACLE_TEST pass -}
{-# LANGUAGE UnboxedTuples #-}
module UnboxedTupleTypeConstructor where

type P = (# , #) Int Bool

type T = (# , , #)
