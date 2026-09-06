{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
module PromotedTupleConstructor where

type P = '(,) Int Bool

type T = '(,,)
