{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module AtOperatorInParens where

-- Regression test: '(@)' is an ordinary parenthesized varsym.
--
-- A tight '@' (no preceding whitespace) lexes as TkReservedAt, and
-- operatorExprNameParser used to reject that token, so '(@)' failed to parse
-- even though GHC accepts it and only rejects it later, in the renamer.
-- The pretty-printer already rendered EVar '@' as '(@)', so any generated AST
-- containing that name failed to round-trip.

f = (@)

g = (@) 1 2

h = (@ ())

i = $(@)

j x = x @Int
