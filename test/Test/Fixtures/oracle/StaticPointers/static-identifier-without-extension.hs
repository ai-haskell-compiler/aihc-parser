{- ORACLE_TEST pass -}
module StaticIdentifierWithoutExtension where

static :: Int -> Int
static x = x + 1

y = static 1
