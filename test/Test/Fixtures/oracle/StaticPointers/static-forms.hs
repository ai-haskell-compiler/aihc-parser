{- ORACLE_TEST pass -}
{-# LANGUAGE StaticPointers #-}
module StaticForms where

import GHC.StaticPtr

ref1 = static 'a'
ref2 = static inc
ref3 = static (inc 1)
ref4 = static (rootRoles . root) <$$> trusted
ref5 = deRefStaticPtr (static inc) 1
ref6 = static inc `seq` ()
ref7 = static Just @Int
ref8 = fmap (static f <$$>) xs
