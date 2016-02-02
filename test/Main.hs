{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, KindSignatures, ScopedTypeVariables, TemplateHaskell, ConstraintKinds #-}

module Main where

import           Data.Maybe
import           Data.Typeable
import           IfCxt
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

mkIfCxtInstances ''Ord

main = defaultMain $ testGroup "All tests" [
    testProperty "Find Ord for Int"        haveOrdInt
  , testProperty "Find Ord for [Int]"      haveOrdListInt
  , testProperty "No Ord for (Int -> Int)" noOrdFuncInt
  ]

-- Tests

haveOrdInt     = isJust $ getLTE (Proxy :: Proxy Int)
haveOrdListInt = isJust $ getLTE (Proxy :: Proxy [Int])

noOrdFuncInt = isNothing $ getLTE (Proxy :: Proxy (Int -> Int))

-- Helpers

getLTE :: forall proxy a. (IfCxt (Ord a)) => proxy a -> Maybe (a -> a -> Bool)
getLTE _ = ifCxt (Proxy :: Proxy (Ord a))
                 (Just (<=))
                 Nothing
