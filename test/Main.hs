{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, KindSignatures, ScopedTypeVariables, TemplateHaskell, ConstraintKinds #-}

module Main where

import           Data.Maybe
import           Data.Typeable
import           IfCxt
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

mkIfCxtInstances ''Ord

main = defaultMain $ testGroup "All tests" [
    testProperty "Find Ord for Int"   haveLTEInt
  , testProperty "Find Ord for [Int]" haveLTEListInt
  ]

-- Tests

haveLTEInt     = isJust $ getLTE (Proxy :: Proxy Int)
haveLTEListInt = isJust $ getLTE (Proxy :: Proxy [Int])

-- Helpers

getLTE :: forall proxy a. (IfCxt (Ord a)) => proxy a -> Maybe (a -> a -> Bool)
getLTE _ = ifCxt (Proxy :: Proxy (Ord a))
                 (Just (<=))
                 Nothing
