{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, KindSignatures, ScopedTypeVariables, TemplateHaskell, ConstraintKinds #-}

module Main where

import           Data.Maybe
import           Data.Typeable
import           IfCxt
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

mkIfCxtInstances ''Ord

main = defaultMain $ testGroup "All tests" [
    testProperty      "Find Ord for Int"              findOrdInt
  , testProperty      "Find Ord for [Int]"            findOrdListInt
  , testProperty        "No Ord for (Int -> Int)" notFoundOrdFuncInt
  , testProperty        "No Ord for [Int -> Int]" notFoundOrdListFuncInt
  , testProperty       "Get Ord for Int"              haveOrdInt
  , testProperty       "Get Ord for [Int]"            haveOrdListInt
  , testProperty "Can't get Ord for (Int -> Int)"       noOrdFuncInt
  , testProperty "Can't get Ord for [Int -> Int]"       noOrdListFuncInt
  ]

-- Tests

findOrdInt             = ifCxt (Proxy :: Proxy (Ord Int))          True  False
findOrdListInt         = ifCxt (Proxy :: Proxy (Ord [Int]))        True  False
notFoundOrdFuncInt     = ifCxt (Proxy :: Proxy (Ord (Int -> Int))) False True
notFoundOrdListFuncInt = ifCxt (Proxy :: Proxy (Ord [Int -> Int])) False True

haveOrdInt       = isJust    $ getLTE (Proxy :: Proxy Int)
haveOrdListInt   = isJust    $ getLTE (Proxy :: Proxy [Int])
noOrdFuncInt     = isNothing $ getLTE (Proxy :: Proxy (Int -> Int))
noOrdListFuncInt = isNothing $ getLTE (Proxy :: Proxy [Int -> Int])

-- Helpers

getLTE :: forall proxy a. (IfCxt (Ord a)) => proxy a -> Maybe (a -> a -> Bool)
getLTE _ = ifCxt (Proxy :: Proxy (Ord a))
                 (Just (<=))
                 Nothing
