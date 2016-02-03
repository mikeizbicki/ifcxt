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
  , testProperty   "Can use Ord for Int"            usableOrdInt
  , testProperty   "Can use Ord for [Int]"          usableOrdListInt
  , testProperty   "Can use Ord for (Int -> Int)" unusableOrdFuncInt
  , testProperty   "Can use Ord for [Int -> Int]" unusableOrdListFuncInt
  ]

-- Tests

findOrdInt             = ifCxt (Proxy :: Proxy (Ord Int))          True  False
findOrdListInt         = ifCxt (Proxy :: Proxy (Ord [Int]))        True  False
notFoundOrdFuncInt     = ifCxt (Proxy :: Proxy (Ord (Int -> Int))) False True
notFoundOrdListFuncInt = ifCxt (Proxy :: Proxy (Ord [Int -> Int])) False True

haveOrdInt       = isJust    $ getLTE (Proxy :: Proxy (Ord Int))
haveOrdListInt   = isJust    $ getLTE (Proxy :: Proxy (Ord [Int]))
noOrdFuncInt     = isNothing $ getLTE (Proxy :: Proxy (Ord (Int -> Int)))
noOrdListFuncInt = isNothing $ getLTE (Proxy :: Proxy (Ord [Int -> Int]))

usableOrdInt       (x :: Int)   y =   usableLTE x y
usableOrdListInt   (x :: [Int]) y =   usableLTE x y
unusableOrdFuncInt (x :: Int)   y = unusableLTE (+x) (*y)
unusableOrdListFuncInt            = unusableLTE ([] :: [Int -> Int]) []

-- Helpers

getLTE :: forall proxy a. IfCxt (Ord a) => proxy (Ord a) -> Maybe (a -> a -> Bool)
getLTE _ = ifCxt (Proxy :: Proxy (Ord a))
                 (Just (<=))
                 Nothing

useLTE :: (IfCxt (Ord a)) => a -> a -> Maybe Bool
useLTE x y = getLTE (Proxy :: Proxy (Ord a)) <*> pure x <*> pure y

usableLTE :: (IfCxt (Ord a), Ord a) => a -> a -> Bool
usableLTE x y = useLTE x y == Just (x <= y)

unusableLTE :: (IfCxt (Ord a)) => a -> a -> Bool
unusableLTE x y = useLTE x y == Nothing
