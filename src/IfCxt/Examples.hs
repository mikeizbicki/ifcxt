{-# LANGUAGE KindSignatures, ScopedTypeVariables, MultiParamTypeClasses #-}
-- | Some motivating examples for the "IfCxt" module.
module IfCxt.Examples
    ( cxtShow
    , cxtShowTypeable
    , cxtNub
    , Magic
    , magic
    )
    where

import IfCxt
import Data.List
import Data.Typeable

mkIfCxtInstances ''Ord
mkIfCxtInstances ''Show
mkIfCxtInstances ''Typeable

-- | A version of "show" that can be called on any type.
-- If the type is not an instance of "Show", then @<<unshowable>>@ gets displayed.
cxtShow :: forall a. IfCxt (Show a) => a -> String
cxtShow a = ifCxt (Proxy::Proxy (Show a))
    (show a)
    "<<unshowable>>"

-- | Like "cxtShow" above, but if @a@ is not an instance of "Show" then we print out the type.
cxtShowTypeable :: forall a.
    ( IfCxt (Show a)
    , IfCxt (Typeable a)
    ) => a -> String
cxtShowTypeable = ifCxt (Proxy::Proxy (Show a))
    show
    ( ifCxt (Proxy::Proxy (Typeable a))
        (\a -> "<<"++show (typeOf a) ++">>")
        (const "<<unshowable>>")
    )

-- | A version of "nub" that is maximally efficient for the given type.
-- If we only have an "Eq" constraint, then "cxtNub" takes time @O(n^2)@,
-- but if we also have an "Ord" constraint, then "cxtNub" only takes time @O(n*log n)@.
-- If the type @a@ does have an "Ord" constraint, then the order of the elements may change.
cxtNub :: forall a. (Eq a, IfCxt (Ord a)) => [a] -> [a]
cxtNub = ifCxt (Proxy::Proxy (Ord a)) nubOrd nub

nubOrd :: Ord a => [a] -> [a]
nubOrd = go . sort
    where
        go (x1:x2:xs)
            | x1==x2    =      go (x2:xs)
            | otherwise = x1 : go (x2:xs)
        go [x] = [x]
        go []  = []

class Magic

magic :: Int
magic = ifCxt (Proxy::Proxy Magic) 1 2
