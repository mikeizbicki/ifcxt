module IfCxt
    ( IfCxt (..)
    , mkIfCxtInstances
    )
    where

import Control.Monad
import Data.Proxy
import Language.Haskell.TH.Syntax

-- | This class lets a polymorphic function behave differently depending on the constraints of the polymorphic variable.
-- For example, we can use it to write the following improved version of the "show" function:
--
-- > ifShow :: forall a. IfCxt (Show a) => a -> String
-- > ifShow = ifCxt (Proxy::Proxy (Show a))
-- >   show
-- >   (const "<<unshowable>>")
--
-- In ghci, we can run:
--
-- >>> ifShow (1 :: Int)
-- "1"
-- >>> ifShow (id :: a -> a)
-- "<<unshowable>>"
--
-- The "IfCxt.Examples" module contains more examples.
class IfCxt cxt where
    ifCxt :: proxy cxt -> (cxt => a) -> a -> a

instance {-# OVERLAPPABLE #-} IfCxt cxt where ifCxt _ t f = f

-- | Derives all possible instances of "IfCxt" for the given one parameter type class.
mkIfCxtInstances :: Name -> Q [Dec]
mkIfCxtInstances n = do
    info <- reify ''IfCxt
    let instancesOfIfCxt = case info of
            ClassI _ xs -> map (\(InstanceD _ (AppT _ t) _) -> t) xs

        isInstanceOfIfCxt t = t `elem` instancesOfIfCxt

    info <- reify n
    case info of
        ClassI _ xs -> fmap concat $ forM xs $ \(InstanceD cxt (AppT classt t) ys) -> return $
            if isInstanceOfIfCxt (AppT classt t)
                then []
                else [ InstanceD
                    cxt
                    (AppT
                        (ConT ''IfCxt)
                        (AppT
                            (ConT n)
                            t
                        )
                    )
                    [ FunD 'ifCxt
                        [ Clause
                            [ VarP $ mkName "proxy"
                            , VarP $ mkName "t"
                            , VarP $ mkName "f"
                            ]
                            ( NormalB ( VarE $ mkName "t") )
                            []
                        ]
                    ]
                ]

        otherwise -> fail $ show n ++ " is not a class name."
