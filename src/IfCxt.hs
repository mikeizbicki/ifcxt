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
               else mkInstance cxt classt t n
        otherwise -> fail $ show n ++ " is not a class name."

mkInstance :: Cxt -> Type -> Type -> Name -> [Dec]
mkInstance cxt classt t n = [
    InstanceD
        (map relaxCxt cxt)
        (relaxCxt (AppT (ConT n) t))
        [ FunD 'ifCxt
            [ Clause
                [ VarP $ mkName "proxy"
                , VarP $ mkName "t"
                , VarP $ mkName "f"
                ]
                (NormalB (mkIfCxtFun cxt))
                []
            ]
        ]
    ]

-- | "Relax" constraints by wrapping in "IfCxt".
relaxCxt :: Type -> Type
relaxCxt t@(AppT (ConT c) _) | c == ''IfCxt = t
relaxCxt t                                  = AppT (ConT ''IfCxt) t

-- | Creates an implementation of "ifCxt". If our instance has no extra
-- constraints, e.g. deriving "IfCxt (Show Bool)" from "Show Bool", we simply
-- return the first argument.
--
-- If we have extra constraints, e.g. deriving
-- "IfCxt (Show a) => IfCxt(Show [a])" from "Show a => Show [a]", we call
-- "ifCxt" recursively to bring those instances in scope. We only return the
-- first argument if all constraints are satisfied.
mkIfCxtFun :: Cxt -> Exp
mkIfCxtFun []     = VarE $ mkName "t"
mkIfCxtFun (c:cs) = AppE (AppE (AppE (VarE 'ifCxt)
                                     proxy)
                               (mkIfCxtFun cs))
                         (VarE $ mkName "f")
    where proxy = SigE (ConE 'Proxy)
                       (AppT (ConT ''Proxy) c)
