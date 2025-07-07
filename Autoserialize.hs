{-# LANGUAGE TemplateHaskell #-}
module Autoserialize where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

class EncTup a where
    encTup :: a -> (Int, Int)


typeInfo :: DecQ -> Q (Name, [Name], [(Name, Int)], [(Name, [(Maybe Name, Type)])])
typeInfo m =
     do d <- m
        case d of
           d@(DataD {}) ->
            return (simpleName $ name d, paramsA d, consA d, termsA d)
           d@(NewtypeD {}) ->
            return (simpleName $ name d, paramsA d, consA d, termsA d)
           _ -> error ("derive: not a data type declaration: " ++ show d)

     where
        consA (DataD _ _ _ _ cs _)    = map conA cs
        consA (NewtypeD _ _ _ _ c _)  = [ conA c ]
        paramsA (DataD _ _ ps _ _ _) = map nameFromTyVar ps
        paramsA (NewtypeD _ _ ps _ _ _) = map nameFromTyVar ps

        nameFromTyVar (PlainTV a _) = a
        nameFromTyVar (KindedTV a _ _) = a


        termsA (DataD _ _ _ _ cs _) = map termA cs
        termsA (NewtypeD _ _ _ _ c _) = [ termA c ]

        termA (NormalC c xs)        = (c, map (\x -> (Nothing, snd x)) xs)
        termA (RecC c xs)           = (c, map (\(n, _, t) -> (Just $ simpleName n, t)) xs)
        termA (InfixC t1 c t2)      = (c, [(Nothing, snd t1), (Nothing, snd t2)])

        conA (NormalC c xs)         = (simpleName c, length xs)
        conA (RecC c xs)            = (simpleName c, length xs)
        conA (InfixC _ c _)         = (simpleName c, 2)

        name (DataD _ n _ _ _ _)      = n
        name (NewtypeD _ n _ _ _ _)   = n
        name d                      = error $ show d

simpleName :: Name -> Name
simpleName nm =
   let s = nameBase nm
   in case dropWhile (/=':') s of
        []          -> mkName s
        [_]        -> mkName s
        _:t         -> mkName t



unopStart :: Int
unopStart = 0x1000

binopStart :: Int
binopStart = 0x2000

-- TODO correct arument oder?
pack2 a b = a + b * (2^16)

autoser :: Name -> Q [Dec]
autoser tName = do
    (TyConI d) <- reify tName
    (type_name,_,_,constructors) <- typeInfo (return d)
    return <$> instanceD (cxt []) (appT (conT ''EncTup) (conT type_name)) [
            funD (mkName "encTup") (zipWith f [0..] constructors)
        ]
    where
        f :: Int -> (Name, [(Maybe Name, Type)]) -> Q Clause
        f i (name, []) = clause [conP name []] (normalB [|($(lift i), 0)|]) []
        f i (name, [(n1, t1)]) | nameBase name == "IBop" = clause [conP name [varP x]]
            (normalB [|($(lift binopStart) + fromEnum xx, 0)|]) []
        f i (name, [(n1, t1)]) | nameBase name == "IUop" = clause [conP name [varP x]]
            (normalB [|($(lift  unopStart) + fromEnum xx, 0)|]) []
        f i (name, [(n1, t1)]) = clause [conP name [varP x]]
            (normalB [|($(lift i), xx)|]) []
        f i (name, [(n1, t1), (n2, t2)]) = clause [conP name [varP x, varP y]]
            (normalB [|($(lift i), pack2 xx yy)|]) []
        f i (name, _) = fail $ "constructor type not supported for: " ++ nameBase name
        x = mkName "xx"
        y = mkName "yy"
