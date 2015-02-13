{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.Instances.Internal where

import Data.Generics (toConstr, constrIndex)
import Language.Haskell.TH

-- Overall structure taken from
-- https://hackage.haskell.org/package/derive-2.5.18/docs/src/Data-Derive-Ord.html

deriveOrd :: Name -> Q Dec
deriveOrd n = do
    TyConI (DataD _ _ _ cons _) <- reify n
    na <- newName "a"
    nb <- newName "b"
    ncheck <- newName "check"
    nthen <- newName "_then"
    ntag <- newName "_tag"
    nx <- newName "x"
    ny <- newName "y"
    checkClauses <- flip mapM cons $ \con -> do
        (pat1, names1) <- conToPat con
        (pat2, names2) <- conToPat con
        return $ Clause [pat1, pat2]
                        (NormalB $
                         foldr (\(n1, n2) ->
                                  AppE (AppE (VarE nthen)
                                             (AppE (AppE (VarE 'compare)
                                                         (VarE n1))
                                                   (VarE n2))))
                               (ConE 'EQ)
                               (zip names1 names2))
                        []
    let lastCheckClause =
            if length cons <= 1
               then []
               else [Clause [VarP nx, VarP ny]
                            (NormalB (AppE (AppE (VarE 'compare)
                                                 (AppE (VarE 'constrIndex) (AppE (VarE 'toConstr) (VarE nx))))
                                           (AppE (VarE 'constrIndex) (AppE (VarE 'toConstr) (VarE ny)))))
                            []]
    tagClauses <- flip mapM (zip [0..] cons) $ \(ix, con) -> do
        (pat, _) <- conToPat con
        return $ Clause [pat]
                        (NormalB (SigE (LitE (IntegerL ix))
                                       (ConT ''Int)))
                        []
    return $ InstanceD []
                       (AppT (ConT ''Ord) (ConT n))
                       [FunD 'compare [Clause
                           [VarP na, VarP nb]
                           (NormalB (AppE (AppE (VarE ncheck)
                                                (VarE na))
                                          (VarE nb)))
                           [ FunD ncheck (checkClauses ++ lastCheckClause)
                           , FunD nthen
                                  [ Clause [ConP 'EQ [], VarP nx] (NormalB (VarE nx)) []
                                  , Clause [VarP nx, WildP] (NormalB (VarE nx)) []
                                  ]
                           , FunD ntag tagClauses
                           ]]]

conToPat :: Con -> Q (Pat, [Name])
conToPat (RecC n fs) = conToPat (NormalC n (map (\(_, s, t) -> (s, t)) fs))
conToPat (ForallC _ _ con) = conToPat con
conToPat (NormalC n tys) = do
    names <- mapM (\_ -> newName "_x") tys
    return (ConP n (map VarP names), names)
conToPat (InfixC _ n _) = do
    ln <- newName "_l"
    rn <- newName "_r"
    return (InfixP (VarP ln) n (VarP rn), [ln, rn])
