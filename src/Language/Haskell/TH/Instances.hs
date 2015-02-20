{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Language.Haskell.TH.Instances.Lift
-- Copyright   :  (c) Matt Morrow 2008
-- License     :  BSD3
-- Maintainer  :  Michael Sloan <mgsloan at gmail>
-- Stability   :  experimental
-- Portability :  portable (template-haskell)
--
-- Provides 'Ord' and 'Lift' instances for the datatypes in
-- "Language.Haskell.TH".  Also provides 'Show' and 'Eq' for 'Loc', as
-- well as 'Ppr' for 'Loc' and 'Lit'.
--
-- Note that the 'Ord' instances are not guaranteed to produce
-- consistent results across template-haskell / GHC versions, as they
-- have different data types, with different constructor orders.
module Language.Haskell.TH.Instances () where

import GHC.Real (Ratio)
import Language.Haskell.TH
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.ReifyMany
import Language.Haskell.TH.Syntax

-- Thanks to Richard Eisenberg, GHC 7.10 adds many of the instances
-- from this module.
#if !MIN_VERSION_template_haskell(2,10,0)
import GHC.Word (Word8)
import Language.Haskell.TH.Ppr

deriving instance Show Loc

deriving instance Eq Loc
deriving instance Eq Info

instance Ord FixityDirection where
  (<=) InfixL _      = True
  (<=) _      InfixR = True
  (<=) InfixN InfixN = True
  (<=) _      _      = False

-- TODO: make this better
instance Ppr Loc where
  ppr = showtextl . show

instance Ppr Lit where
  ppr l = ppr (LitE l)

-- This follows the pattern of the Lift instances for Int / Integer.
instance Lift Word8 where
  lift w = [e| fromIntegral $(lift (fromIntegral w :: Int)) |]
#endif

$(reifyManyWithoutInstances ''Lift [''Info, ''Loc] (const True) >>=
  deriveLiftMany)

-- Ideally, it'd be possible to use reifyManyWithoutInstances for
-- these Ord instances, but TH can't output deriving instances (and
-- even if this is added for later versions, we need to support many
-- ghc / th versions).  We can't generate Ord instances from TH due to
-- some undiagnosed funkiness:
-- https://github.com/mgsloan/th-orphans/issues/14

-- GHC 7.10 comes with Ord instances for TH datatypes.
#if !MIN_VERSION_template_haskell(2,10,0)
deriving instance Ord Info
deriving instance Ord Fixity
deriving instance Ord Exp
deriving instance Ord Dec
deriving instance Ord Stmt
deriving instance Ord Type
deriving instance Ord Foreign
deriving instance Ord FunDep
deriving instance Ord Con
deriving instance Ord Body
deriving instance Ord Clause
deriving instance Ord Strict
deriving instance Ord Safety
deriving instance Ord Callconv
deriving instance Ord Guard
deriving instance Ord Range
deriving instance Ord Match
deriving instance Ord Pat
deriving instance Ord Lit

#if MIN_VERSION_template_haskell(2,4,0)
deriving instance Ord FamFlavour
deriving instance Ord Pragma
deriving instance Ord Pred
deriving instance Ord TyVarBndr
#endif

#if MIN_VERSION_template_haskell(2,4,0) && !(MIN_VERSION_template_haskell(2,8,0))
deriving instance Ord InlineSpec
deriving instance Ord Kind
#endif

#if MIN_VERSION_template_haskell(2,5,0) && !(MIN_VERSION_template_haskell(2,7,0))
deriving instance Eq ClassInstance
deriving instance Ord ClassInstance
#endif

#if MIN_VERSION_template_haskell(2,8,0)
deriving instance Ord Inline
deriving instance Ord Phases
deriving instance Ord RuleBndr
deriving instance Ord RuleMatch
deriving instance Ord TyLit
#endif

#if MIN_VERSION_template_haskell(2,9,0)
deriving instance Ord AnnTarget
deriving instance Ord Role
deriving instance Ord TySynEqn
#endif
#endif
