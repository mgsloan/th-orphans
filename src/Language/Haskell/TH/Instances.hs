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
module Language.Haskell.TH.Instances () where

import GHC.Real (Ratio)
import Language.Haskell.TH
import Language.Haskell.TH.Instances.Internal
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.ReifyMany
import Language.Haskell.TH.Syntax

#if !MIN_VERSION_template_haskell(2,10,0)
import GHC.Word (Word8)
import Language.Haskell.TH.Ppr
#endif

-- Orphan Show instances

#if !MIN_VERSION_template_haskell(2,10,0)
deriving instance Show Loc
#endif

-- Orphan Eq instances

#if !MIN_VERSION_template_haskell(2,10,0)
deriving instance Eq Loc
deriving instance Eq Info
#endif

#if MIN_VERSION_template_haskell(2,5,0) && !(MIN_VERSION_template_haskell(2,7,0))
deriving instance Eq ClassInstance
#endif

-- Orphan Ord instances

#if !MIN_VERSION_template_haskell(2,10,0)
instance Ord FixityDirection where
  (<=) InfixL _      = True
  (<=) _      InfixR = True
  (<=) InfixN InfixN = True
  (<=) _      _      = False
#endif

$(reifyManyWithoutInstances ''Ord [''Info] (`notElem` [''Ratio]) >>=
  mapM deriveOrd)

-- Orphan Ppr instances

#if !MIN_VERSION_template_haskell(2,10,0)
-- TODO: make this better
instance Ppr Loc where
  ppr = showtextl . show

instance Ppr Lit where
  ppr l = ppr (LitE l)
#endif

-- Orphan Lift instances (for when your TH generates TH!)

#if !MIN_VERSION_template_haskell(2,10,0)
-- This follows the pattern of the Lift instances for Int / Integer.
instance Lift Word8 where
  lift w = [e| fromIntegral $(lift (fromIntegral w :: Int)) |]
#endif

$(reifyManyWithoutInstances ''Lift [''Info] (const True) >>=
  deriveLiftMany)
