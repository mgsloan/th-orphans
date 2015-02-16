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
import Language.Haskell.TH.Instances.Internal
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

#if MIN_VERSION_template_haskell(2,5,0) && !(MIN_VERSION_template_haskell(2,7,0))
deriving instance Eq ClassInstance
#endif

$(reifyManyWithoutInstances ''Ord [''Info] (`notElem` [''Ratio]) >>=
  mapM deriveOrd)

$(reifyManyWithoutInstances ''Lift [''Info, ''Loc] (const True) >>=
  deriveLiftMany)
