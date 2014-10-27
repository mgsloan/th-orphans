{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Data.Derive.Ord (makeOrd)
import Data.DeriveTH (derives)
import GHC.Word (Word8)
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.ReifyMany
import Language.Haskell.TH.Syntax

-- Orphan Show instances
deriving instance Show Loc

-- Orphan Eq instances
deriving instance Eq Loc
deriving instance Eq Info
#if MIN_VERSION_template_haskell(2,5,0) && !(MIN_VERSION_template_haskell(2,7,0))
deriving instance Eq ClassInstance
#endif

-- Orphan Ord instances
instance Ord FixityDirection where
  (<=) InfixL _      = True
  (<=) _      InfixR = True
  (<=) InfixN InfixN = True
  (<=) _      _      = False

$(reifyManyWithoutInstances ''Ord [''Info] (const True) >>=
  derives [makeOrd])

-- Orphan Ppr instances
-- TODO: make this better
instance Ppr Loc where
  ppr = showtextl . show

instance Ppr Lit where
  ppr l = ppr (LitE l)

-- Orphan Lift instances (for when your TH generates TH!)

-- This follows the pattern of the Lift instances for Int / Integer.
instance Lift Word8 where
  lift w = [e| fromIntegral $(lift (fromIntegral w :: Int)) |]

$(reifyManyWithoutInstances ''Lift [''Info] (const True) >>=
  deriveLiftMany)
