{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

#if defined(__GLASGOW_HASKELL__)
# define LANGUAGE_DeriveDataTypeable
{-# LANGUAGE DeriveDataTypeable #-}
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
# define LANGUAGE_DeriveGeneric
{-# LANGUAGE DeriveGeneric #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Language.Haskell.TH.Instances.Lift
-- Copyright   :  (c) Matt Morrow 2008
-- License     :  BSD3
-- Maintainer  :  Michael Sloan <mgsloan at gmail>
-- Stability   :  experimental
-- Portability :  portable (template-haskell)
--
-- It provides the following instances:
--
--   * 'Ord', 'Lift', 'Generic', 'Show', 'Eq', 'Data', 'Typeable',
--   'Ppr', instances for the datatypes in "Language.Haskell.TH"
--
--   * 'Lift' instances for "Data.Word" / "Data.Int" types
--
--   * 'Applicative' for 'Q'
--
--   * 'Quasi' for 'ReaderT', 'WriterT', 'StateT', and 'RWST'.
--
-- More recent versions of template-haskell, particularly 2.10 (GHC
-- 7.10), provide these instances.  However, in order to support older
-- versions you should import this module.
--
-- Note that the 'Ord' instances are not guaranteed to produce
-- consistent results across template-haskell / GHC versions, as they
-- have different data types, with different constructor orders.
module Language.Haskell.TH.Instances () where

import Language.Haskell.TH
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.ReifyMany
import Language.Haskell.TH.Syntax

import Data.Monoid (Monoid)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.RWS (RWST(RWST), runRWST)
import Control.Monad.State (StateT(StateT), runStateT)
import Control.Monad.Writer (WriterT(WriterT), runWriterT)
import qualified Control.Monad.Trans as MTL (lift)

-- Thanks to Richard Eisenberg, GHC 7.10 adds many of the instances
-- from this module.
#if !MIN_VERSION_template_haskell(2,10,0)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)

import Language.Haskell.TH.Ppr
# if MIN_VERSION_template_haskell(2,3,0)
import Language.Haskell.TH.PprLib
# endif
# if MIN_VERSION_template_haskell(2,4,0) && !(MIN_VERSION_template_haskell(2,8,0))
import Language.Haskell.TH.Syntax.Internals
# endif

# if !(MIN_VERSION_template_haskell(2,9,0))
#  if !(MIN_VERSION_base(4,8,0))
import Control.Applicative (Applicative(..))
#  endif
import Control.Monad (ap, liftM)
# endif

# if !(MIN_VERSION_base(4,8,0))
import Data.Word (Word)
# endif

# if MIN_VERSION_template_haskell(2,3,0) && defined(LANGUAGE_DeriveDataTypeable)
import Data.Data (Data, Typeable)
# endif

# if defined(LANGUAGE_DeriveGeneric)
import GHC.Generics (Generic)
# endif
#endif

-- Ideally, it'd be possible to use reifyManyWithoutInstances for
-- these Ord instances, but TH can't output deriving instances (and
-- even if this is added for later versions, we need to support many
-- ghc / th versions).  We can't generate Ord instances from TH due to
-- some undiagnosed funkiness:
-- https://github.com/mgsloan/th-orphans/issues/14

-- GHC 7.10 comes with Ord instances for TH datatypes.
#if !MIN_VERSION_template_haskell(2,10,0)
instance Ppr Lit where
    ppr = pprLit noPrec

-- This follows the pattern of the Lift instances for Int / Integer.
instance Lift Int8 where
    lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int16 where
    lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int32 where
    lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int64 where
    lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word where
    lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word8 where
    lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word16 where
    lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word32 where
    lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word64 where
    lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Natural where
    lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Float where
    lift x = return (LitE (RationalL (toRational x)))

instance Lift Double where
    lift x = return (LitE (RationalL (toRational x)))

deriving instance Eq Info

deriving instance Ord Body
deriving instance Ord Callconv
deriving instance Ord Clause
deriving instance Ord Con
deriving instance Ord Dec
deriving instance Ord Exp
deriving instance Ord Fixity
deriving instance Ord FixityDirection
deriving instance Ord Foreign
deriving instance Ord FunDep
deriving instance Ord Guard
deriving instance Ord Info
deriving instance Ord Lit
deriving instance Ord Match
deriving instance Ord Pat
deriving instance Ord Range
deriving instance Ord Safety
deriving instance Ord Stmt
deriving instance Ord Strict
deriving instance Ord Type

# if defined(LANGUAGE_DeriveGeneric)
deriving instance Generic Body
deriving instance Generic Callconv
deriving instance Generic Clause
deriving instance Generic Con
deriving instance Generic Dec
deriving instance Generic Exp
deriving instance Generic Fixity
deriving instance Generic FixityDirection
deriving instance Generic Foreign
deriving instance Generic FunDep
deriving instance Generic Guard
deriving instance Generic Info
deriving instance Generic Lit
deriving instance Generic Match
deriving instance Generic Name
deriving instance Generic NameSpace
deriving instance Generic Pat
deriving instance Generic Range
deriving instance Generic Safety
deriving instance Generic Stmt
deriving instance Generic Strict
deriving instance Generic Type
# endif

# if MIN_VERSION_template_haskell(2,3,0)
instance Ppr Loc where
    ppr (Loc { loc_module = md
             , loc_package = pkg
             , loc_start = (start_ln, start_col)
             , loc_end = (end_ln, end_col) })
      = hcat [ text pkg, colon, text md, colon
             , parens $ int start_ln <> comma <> int start_col
             , text "-"
             , parens $ int end_ln <> comma <> int end_col ]

deriving instance Eq Loc
deriving instance Ord Loc
deriving instance Show Loc

#  if defined(LANGUAGE_DeriveDataTypeable)
deriving instance Data Loc
deriving instance Typeable Loc
#  endif

#  if defined(LANGUAGE_DeriveGeneric)
deriving instance Generic Loc
#  endif
# endif

# if MIN_VERSION_template_haskell(2,4,0)
deriving instance Ord FamFlavour
deriving instance Ord Pragma
deriving instance Ord Pred
deriving instance Ord TyVarBndr

#  if defined(LANGUAGE_DeriveGeneric)
deriving instance Generic FamFlavour
deriving instance Generic ModName
deriving instance Generic OccName
deriving instance Generic PkgName
deriving instance Generic Pragma
deriving instance Generic Pred
deriving instance Generic TyVarBndr
#  endif

#  if !(MIN_VERSION_template_haskell(2,8,0))
deriving instance Ord InlineSpec
deriving instance Ord Kind

#   if defined(LANGUAGE_DeriveGeneric)
deriving instance Generic InlineSpec
deriving instance Generic Kind
#   endif
#  endif
# endif

# if MIN_VERSION_template_haskell(2,5,0) && !(MIN_VERSION_template_haskell(2,7,0))
deriving instance Eq ClassInstance
deriving instance Ord ClassInstance

#  if defined(LANGUAGE_DeriveGeneric)
deriving instance Generic ClassInstance
#  endif
# endif

# if !(MIN_VERSION_template_haskell(2,7,0))
instance Applicative Q where
    pure  = return
    (<*>) = ap
# endif

# if MIN_VERSION_template_haskell(2,8,0)
deriving instance Ord Inline
deriving instance Ord Phases
deriving instance Ord RuleBndr
deriving instance Ord RuleMatch
deriving instance Ord TyLit

#  if defined(LANGUAGE_DeriveGeneric)
deriving instance Generic Inline
deriving instance Generic Phases
deriving instance Generic RuleBndr
deriving instance Generic RuleMatch
deriving instance Generic TyLit
#  endif
# endif

# if MIN_VERSION_template_haskell(2,9,0)
deriving instance Eq ModuleInfo

deriving instance Ord AnnLookup
deriving instance Ord AnnTarget
deriving instance Ord ModuleInfo
deriving instance Ord Role
deriving instance Ord TySynEqn

#  if defined(LANGUAGE_DeriveGeneric)
deriving instance Generic AnnLookup
deriving instance Generic AnnTarget
deriving instance Generic Module
deriving instance Generic ModuleInfo
deriving instance Generic Role
deriving instance Generic TySynEqn
#  endif
# else
deriving instance Show ModName
deriving instance Show OccName
deriving instance Show PkgName

instance Functor PprM where
    fmap = liftM

instance Applicative PprM where
    pure  = return
    (<*>) = ap
# endif
#endif

instance Quasi m => Quasi (ReaderT r m) where
  qNewName          = MTL.lift . qNewName
  qReport a b       = MTL.lift $ qReport a b
  qRecover m1 m2    = ReaderT $ \ r -> runReaderT m1 r `qRecover` runReaderT m2 r
  qReify            = MTL.lift . qReify
  qLocation         = MTL.lift qLocation
  qRunIO            = MTL.lift . qRunIO
#if MIN_VERSION_template_haskell(2,7,0)
  qReifyInstances a b = MTL.lift $ qReifyInstances a b
  qLookupName a b   = MTL.lift $ qLookupName a b
  qAddDependentFile = MTL.lift . qAddDependentFile
#if MIN_VERSION_template_haskell(2,9,0)
  qReifyRoles       = MTL.lift . qReifyRoles
  qReifyAnnotations = MTL.lift . qReifyAnnotations
  qReifyModule      = MTL.lift . qReifyModule
  qAddTopDecls      = MTL.lift . qAddTopDecls
  qAddModFinalizer  = MTL.lift . qAddModFinalizer
  qGetQ             = MTL.lift qGetQ
  qPutQ             = MTL.lift . qPutQ
#endif
#endif

instance (Quasi m, Monoid w) => Quasi (WriterT w m) where
  qNewName          = MTL.lift . qNewName
  qReport a b       = MTL.lift $ qReport a b
  qRecover m1 m2    = WriterT $ runWriterT m1 `qRecover` runWriterT m2
  qReify            = MTL.lift . qReify
  qLocation         = MTL.lift qLocation
  qRunIO            = MTL.lift . qRunIO
#if MIN_VERSION_template_haskell(2,7,0)
  qReifyInstances a b = MTL.lift $ qReifyInstances a b
  qLookupName a b   = MTL.lift $ qLookupName a b
  qAddDependentFile = MTL.lift . qAddDependentFile
#if MIN_VERSION_template_haskell(2,9,0)
  qReifyRoles       = MTL.lift . qReifyRoles
  qReifyAnnotations = MTL.lift . qReifyAnnotations
  qReifyModule      = MTL.lift . qReifyModule
  qAddTopDecls      = MTL.lift . qAddTopDecls
  qAddModFinalizer  = MTL.lift . qAddModFinalizer
  qGetQ             = MTL.lift qGetQ
  qPutQ             = MTL.lift . qPutQ
#endif
#endif

instance Quasi m => Quasi (StateT s m) where
  qNewName          = MTL.lift . qNewName
  qReport a b       = MTL.lift $ qReport a b
  qRecover m1 m2    = StateT $ \ s -> runStateT m1 s `qRecover` runStateT m2 s
  qReify            = MTL.lift . qReify
  qLocation         = MTL.lift qLocation
  qRunIO            = MTL.lift . qRunIO
#if MIN_VERSION_template_haskell(2,7,0)
  qReifyInstances a b = MTL.lift $ qReifyInstances a b
  qLookupName a b   = MTL.lift $ qLookupName a b
  qAddDependentFile = MTL.lift . qAddDependentFile
#if MIN_VERSION_template_haskell(2,9,0)
  qReifyRoles       = MTL.lift . qReifyRoles
  qReifyAnnotations = MTL.lift . qReifyAnnotations
  qReifyModule      = MTL.lift . qReifyModule
  qAddTopDecls      = MTL.lift . qAddTopDecls
  qAddModFinalizer  = MTL.lift . qAddModFinalizer
  qGetQ             = MTL.lift qGetQ
  qPutQ             = MTL.lift . qPutQ
#endif
#endif

instance (Quasi m, Monoid w) => Quasi (RWST r w s m) where
  qNewName          = MTL.lift . qNewName
  qReport a b       = MTL.lift $ qReport a b
  qRecover m1 m2    = RWST $ \ r s -> runRWST m1 r s `qRecover` runRWST m2 r s
  qReify            = MTL.lift . qReify
  qLocation         = MTL.lift qLocation
  qRunIO            = MTL.lift . qRunIO
#if MIN_VERSION_template_haskell(2,7,0)
  qReifyInstances a b = MTL.lift $ qReifyInstances a b
  qLookupName a b   = MTL.lift $ qLookupName a b
  qAddDependentFile = MTL.lift . qAddDependentFile
#if MIN_VERSION_template_haskell(2,9,0)
  qReifyRoles       = MTL.lift . qReifyRoles
  qReifyAnnotations = MTL.lift . qReifyAnnotations
  qReifyModule      = MTL.lift . qReifyModule
  qAddTopDecls      = MTL.lift . qAddTopDecls
  qAddModFinalizer  = MTL.lift . qAddModFinalizer
  qGetQ             = MTL.lift qGetQ
  qPutQ             = MTL.lift . qPutQ
#endif
#endif

$(reifyManyWithoutInstances ''Lift [''Info, ''Loc] (const True) >>=
  deriveLiftMany)
