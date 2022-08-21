{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

#if defined(__GLASGOW_HASKELL__)
# define LANGUAGE_DeriveDataTypeable
{-# LANGUAGE DeriveDataTypeable #-}
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
# define LANGUAGE_DeriveGeneric
{-# LANGUAGE DeriveGeneric #-}
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
#endif

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Language.Haskell.TH.Instances
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
--   * 'Typeable' for 'Lift', 'NameIs', 'Ppr', 'PprM', 'Q', 'Quasi',
--   'QuasiQuoter', and 'TExp'
--
-- More recent versions of template-haskell, particularly 2.10 (GHC
-- 7.10), provide these instances.  However, in order to support older
-- versions you should import this module.
--
-- Note that the 'Ord' instances are not guaranteed to produce
-- consistent results across template-haskell / GHC versions, as they
-- have different data types, with different constructor orders.
--
-- This module also implicitly re-exports the instances defined in
-- "Instances.TH.Lift". This is mostly to ensure that there aren't
-- collisions of orphans between @th-orphans@ and @th-lift-instances@.
module Language.Haskell.TH.Instances () where

import Language.Haskell.TH hiding (newName)
import Language.Haskell.TH.Instances.Internal
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.ReifyMany
import Language.Haskell.TH.Syntax hiding (newName)
import Language.Haskell.TH.Syntax.Compat (Quote(..))

import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.RWS (RWST(RWST), runRWST)
import Control.Monad.State (StateT(StateT), runStateT)
import qualified Control.Monad.Trans as Trans (MonadTrans(lift))
import Control.Monad.Writer (WriterT(WriterT), runWriterT)
#if !MIN_VERSION_base(4,8,0)
import Instances.TH.Lift ()
#endif

#if !(MIN_VERSION_template_haskell(2,8,0))
import Unsafe.Coerce (unsafeCoerce)
#endif

#if MIN_VERSION_template_haskell(2,11,0) && !(MIN_VERSION_template_haskell(2,15,0))
import Language.Haskell.TH.LanguageExtensions (Extension(..))
#endif

-- Thanks to Richard Eisenberg, GHC 7.10 adds many of the instances
-- from this module.
#if !MIN_VERSION_template_haskell(2,10,0)
import Language.Haskell.TH.Ppr
# if MIN_VERSION_template_haskell(2,3,0)
import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Quote
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
import Data.Monoid (Monoid (..))
# endif

# if MIN_VERSION_template_haskell(2,3,0) && defined(LANGUAGE_DeriveDataTypeable)
import Data.Data hiding (Fixity(..))
# endif

# if __GLASGOW_HASKELL__ > 702
import GHC.Generics (Generic)
# endif

# if __GLASGOW_HASKELL__ <= 702 || !(MIN_VERSION_template_haskell(2,10,0))
import qualified Generics.Deriving.TH as Generic (deriveAll)
# endif
#endif

#if !(MIN_VERSION_template_haskell(2,11,0))
import qualified Control.Monad.Fail as Fail
#endif

#if MIN_VERSION_template_haskell(2,16,0)
import GHC.Ptr (Ptr(Ptr))
import GHC.ForeignPtr (newForeignPtr_)
import Language.Haskell.TH.Syntax.Compat (liftTypedFromUntypedSplice)
import System.IO.Unsafe (unsafePerformIO)
#endif

#if !MIN_VERSION_template_haskell(2,17,0)
import Control.Applicative (liftA2)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Monad.Fix (MonadFix (..))
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.Semigroup as Semi

# if MIN_VERSION_base(4,11,0)
import Control.Exception (throwIO, catch)
import GHC.IO.Exception (BlockedIndefinitelyOnMVar (..), FixIOException (..))
# endif
#endif

#if !MIN_VERSION_template_haskell(2,11,0)
deriving instance Show NameFlavour
deriving instance Show NameSpace

instance Fail.MonadFail Q where
  fail s = report True s >> q (fail "Q monad failure")
    where
      q :: (forall m. Quasi m => m a) -> Q a
# if MIN_VERSION_template_haskell(2,8,0)
      q = Q
# else
      -- Early versions of template-haskell did not expose Q's newtype
      -- constructor. Desperate times call for desperate measures.
      q = unsafeCoerce
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

# if defined(LANGUAGE_DeriveDataTypeable)
deriving instance Typeable  NameIs
deriving instance Typeable1 PprM
deriving instance Typeable1 Q
# endif

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
# else
$(Generic.deriveAll ''Body)
$(Generic.deriveAll ''Callconv)
$(Generic.deriveAll ''Clause)
$(Generic.deriveAll ''Con)
$(Generic.deriveAll ''Dec)
$(Generic.deriveAll ''Exp)
$(Generic.deriveAll ''Fixity)
$(Generic.deriveAll ''FixityDirection)
$(Generic.deriveAll ''Foreign)
$(Generic.deriveAll ''FunDep)
$(Generic.deriveAll ''Guard)
$(Generic.deriveAll ''Info)
$(Generic.deriveAll ''Lit)
$(Generic.deriveAll ''Match)
$(Generic.deriveAll ''Name)
$(Generic.deriveAll ''NameSpace)
$(Generic.deriveAll ''Pat)
$(Generic.deriveAll ''Range)
$(Generic.deriveAll ''Safety)
$(Generic.deriveAll ''Stmt)
$(Generic.deriveAll ''Strict)
$(Generic.deriveAll ''Type)
# endif

-- Unconditionally use Template Haskell to derive this Generic instance, since
-- NameFlavour has fields with unboxed types (for which deriving Generic
-- support wasn't added to GHC until 8.0).
$(Generic.deriveAll ''NameFlavour)

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

deriving instance Typeable QuasiQuoter
#  endif

#  if defined(LANGUAGE_DeriveGeneric)
deriving instance Generic Loc
#  else
$(Generic.deriveAll ''Loc)
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
#  else
$(Generic.deriveAll ''FamFlavour)
$(Generic.deriveAll ''ModName)
$(Generic.deriveAll ''OccName)
$(Generic.deriveAll ''PkgName)
$(Generic.deriveAll ''Pragma)
$(Generic.deriveAll ''Pred)
$(Generic.deriveAll ''TyVarBndr)
#  endif

#  if !(MIN_VERSION_template_haskell(2,8,0))
deriving instance Ord InlineSpec
deriving instance Ord Kind

#   if defined(LANGUAGE_DeriveGeneric)
deriving instance Generic InlineSpec
deriving instance Generic Kind
#   else
$(Generic.deriveAll ''InlineSpec)
$(Generic.deriveAll ''Kind)
#   endif
#  endif
# endif

# if MIN_VERSION_template_haskell(2,5,0) && !(MIN_VERSION_template_haskell(2,7,0))
deriving instance Eq ClassInstance
deriving instance Ord ClassInstance

#  if defined(LANGUAGE_DeriveGeneric)
deriving instance Generic ClassInstance
#  else
$(Generic.deriveAll ''ClassInstance)
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
#  else
$(Generic.deriveAll ''Inline)
$(Generic.deriveAll ''Phases)
$(Generic.deriveAll ''RuleBndr)
$(Generic.deriveAll ''RuleMatch)
$(Generic.deriveAll ''TyLit)
#  endif
# endif

# if MIN_VERSION_template_haskell(2,9,0)
deriving instance Eq ModuleInfo

deriving instance Ord AnnLookup
deriving instance Ord AnnTarget
deriving instance Ord ModuleInfo
deriving instance Ord Role
deriving instance Ord TySynEqn

#  if defined(LANGUAGE_DeriveDataTypeable)
deriving instance Typeable TExp
#  endif

#  if defined(LANGUAGE_DeriveGeneric)
deriving instance Generic AnnLookup
deriving instance Generic AnnTarget
deriving instance Generic Module
deriving instance Generic ModuleInfo
deriving instance Generic Role
deriving instance Generic TySynEqn
#  else
$(Generic.deriveAll ''AnnLookup)
$(Generic.deriveAll ''AnnTarget)
$(Generic.deriveAll ''Module)
$(Generic.deriveAll ''ModuleInfo)
$(Generic.deriveAll ''Role)
$(Generic.deriveAll ''TySynEqn)
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

#if MIN_VERSION_template_haskell(2,11,0) && !(MIN_VERSION_template_haskell(2,15,0))
deriving instance Bounded Extension
#endif

instance Quote m => Quote (ReaderT r m) where
    newName = Trans.lift . newName
$(deriveQuasiTrans
    [t| forall r m. Quasi m => Proxy2 (ReaderT r m) |]
    [e| \m1 m2 -> ReaderT $ \ r -> runReaderT m1 r `qRecover` runReaderT m2 r |])

instance (Quote m, Monoid w) => Quote (WriterT w m) where
    newName = Trans.lift . newName
$(deriveQuasiTrans
    [t| forall w m. (Quasi m, Monoid w) => Proxy2 (WriterT w m) |]
    [e| \m1 m2 -> WriterT $ runWriterT m1 `qRecover` runWriterT m2 |])

instance Quote m => Quote (StateT s m) where
    newName = Trans.lift . newName
$(deriveQuasiTrans
    [t| forall s m. Quasi m => Proxy2 (StateT s m) |]
    [e| \m1 m2 -> StateT $ \ s -> runStateT m1 s `qRecover` runStateT m2 s |])

instance (Quote m, Monoid w) => Quote (RWST r w s m) where
    newName = Trans.lift . newName
$(deriveQuasiTrans
    [t| forall r w s m. (Quasi m, Monoid w) => Proxy2 (RWST r w s m) |]
    [e| \m1 m2 -> RWST $ \ r s -> runRWST m1 r s `qRecover` runRWST m2 r s |])

#if MIN_VERSION_base(4,7,0) && defined(LANGUAGE_DeriveDataTypeable) && __GLASGOW_HASKELL__ < 710
deriving instance Typeable Lift
deriving instance Typeable Ppr
deriving instance Typeable Quasi
#endif

#if MIN_VERSION_template_haskell(2,16,0)
instance Lift Bytes where
  lift bytes =
    [| Bytes
      { bytesPtr = unsafePerformIO $ newForeignPtr_ (Ptr $(litE (BytesPrimL bytes)))
      , bytesOffset = 0
      , bytesSize = size
      }
    |]
    where
      size = bytesSize bytes
  liftTyped = liftTypedFromUntypedSplice
#endif

#if !MIN_VERSION_template_haskell(2,17,0)
instance Semi.Semigroup a => Semi.Semigroup (Q a) where
  (<>) = liftA2 (Semi.<>)

instance Monoid a => Monoid (Q a) where
  mempty = return mempty
#if !MIN_VERSION_base(4,11,0)
  mappend = liftA2 mappend
#endif

-- | If the function passed to 'mfix' inspects its argument,
-- the resulting action will throw a 'FixIOException'
-- (@base >=4.11@) or a 'BlockedIndefinitelyOnMVar'
-- with older @base@.
--
instance MonadFix Q where
  mfix k = do
    m <- runIO newEmptyMVar
    ans <- runIO (unsafeInterleaveIO (readMVar m
#if MIN_VERSION_base(4,11,0)
        `catch` \BlockedIndefinitelyOnMVar -> throwIO FixIOException
#endif
        ))
    result <- k ans
    runIO (putMVar m result)
    return result
#endif

$(reifyManyWithoutInstances ''Lift [''Info, ''Loc] (const True) >>=
  deriveLiftMany)
