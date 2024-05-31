{-# LANGUAGE ExplicitNamespaces #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Base.TH
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module contains everything you need to derive your own singletons via
-- Template Haskell, exporting both Template Haskell functionality and
-- definitions that are likely to be mentioned in promoted or singled code.
-- For a version of this module that only contains the Template Haskell
-- functionality and nothing else, see "Data.Singletons.TH" from the
-- @singletons-th@ library.
--
----------------------------------------------------------------------------

module Data.Singletons.Base.TH (
  -- * Primary Template Haskell generation functions
  singletons, singletonsOnly, genSingletons,
  promote, promoteOnly, genDefunSymbols, genPromotions,

  -- ** Functions to generate equality instances
  promoteEqInstances, promoteEqInstance,
  singEqInstances, singEqInstance,
  singDecideInstances, singDecideInstance,

  -- ** Functions to generate 'Ord' instances
  promoteOrdInstances, promoteOrdInstance,
  singOrdInstances, singOrdInstance,

  -- ** Functions to generate 'Bounded' instances
  promoteBoundedInstances, promoteBoundedInstance,
  singBoundedInstances, singBoundedInstance,

  -- ** Functions to generate 'Enum' instances
  promoteEnumInstances, promoteEnumInstance,
  singEnumInstances, singEnumInstance,

  -- ** Functions to generate 'Show' instances
  promoteShowInstances, promoteShowInstance,
  singShowInstances, singShowInstance,
  showSingInstances, showSingInstance,

  -- ** Utility functions
  singITyConInstances, singITyConInstance,
  cases, sCases,

  -- * Basic singleton definitions
  SList(..), SBool(..), STuple0(..), STuple2(..), STuple3(..), STuple4(..),
  STuple5(..), STuple6(..), STuple7(..), SOrdering(..),
  module Data.Singletons,

  -- * Auxiliary definitions
  -- | These definitions might be mentioned in code generated by Template Haskell,
  -- so they must be in scope.

  PEq(..), If, sIf, type (&&), (%&&), SEq(..),
  POrd(..), SOrd(..),
  SDecide(..), (:~:)(..), Void, Refuted, Decision(..),
  PBounded(..), SBounded(..),
  PEnum(FromEnum, ToEnum), SEnum(sFromEnum, sToEnum),
  PShow(..), SShow(..), PIsString(..), SIsString(..),
  ShowString, sShowString, ShowParen, sShowParen, ShowSpace, sShowSpace,
  ShowChar, sShowChar, ShowCommaSpace, sShowCommaSpace,
  FromInteger, sFromInteger, Negate, sNegate,
  PFunctor(..), SFunctor(..),
  PFoldable(..), SFoldable(..),
  PSemigroup(..), SSemigroup(..),
  PMonoid(..), SMonoid(..),
  PTraversable(..), STraversable(..), PApplicative(..), SApplicative(..),
  type (.), (%.),

  Error, sError, ErrorSym0, ErrorSym1,
  Undefined, sUndefined, UndefinedSym0,
  TrueSym0, FalseSym0,
  type (==@#@$), type (==@#@$$), type (==@#@$$$),
  type (>@#@$),  type (>@#@$$),  type (>@#@$$$),
  IfSym0, IfSym1, IfSym2, IfSym3,
  type (&&@#@$), type (&&@#@$$), type (&&@#@$$$),
  LTSym0, EQSym0, GTSym0,
  Tuple0Sym0,
  Tuple2Sym0, Tuple2Sym1, Tuple2Sym2,
  Tuple3Sym0, Tuple3Sym1, Tuple3Sym2, Tuple3Sym3,
  Tuple4Sym0, Tuple4Sym1, Tuple4Sym2, Tuple4Sym3, Tuple4Sym4,
  Tuple5Sym0, Tuple5Sym1, Tuple5Sym2, Tuple5Sym3, Tuple5Sym4, Tuple5Sym5,
  Tuple6Sym0, Tuple6Sym1, Tuple6Sym2, Tuple6Sym3, Tuple6Sym4, Tuple6Sym5, Tuple6Sym6,
  Tuple7Sym0, Tuple7Sym1, Tuple7Sym2, Tuple7Sym3, Tuple7Sym4, Tuple7Sym5, Tuple7Sym6, Tuple7Sym7,
  CompareSym0, CompareSym1, CompareSym2,
  FoldlSym0, FoldlSym1, FoldlSym2, FoldlSym3,
  MinBoundSym0, MaxBoundSym0,
  ShowsPrecSym0, ShowsPrecSym1, ShowsPrecSym2, ShowsPrecSym3,
  ShowStringSym0, ShowStringSym1, ShowStringSym2,
  ShowParenSym0, ShowParenSym1, ShowParenSym2,
  ShowSpaceSym0, ShowSpaceSym1,
  ShowCharSym0, ShowCharSym1, ShowCharSym2,
  ShowCommaSpaceSym0, ShowCommaSpaceSym1,
  FromIntegerSym0, FromIntegerSym1,
  NegateSym0, NegateSym1,
  FromStringSym0, FromStringSym1,
  FmapSym0, FmapSym1, FmapSym2,
  type (<$@#@$),  type (<$@#@$$),  type (<$@#@$$$),
  FoldMapSym0, FoldMapSym1, FoldMapSym2,
  type (<>@#@$), type (<>@#@$$), type (<>@#@$$$),
  MemptySym0,
  MappendSym0, MappendSym1, MappendSym2,
  FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3,
  TraverseSym0, TraverseSym1, TraverseSym2,
  PureSym0, PureSym1,
  type (<*>@#@$), type (<*>@#@$$), type (<*>@#@$$$),
  LiftA2Sym0, LiftA2Sym1, LiftA2Sym2, LiftA2Sym3,
  type (.@#@$), type (.@#@$$), type (.@#@$$$), type (.@#@$$$$),
  NilSym0, (:@#@$), (:@#@$$), (:@#@$$$),

  SuppressUnusedWarnings(..)

 ) where

import Control.Applicative.Singletons
import Data.Bool.Singletons
import Data.Eq.Singletons
import Data.Foldable.Singletons
import Data.Functor.Singletons hiding (Void)
import Data.Monoid.Singletons
import Data.Ord.Singletons
import Data.Semigroup.Singletons
       ( PSemigroup(..), SSemigroup(..)
       , type (<>@#@$), type (<>@#@$$), type (<>@#@$$$) )
import Data.Singletons
import Data.Singletons.Base.Enum
import Data.Singletons.Base.Instances
  hiding (Foldl, FoldlSym0, FoldlSym1, FoldlSym2, FoldlSym3, sFoldl)
import Data.Singletons.Decide
import Data.Singletons.TH
import Data.String.Singletons
import Data.Traversable.Singletons
import GHC.Base.Singletons
  hiding (Foldr, FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3, sFoldr)
import GHC.Num.Singletons
import GHC.TypeLits.Singletons
import Text.Show.Singletons
