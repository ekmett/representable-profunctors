{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances #-}
module Data.Profunctor.Corepresentable
  ( CorepresentableProfunctor(..)
  ) where

import Data.Functor
import Data.Functor.Identity
import Data.Profunctor
import Control.Arrow
import Data.Profunctor.Composition
import Data.Functor.Compose

class (Profunctor k, Functor (Corep k)) => CorepresentableProfunctor k where
  type Corep k :: * -> *
  cotabulatePro :: (d -> Corep k c) -> k d c
  coindexPro :: k d c -> d -> Corep k c

instance CorepresentableProfunctor (->) where
  type Corep (->) = Identity
  cotabulatePro f = runIdentity . f
  coindexPro f = Identity . f

instance (Monad m, Functor m) => CorepresentableProfunctor (Kleisli m) where
  type Corep (Kleisli m) = m
  cotabulatePro = Kleisli
  coindexPro = runKleisli

instance Functor f => CorepresentableProfunctor (UpStar f) where
  type Corep (UpStar f) = f
  cotabulatePro = UpStar
  coindexPro = runUpStar

instance (CorepresentableProfunctor c, CorepresentableProfunctor d) => CorepresentableProfunctor (Procompose c d) where
  type Corep (Procompose c d) = Compose (Corep c) (Corep d)
  cotabulatePro f = Procompose (cotabulatePro (getCompose . f)) (cotabulatePro id)
  coindexPro (Procompose f g) d = Compose $ coindexPro g <$> coindexPro f d
