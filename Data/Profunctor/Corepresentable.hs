
{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances #-}
module Data.Profunctor.Corepresentable 
  ( CorepresentableProfunctor(..) 
  ) where

import Data.Functor
import Data.Functor.Identity
import Data.Profunctor
import Control.Arrow
import qualified Data.Profunctor.Composition as Profunctor
import qualified Data.Functor.Compose as Functor

class Functor (Corep k) => CorepresentableProfunctor k where
  type Corep k :: * -> *
  cotabulatePro :: (d -> Corep k c) -> k d c
  coindexPro :: k d c -> d -> Corep k c 
  
instance CorepresentableProfunctor (->) where
  type Corep (->) = Identity
  cotabulatePro f = runIdentity . f
  coindexPro f = Identity . f 

instance Functor m => CorepresentableProfunctor (Kleisli m) where
  type Corep (Kleisli m) = m
  cotabulatePro = Kleisli
  coindexPro = runKleisli 

instance Functor f => CorepresentableProfunctor (UpStar f) where
  type Corep (UpStar f) = f
  cotabulatePro = UpStar
  coindexPro = runUpStar

instance (CorepresentableProfunctor c, CorepresentableProfunctor d) => CorepresentableProfunctor (Profunctor.Compose c d) where
  type Corep (Profunctor.Compose c d) = Functor.Compose (Corep c) (Corep d)
  cotabulatePro f = Profunctor.Compose (cotabulatePro (Functor.getCompose . f)) (cotabulatePro id)
  coindexPro (Profunctor.Compose f g) d = Functor.Compose $ coindexPro g <$> coindexPro f d
