
{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances #-}
module Data.Profunctor.Corepresentable 
  ( CorepresentableProfunctor(..) 
  ) where

import Data.Profunctor
import Control.Arrow
import Data.Functor.Identity

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
