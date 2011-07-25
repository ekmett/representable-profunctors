{-# LANGUAGE TypeFamilies #-}
module Data.Profunctor.Corepresentable 
  ( RepresentableProfunctor(..) 
  ) where

import Data.Profunctor
import Control.Arrow
import Control.Monad.Identity

class Functor (Corep k) => CorepresentableProfunctor k where
  type Corep k :: * -> *
  cotabulatePro :: (d -> Corep k c) -> k d c
  coindexPro :: k d c -> d -> Corep k c 
  
instance CorepresentableProfunctor (->) where
  type Corep (->) = Identity
  cotabulatePro f = runIdentity . f
  indexPro f = Identity . f 

instance Functor w => CorepresentableProfunctor (Kleisli w) where
  type Rep (Kleisli m) = m
  cotabulatePro = Kleisli
  coindexPro = runKleisli 

instance Functor w => CorepresentableProfunctor (UpStar f) where
  type Rep (DownStar f) = f
  cotabulatePro = UpStar
  coindexPro = runUpStar
