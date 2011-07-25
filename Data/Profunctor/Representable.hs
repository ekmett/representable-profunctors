{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances #-}
module Data.Profunctor.Representable 
  ( RepresentableProfunctor(..) 
  ) where

import Data.Functor
import Data.Functor.Identity
import qualified Data.Functor.Compose as Functor
import Data.Profunctor
import qualified Data.Profunctor.Composition as Profunctor
import Control.Comonad

class Functor (Rep k) => RepresentableProfunctor k where
  type Rep k :: * -> *
  tabulatePro :: (Rep k d -> c) -> k d c
  indexPro :: k d c -> Rep k d -> c 
  
instance RepresentableProfunctor (->) where
  type Rep (->) = Identity
  tabulatePro f = f . Identity
  indexPro f (Identity d) = f d

instance Functor w => RepresentableProfunctor (Cokleisli w) where
  type Rep (Cokleisli w) = w 
  tabulatePro = Cokleisli
  indexPro = runCokleisli 

instance Functor f => RepresentableProfunctor (DownStar f) where
  type Rep (DownStar f) = f
  tabulatePro = DownStar
  indexPro = runDownStar

instance (RepresentableProfunctor f, RepresentableProfunctor g) => RepresentableProfunctor (Profunctor.Compose f g) where
  type Rep (Profunctor.Compose f g) = Functor.Compose (Rep g) (Rep f)
  tabulatePro f = Profunctor.Compose (tabulatePro id) (tabulatePro (f . Functor.Compose))
  indexPro (Profunctor.Compose f g) (Functor.Compose d) = indexPro g $ indexPro f <$> d
