{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances #-}
module Data.Profunctor.Representable 
  ( RepresentableProfunctor(..) 
  ) where

import Data.Profunctor
import Control.Comonad
import Data.Functor.Identity

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
