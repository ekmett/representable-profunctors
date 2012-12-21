{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances #-}
module Data.Profunctor.Representable
  ( RepresentableProfunctor(..)
  ) where

import Data.Functor
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Proxy
import Data.Profunctor
import Data.Profunctor.Composition
import Data.Tagged
import Control.Comonad

class (Profunctor k, Functor (Rep k)) => RepresentableProfunctor k where
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

instance RepresentableProfunctor Tagged where
  type Rep Tagged = Proxy
  tabulatePro f = Tagged (f Proxy)
  indexPro (Tagged a) _ = a

instance Functor f => RepresentableProfunctor (DownStar f) where
  type Rep (DownStar f) = f
  tabulatePro = DownStar
  indexPro = runDownStar

instance (RepresentableProfunctor f, RepresentableProfunctor g) => RepresentableProfunctor (Procompose f g) where
  type Rep (Procompose f g) = Compose (Rep g) (Rep f)
  tabulatePro f = Procompose (tabulatePro id) (tabulatePro (f . Compose))
  indexPro (Procompose f g) (Compose d) = indexPro g $ indexPro f <$> d
