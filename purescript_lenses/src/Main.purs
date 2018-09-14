module Main where

import Prelude

import Data.Profunctor (class Profunctor)
import Data.Profunctor.Strong (class Strong)
import Data.Profunctor.Choice (class Choice)

-- | Optic
type Optic p s a t b = p a b -> p s t -- general-purpose Lens
type Optic' p s a = Optic p s s a a

-- | Iso
type Iso s t a b = forall p. Profunctor p => Optic p s t a b -- profunctor
type Iso' s a = Iso s s a a

-- | Lens
type Lens s t a b = forall p. Strong p => Optic p s t a b -- profunctor for product
type Lens' s a = Lens s s a a

-- | Prism
type Prism s t a b = forall p. Choice p => Optic p s t a b -- profunctor for coproduct
type Prism' s a = Prism s s a a
