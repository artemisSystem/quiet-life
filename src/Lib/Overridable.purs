module Lib.Overridable where

import Prelude

import Data.Eq (class Eq1)
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Lazy (Lazy, force)
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Ord (class Ord1)
import Data.Show.Generic (genericShow)
import Lib.ResourceLocation (ResourceLocation)

data Overridable a = Override a | Default (Lazy a)

derive instance Generic (Overridable a) _

derive instance Eq a ⇒ Eq (Overridable a)
derive instance Eq1 Overridable

derive instance Ord a ⇒ Ord (Overridable a)
derive instance Ord1 Overridable

instance Show a ⇒ Show (Overridable a) where
  show = genericShow

derive instance Functor Overridable

instance Semigroup a ⇒ Semigroup (Overridable a) where
  append (Override a) (Override b) = Override (a <> b)
  append (Override a) (Default _) = Override a
  append (Default _) (Override b) = Override b
  append (Default a) (Default b) = Default (a <> b)

instance Monoid a ⇒ Monoid (Overridable a) where
  mempty = Default mempty

getOverridable ∷ ∀ a. Overridable a → a
getOverridable (Override a) = a
getOverridable (Default a) = force a

type Overridables k v = SemigroupMap k (Overridable v)

type OverridablesRL v = Overridables ResourceLocation v

default ∷ ∀ k v. k → Lazy v → Overridables k v
default k v = SemigroupMap (Map.singleton k (Default v))

override ∷ ∀ k v. k → v → Overridables k v
override k v = SemigroupMap (Map.singleton k (Override v))

foldOverridables ∷ ∀ k v. Monoid v ⇒ Overridables k v → v
foldOverridables = foldMap getOverridable
