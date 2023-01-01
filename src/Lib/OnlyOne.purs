module Lib.OnlyOne where

import Prelude

import Data.Eq (class Eq1)
import Data.Generic.Rep (class Generic)
import Data.Map (Map, SemigroupMap(..))
import Data.Ord (class Ord1)
import Data.Show.Generic (genericShow)
import Safe.Coerce (coerce)

data OnlyOne a = One a | MoreThanOne (Array a)

derive instance Generic (OnlyOne a) _

derive instance Eq a ⇒ Eq (OnlyOne a)
derive instance Eq1 OnlyOne

derive instance Ord a ⇒ Ord (OnlyOne a)
derive instance Ord1 OnlyOne

instance Show a ⇒ Show (OnlyOne a) where
  show = genericShow

derive instance Functor OnlyOne

instance Semigroup (OnlyOne a) where
  append (One a) (One b) = MoreThanOne [ a, b ]
  append (One a) (MoreThanOne bs) = MoreThanOne ([ a ] <> bs)
  append (MoreThanOne as) (One b) = MoreThanOne (as <> [ b ])
  append (MoreThanOne as) (MoreThanOne bs) = MoreThanOne (as <> bs)

type UniqueStrMap v = SemigroupMap String (OnlyOne v)

toUMap ∷ ∀ v. Map String v → UniqueStrMap v
toUMap = map One >>> coerce
