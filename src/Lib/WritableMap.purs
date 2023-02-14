-- | Map with Semigroup and WriteForeign instances.
module Lib.WritableMap where

import Prelude

import Data.Map (empty, unionWith)
import Data.Map.Internal (Map(..))
import Foreign.Object as Object
import Foreign.ReadWrite (class WriteForeign, writeForeign)
import Lib.ResourceLocation (ResourceLocation)
import Lib.ResourceLocation as RL
import Safe.Coerce (coerce)

newtype WritableMap k v = WritableMap (Map k v)

instance (Ord k, Semigroup v) ⇒ Semigroup (WritableMap k v) where
  append (WritableMap l) (WritableMap r) = WritableMap (unionWith append l r)

instance (Ord k, Semigroup v) ⇒ Monoid (WritableMap k v) where
  mempty = WritableMap empty

instance (WriteForeign v) ⇒ WriteForeign (WritableMap String v) where
  writeForeign map = writeForeign $
    Object.fromFoldableWithIndex (coerce map ∷ Map String v)

instance (WriteForeign v) ⇒ WriteForeign (WritableMap ResourceLocation v) where
  writeForeign = transformKey RL.toStr >>> writeForeign

transformKey ∷ ∀ k1 k2 v. (k1 → k2) → WritableMap k1 v → WritableMap k2 v
transformKey f (WritableMap map) = WritableMap $ transformKey' map
  where
  transformKey' Leaf = Leaf
  transformKey' (Two m1 k v m2) =
    Two (transformKey' m1) (f k) v (transformKey' m2)
  transformKey' (Three m1 k1 v1 m2 k2 v2 m3) = Three
    (transformKey' m1)
    (f k1)
    v1
    (transformKey' m2)
    (f k2)
    v2
    (transformKey' m3)