module Lib.Util where

import Data.Map (Map, SemigroupMap(..))
import Data.Semigroup.First (First(..))
import Node.Path (FilePath, sep)
import Prelude ((<>))
import Safe.Coerce (coerce)

appendPath ∷ FilePath → FilePath → FilePath
appendPath a b = a <> sep <> b

infixl 1 appendPath as /

toSMap ∷ ∀ k v. Map k v → SemigroupMap k (First v)
toSMap = coerce
