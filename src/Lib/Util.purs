module Lib.Util where

import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Semigroup.First (First(..))
import Data.Symbol (class IsSymbol)
import Node.Path (FilePath, sep)
import Prelude (Unit, (<>))
import Prim.Row as Row
import Run (Run)
import Run.Writer (Writer, tellAt)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy)

appendPath ∷ FilePath → FilePath → FilePath
appendPath a b = a <> sep <> b

infixl 1 appendPath as /

toSMap ∷ ∀ k v. Map k v → SemigroupMap k (First v)
toSMap = coerce

sMapSingleton ∷ ∀ k v. k → v → SemigroupMap k v
sMapSingleton k v = coerce (Map.singleton k v)

tellSingleton
  ∷ ∀ w r t s
  . IsSymbol s
  ⇒ Row.Cons s (Writer (SemigroupMap String w)) t r
  ⇒ Proxy s
  → String
  → w
  → Run r Unit
tellSingleton s key value = tellAt s (sMapSingleton key value)

tellFSingleton
  ∷ ∀ w r t s
  . IsSymbol s
  ⇒ Row.Cons s (Writer (SemigroupMap String (First w))) t r
  ⇒ Proxy s
  → String
  → w
  → Run r Unit
tellFSingleton s key value = tellAt s (toSMap (Map.singleton key value))
