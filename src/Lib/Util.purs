module Lib.Util where

import Prelude

import Data.Either (Either(..))
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Symbol (class IsSymbol)
import Effect.Aff (Aff, attempt, message)
import Effect.Class.Console (log)
import Lib.OnlyOne (UniqueRLMap, toUMap)
import Lib.ResourceLocation (ResourceLocation)
import Node.Path (FilePath, sep)
import Prim.Row as Row
import QualifiedDo.Semigroup as S
import Run (Run)
import Run.Writer (Writer, tellAt)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy)

appendPath ∷ FilePath → FilePath → FilePath
appendPath a b = a <> sep <> b

infixl 2 appendPath as /

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

tellUSingleton
  ∷ ∀ w r t s
  . IsSymbol s
  ⇒ Row.Cons s (Writer (UniqueRLMap w)) t r
  ⇒ Proxy s
  → ResourceLocation
  → w
  → Run r Unit
tellUSingleton s key value = tellAt s (toUMap (Map.singleton key value))

prettyPrintErrors ∷ String → Aff Unit → Aff Unit
prettyPrintErrors action aff = attempt aff >>= case _ of
  Right result → pure result
  Left err → log S.do
    "Encountered error while "
    action
    ". Error message: "
    message err
