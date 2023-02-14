module Lib.Util where

import Prelude

import Data.Either (Either(..))
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Symbol (class IsSymbol)
import Effect.Aff (Aff, attempt, message)
import Effect.Class.Console (log)
import Lib.OnlyOne (OnlyOne, uSingleton)
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
  ∷ ∀ k w r t s
  . IsSymbol s
  ⇒ Row.Cons s (Writer (SemigroupMap k w)) t r
  ⇒ Proxy s
  → k
  → w
  → Run r Unit
tellSingleton sym key value = tellAt sym (sMapSingleton key value)

tellUSingleton
  ∷ ∀ k w r t s
  . IsSymbol s
  ⇒ Row.Cons s (Writer (SemigroupMap k (OnlyOne w))) t r
  ⇒ Proxy s
  → k
  → w
  → Run r Unit
tellUSingleton sym key value = tellAt sym (uSingleton key value)

prettyPrintErrors ∷ String → Aff Unit → Aff Unit
prettyPrintErrors action aff = attempt aff >>= case _ of
  Right result → pure result
  Left err → log S.do
    "Encountered error while "
    action
    ". Error message: "
    message err
