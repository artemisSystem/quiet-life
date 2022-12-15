module Lib.Datagen.Lang where

import Prelude

import Data.Foldable (intercalate)
import Data.Map (Map, unionWith)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), toUpper)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Foreign.Object as Object
import Foreign.ReadWrite (class WriteForeign, writeForeign)
import Lib.Json (unsafeFormatJson)
import Lib.Serializer (class IsDataType, class Serializable)
import QualifiedDo.Semigroupoid as Compose

newtype Lang = Lang (Map String String)

instance Semigroup Lang where
  append (Lang m1) (Lang m2) = Lang $ unionWith (\a _ → a) m1 m2

instance Monoid Lang where
  mempty = Lang Map.empty

instance IsDataType Lang where
  getFileExtension _ = "json"

instance WriteForeign Lang where
  writeForeign (Lang langMap) = writeForeign
    (Object.fromFoldableWithIndex langMap)

instance Serializable Lang where
  serialize lang = unsafeFormatJson (writeForeign lang)

spaceAndTitleCase ∷ String → String
spaceAndTitleCase = Compose.do
  String.split (Pattern "_")
  map firstToUpper
  intercalate " "
  where
  firstToUpper str = case CodeUnits.uncons str of
    Just { head, tail } → toUpper (CodeUnits.singleton head) <> tail
    Nothing → str

simpleLangName ∷ String → Lang
simpleLangName name = Lang
  (Map.singleton ("block.kdlycontent." <> name) (spaceAndTitleCase name))
