module QuietLife.Constants where

import Prelude

import Data.Maybe (isJust)
import Data.String (Pattern(..), Replacement(..), stripPrefix)
import Data.String as String
import QualifiedDo.Semigroup as S
import Record as Record
import Type.Proxy (Proxy(..))

type LogDefinition =
  { namespace ∷ String
  , name ∷ String
  , logSuffix ∷ String
  , woodSuffix ∷ String
  , tagSuffix ∷ String
  }

logTemplate ∷ String → String → String → String → String → LogDefinition
logTemplate namespace logSuffix woodSuffix tagSuffix name =
  { namespace, logSuffix, woodSuffix, tagSuffix, name }

toStrippedLog ∷ LogDefinition → LogDefinition
toStrippedLog = Record.modify (Proxy ∷ _ "name") \name →
  if String.contains (Pattern "glimmering_") name then String.replace
    (Pattern "glimmering_")
    (Replacement "glimmering_stripped_")
    name
  else ("stripped_" <> name)

isStripped ∷ LogDefinition → Boolean
isStripped { name } = isJust $ stripPrefix (Pattern "stripped_") name

existingLogs ∷ Array LogDefinition
existingLogs = S.do
  [ "oak", "spruce", "birch", "jungle", "dark_oak", "acacia", "mangrove" ]
    <#> logTemplate "minecraft" "log" "wood" "logs"
  [ "warped", "crimson" ] <#> logTemplate "minecraft" "stem" "hyphae" "stems"
  [ "livingwood", "glimmering_livingwood", "dreamwood", "glimmering_dreamwood" ]
    <#> logTemplate "botania" "log" "wood" "logs"

newLogs ∷ Array LogDefinition
newLogs = S.do
  [ "maple", "scarlet", "violet" ] <#>
    logTemplate "kdlycontent" "log" "wood" "logs"
  [ "red_mushroom", "brown_mushroom" ] <#>
    logTemplate "kdlycontent" "stem" "hyphae" "stems"

getTagName ∷ String → String → String
getTagName suffix name = name <> "_" <> suffix

allLogs ∷ Array LogDefinition
allLogs = existingLogs {- <> newLogs -}

allLogsWithStripped ∷ Array LogDefinition
allLogsWithStripped = allLogs >>= \x → [ x, toStrippedLog x ]
