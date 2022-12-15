module QuietLife.Constants where

import Prelude

import Data.Maybe (isJust)
import Data.String (Pattern(..), stripPrefix)
import QualifiedDo.Semigroup as S
import Record as Record
import Type.Proxy (Proxy(..))

type LogDefinition =
  { namespace ∷ String, name ∷ String, logSuffix ∷ String, woodSuffix ∷ String }

toStrippedLog ∷ LogDefinition → LogDefinition
toStrippedLog = Record.modify (Proxy ∷ _ "name") ("stripped_" <> _)

isStripped ∷ LogDefinition → Boolean
isStripped { name } = isJust $ stripPrefix (Pattern "stripped_") name

existingLogs ∷ Array LogDefinition
existingLogs = S.do
  [ "oak", "spruce", "birch", "jungle", "dark_oak", "acacia", "mangrove" ] <#>
    { namespace: "minecraft", logSuffix: "log", woodSuffix: "wood", name: _ }
  [ "warped", "crimson" ] <#>
    { namespace: "minecraft", logSuffix: "stem", woodSuffix: "hyphae", name: _ }
  [ "livingwood", "dreamwood" ] <#>
    { namespace: "botania", logSuffix: "log", woodSuffix: "", name: _ }

newLogs ∷ Array LogDefinition
newLogs = S.do
  [ "maple", "scarlet", "violet" ] <#>
    { namespace: "kdlycontent", logSuffix: "log", woodSuffix: "wood", name: _ }
  [ "red_mushroom", "brown_mushroom" ] <#>
    { namespace: "kdlycontent"
    , logSuffix: "stem"
    , woodSuffix: "hyphae"
    , name: _
    }

allLogs ∷ Array LogDefinition
allLogs = existingLogs {- <> newLogs -}

allLogsWithStripped ∷ Array LogDefinition
allLogsWithStripped = allLogs >>= \x → [ x, toStrippedLog x ]
