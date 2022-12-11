module QuietLife.Constants where

import Prelude

import QualifiedDo.Semigroup as S

type LogDefinition =
  { namespace ∷ String, name ∷ String, logSuffix ∷ String, woodSuffix ∷ String }

existingLogs ∷ Array LogDefinition
existingLogs = S.do
  [ "oak", "spruce", "birch", "jungle", "dark_oak", "acacia" ] <#>
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
allLogs = existingLogs <> newLogs
