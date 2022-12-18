module QuietLife.Templates where

import Prelude hiding ((/))

import Data.Map (SemigroupMap)
import Data.Map as Map
import Data.Semigroup.First (First)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Foreign.Object as Object
import Lib.Datagen.Blockstate (Blockstate(..), Rotation(..), rotatedVariant)
import Lib.Datagen.KdlyContent.Block (block, box)
import Lib.Datagen.Lang (Lang, simpleLangName)
import Lib.Datagen.Model (Model, itemModel)
import Lib.Datagen.Model as Model
import Lib.Datagen.Recipe (CraftingResult(..), Ingredient(..), Recipe(..), SingleIngredient(..))
import Lib.Datagen.Recipe.ShapedCrafting (ShapedCraftingRecipe)
import Lib.Datagen.ResourceLocation ((:))
import Lib.Datagen.Tag (TagCollection, singleEntry)
import Lib.Kdl (Kdl, KdlNode, appendProp, appendValue, node, unfoldChildren)
import Lib.Kdl as Kdl
import Lib.Util (tellFSingleton, toSMap, (/))
import QualifiedDo.Semigroup as S
import QualifiedDo.Unfoldable as U
import QuietLife.Constants (LogDefinition, isStripped, toStrippedLog)
import Run (Run)
import Run.Writer (Writer, tellAt)
import Type.Proxy (Proxy(..))

type DefaultBlockRows r =
  ( blocks ∷ Writer Kdl
  , blockstates ∷ Writer (SemigroupMap String (First Blockstate))
  , models ∷ Writer (SemigroupMap String (First Model))
  , lang ∷ Writer Lang
  , recipes ∷ Writer (SemigroupMap String (First ShapedCraftingRecipe))
  , block_tags ∷ Writer TagCollection
  | r
  )

_blocks ∷ Proxy "blocks"
_blocks = Proxy

_blockstates ∷ Proxy "blockstates"
_blockstates = Proxy

_models ∷ Proxy "models"
_models = Proxy

_lang ∷ Proxy "lang"
_lang = Proxy

_recipes ∷ Proxy "recipes"
_recipes = Proxy

_block_tags ∷ Proxy "block_tags"
_block_tags = Proxy

hollowLogName ∷ LogDefinition → String
hollowLogName log = "hollow_" <> log.name <> "_" <> log.logSuffix

hollowLogBlock ∷ LogDefinition → KdlNode
hollowLogBlock log = block (hollowLogName log) # unfoldChildren U.do
  node "type" # appendValue "custom" # unfoldChildren U.do
    node "properties" # unfoldChildren U.do
      node "waterloggable"
      node "rotation"
        # appendProp "type" "axis"
        # appendProp "placement" "side"
    node "shape" # unfoldChildren U.do
      box 0 0 0 16 16 2
      box 0 0 14 16 16 16
      box 0 0 2 2 16 14
      box 14 0 2 16 16 14
      box 2 0 2 3 16 3
      box 13 0 2 14 16 3
      box 2 0 13 3 16 14
      box 13 0 13 14 16 14
  node "settings" # appendProp "copy" logResourceLocation
  node "item"
  where
  logResourceLocation = log.namespace <> ":" <> log.name <> "_" <> log.logSuffix

hollowLogBlockstate ∷ LogDefinition → Blockstate
hollowLogBlockstate log = Object.empty
  # Object.insert "axis=x"
      (rotatedVariant (hollowLogLocation <> "_horizontal") R90 R90)
  # Object.insert "axis=y" (rotatedVariant (hollowLogLocation) R0 R0)
  # Object.insert "axis=z"
      (rotatedVariant (hollowLogLocation <> "_horizontal") R90 R0)
  # VariantBlockstate
  where
  hollowLogLocation = "kdlycontent:block/" <> hollowLogName log

hollowLogModels ∷ LogDefinition → SemigroupMap String (First Model)
hollowLogModels log = Map.empty
  # Map.insert ("block" / hollowLogName log)
      (model "quiet_life:block/templates/hollow_log")
  # Map.insert ("block" / hollowLogName log <> "_horizontal")
      (model "quiet_life:block/templates/hollow_log_horizontal")
  # Map.insert ("item" / hollowLogName log)
      (itemModel ("kdlycontent:block/" <> hollowLogName log))
  # toSMap
  where
  model template = Model.hollowPillar template
    { end: String.replace (Pattern "glimmering_") (Replacement "") $
        texture log <> "_top"
    , inside:
        if isStripped log then texture log
        else texture (toStrippedLog log)
    , side: texture log
    }
  texture { namespace, name, logSuffix } =
    namespace <> ":block/" <> name <> "_" <> logSuffix

hollowLogRecipe
  ∷ LogDefinition → ShapedCraftingRecipe
hollowLogRecipe log = Recipe
  { pattern:
      [ "# #"
      , "# #"
      ]
  , key: Object.singleton "#" (Single $ ItemIngredient logLocation)
  , result: CraftingResult { count: 4, item: hollowLogLocation }
  }
  where
  logLocation = log.namespace <> ":" <> log.name <> "_" <> log.logSuffix
  hollowLogLocation = "kdlycontent:" <> hollowLogName log

hollowLogTags ∷ LogDefinition → TagCollection
hollowLogTags log = S.do
  singleEntry ("minecraft" : "climbable") logLoc
  singleEntry ("minecraft" : "mineable/axe") logLoc
  where
  logLoc = "kdlycontent" : hollowLogName log

hollowLog ∷ ∀ r. LogDefinition → Run (DefaultBlockRows r) Unit
hollowLog log = do
  tellAt _blocks (Kdl.singleton $ hollowLogBlock log)
  tellFSingleton _blockstates (hollowLogName log) (hollowLogBlockstate log)
  tellAt _models (hollowLogModels log)
  tellAt _lang (simpleLangName (hollowLogName log))
  tellFSingleton _recipes (hollowLogName log) (hollowLogRecipe log)
  tellAt _block_tags (hollowLogTags log)
