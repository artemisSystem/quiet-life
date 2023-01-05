module QuietLife.Templates where

import Prelude hiding ((/))

import Data.Map as Map
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Foreign.Object as Object
import Lib.Datagen.Blockstate (Blockstate(..), Rotation(..), VariantBlockstate, rotatedVariant, rotatedVariantUvLock, singleVariant)
import Lib.Datagen.KdlyContent.Block (block, box)
import Lib.Datagen.Lang (Lang, simpleLangName)
import Lib.Datagen.Model (Model, itemModel)
import Lib.Datagen.Model as Model
import Lib.Datagen.Recipe (CraftingResult(..), Ingredient(..), Recipe(..), SingleIngredient(..))
import Lib.Datagen.Recipe.ShapedCrafting (ShapedCraftingRecipe)
import Lib.Datagen.ResourceLocation (ResourceLocation(..), getId, (:))
import Lib.Datagen.Tag (TagCollection, singleEntry)
import Lib.Kdl (Kdl, KdlNode, appendProp, appendValue, node, unfoldChildren)
import Lib.Kdl as Kdl
import Lib.OnlyOne (UniqueStrMap, toUMap)
import Lib.Util (tellUSingleton, (/))
import QualifiedDo.Semigroup as S
import QualifiedDo.Unfoldable as U
import QuietLife.Constants (LogDefinition, getLogsTagLocation, isStripped, toStrippedLog)
import Run (Run)
import Run.Writer (Writer, tellAt)
import Type.Proxy (Proxy(..))

type DefaultBlockRows r =
  ( blocks ∷ Writer Kdl
  , blockstates ∷ Writer (UniqueStrMap Blockstate)
  , models ∷ Writer (UniqueStrMap Model)
  , lang ∷ Writer Lang
  , recipes ∷ Writer (UniqueStrMap ShapedCraftingRecipe)
  , block_tags ∷ Writer TagCollection
  , item_tags ∷ Writer TagCollection
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

_item_tags ∷ Proxy "item_tags"
_item_tags = Proxy

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
  # Object.insert "axis=y" (singleVariant hollowLogLocation)
  # Object.insert "axis=z"
      (rotatedVariant (hollowLogLocation <> "_horizontal") R90 R0)
  # VariantBlockstate
  where
  hollowLogLocation = "kdlycontent:block/" <> hollowLogName log

hollowLogModels ∷ LogDefinition → UniqueStrMap Model
hollowLogModels log = Map.empty
  # Map.insert ("block" / hollowLogName log)
      (model "quiet_life:block/templates/hollow_log")
  # Map.insert ("block" / hollowLogName log <> "_horizontal")
      (model "quiet_life:block/templates/hollow_log_horizontal")
  # Map.insert ("item" / hollowLogName log)
      (itemModel ("kdlycontent:block/" <> hollowLogName log))
  # toUMap
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

hollowLogTags
  ∷ ∀ r
  . LogDefinition
  → Run
      (block_tags ∷ Writer TagCollection, item_tags ∷ Writer TagCollection | r)
      Unit
hollowLogTags log = do
  tellAt _block_tags S.do
    singleEntry ("minecraft" : "climbable") logLoc
    -- todo: hexcasting does not add #hexcasting:edified_logs to #mineable/axe
    singleEntry (getLogsTagLocation log) logLoc
  tellAt _item_tags S.do
    singleEntry (getLogsTagLocation log) logLoc
  where
  logLoc = "kdlycontent" : hollowLogName log

hollowLog ∷ ∀ r. LogDefinition → Run (DefaultBlockRows r) Unit
hollowLog log = do
  tellAt _blocks (Kdl.singleton $ hollowLogBlock log)
  tellUSingleton _blockstates (hollowLogName log) (hollowLogBlockstate log)
  tellAt _models (hollowLogModels log)
  tellAt _lang (simpleLangName (hollowLogName log))
  tellUSingleton _recipes (hollowLogName log) (hollowLogRecipe log)
  hollowLogTags log

verticalSlabBlockstate ∷ ResourceLocation → Blockstate
verticalSlabBlockstate (_ : name) = Object.empty
  # Object.insert "type=double,axis=x" (singleVariant fullModel)
  # Object.insert "type=double,axis=z" (singleVariant fullModel)
  # Object.insert "type=top,axis=z" (singleVariant slabModel)
  # Object.insert "type=bottom,axis=x" (rotatedVariantUvLock slabModel R0 R90)
  # Object.insert "type=bottom,axis=z" (rotatedVariantUvLock slabModel R0 R180)
  # Object.insert "type=top,axis=x" (rotatedVariantUvLock slabModel R0 R270)
  # VariantBlockstate
  where
  slabModel = "kdlycontent:block/" <> name <> "_vertical_slab"
  fullModel = slabModel <> "_double"

verticalSlabModels ∷ ResourceLocation → UniqueStrMap Model
verticalSlabModels (namespace : name) = Map.empty
  # Map.insert ("block" / slabName)
      (model "quiet_life:block/templates/vertical_slab")
  # Map.insert ("block" / slabName <> "_double")
      (Model.all "minecraft:block/cube_all" baseTexture)
  # Map.insert ("item" / slabName)
      (itemModel ("kdlycontent:block/" <> slabName))
  # toUMap
  where
  model template = Model.pillar template { side: baseTexture, end: baseTexture }
  baseTexture = namespace <> ":block/" <> name
  slabName = name <> "_vertical_slab"

verticalSlab ∷ ∀ r. ResourceLocation → Run (DefaultBlockRows r) Unit
verticalSlab baseBlock = do
  tellUSingleton _blockstates verticalSlabName
    (verticalSlabBlockstate baseBlock)
  tellAt _models (verticalSlabModels baseBlock)
  where
  verticalSlabName = getId baseBlock <> "_vertical_slab"
