module Main where

import Prelude hiding ((/))

import Effect (Effect)
import Effect.Aff (launchAff_)
import Lib.Datagen.PackMcMeta (PackMcMeta, dataMcmeta, resourceMcmeta)
import Lib.Datagen.Tag (writeTags)
import Lib.OnlyOne (UniqueStrMap)
import Lib.Serializer (writeData, writeDatum)
import Lib.Util (prettyPrintErrors, (/))
import Node.FS.Aff (rm')
import QuietLife.Blocks as Blocks
import QuietLife.Tags as Tags
import QuietLife.Templates (_block_tags, _blocks, _blockstates, _item_tags, _lang, _loot_tables, _models, _recipes)
import Run (Run, runBaseAff)
import Run.Writer (Writer, tellAt)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

_resource_mcmeta ∷ Proxy "resource_mcmeta"
_resource_mcmeta = Proxy

_data_mcmeta ∷ Proxy "data_mcmeta"
_data_mcmeta = Proxy

type ResourceMcMeta r =
  (resource_mcmeta ∷ Writer (UniqueStrMap PackMcMeta) | r)

type DataMcMeta r =
  (data_mcmeta ∷ Writer (UniqueStrMap PackMcMeta) | r)

main ∷ Effect Unit
main = launchAff_ do
  rm' "generated"
    { recursive: true, force: true, maxRetries: 0, retryDelay: 100 }
  Blocks.newBlocks *> Tags.newTags *> mcmeta
    # writeData _resource_mcmeta resourcePackLocation
    # writeData _data_mcmeta dataPackLocation
    # writeDatum _blocks ("generated" / "content") "kdlycontent"
    # writeData _blockstates (assets / "kdlycontent" / "blockstates")
    # writeData _models (assets / "kdlycontent" / "models")
    # writeDatum _lang (assets / "kdlycontent" / "lang") "en_us"
    # writeData _recipes (data_ / "kdlycontent" / "recipes")
    # writeTags _block_tags data_ "blocks"
    # writeTags _item_tags data_ "items"
    # writeData _loot_tables (data_ / "kdlycontent" / "loot_tables" / "blocks")
    # runBaseAff
    # prettyPrintErrors "writing resources"
  where
  resourcePackLocation = "generated" / "resourcepacks" / "core_assets_generated"
  dataPackLocation = "generated" / "datapacks" / "core_data_generated"
  assets = resourcePackLocation / "assets"
  data_ = dataPackLocation / "data"

  mcmeta ∷ ∀ r. Run (ResourceMcMeta + DataMcMeta + r) Unit
  mcmeta = do
    tellAt _resource_mcmeta $ resourceMcmeta "Quiet Life generated assets"
    tellAt _data_mcmeta $ dataMcmeta "Quiet Life generated data"
