module Main where

import Prelude hiding ((/))

import Effect (Effect)
import Effect.Aff (launchAff_)
import Lib.Datagen.PackMcMeta (dataMcmeta, resourceMcmeta)
import Lib.Datagen.Tag (writeTags)
import Lib.Serializer (writeData, writeDatum, writeFile)
import Lib.Util (prettyPrintErrors, (/))
import Node.FS.Aff (rm')
import QuietLife.Blocks as Blocks
import QuietLife.Tags as Tags
import QuietLife.Templates (_block_tags, _blocks, _blockstates, _item_tags, _lang, _loot_tables, _models, _recipes)
import Run (runBaseAff)

main ∷ Effect Unit
main = launchAff_ do
  rm' "generated"
    { recursive: true, force: true, maxRetries: 0, retryDelay: 100 }
  writeFile (resourcePackLocation / "pack")
    (resourceMcmeta "Quiet Life generated assets")
  writeFile (dataPackLocation / "pack") (dataMcmeta "Quiet Life generated data")
  Blocks.newBlocks *> Tags.newTags
    # writeDatum _blocks ("generated" / "content") "kdlycontent"
    # writeData _blockstates assets "blockstates"
    # writeData _models assets "models"
    # writeDatum _lang (assets / "kdlycontent" / "lang") "en_us"
    # writeData _recipes data_ "recipes"
    # writeTags _block_tags data_ "blocks"
    # writeTags _item_tags data_ "items"
    # writeData _loot_tables data_ ("loot_tables" / "blocks")
    # runBaseAff
    # prettyPrintErrors "writing resources"
  where
  resourcePackLocation = "generated" / "resourcepacks" / "core_assets_generated"
  dataPackLocation = "generated" / "datapacks" / "core_data_generated"
  assets = resourcePackLocation / "assets"
  data_ = dataPackLocation / "data"
