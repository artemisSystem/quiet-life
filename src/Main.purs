module Main where

import Prelude hiding ((/))

import Effect (Effect)
import Effect.Aff (launchAff_)
import Lib.Serializer (writeData, writeDatum)
import Node.FS.Aff (rm')
import QuietLife.Blocks as Blocks
import QuietLife.Templates (_blocks, _blockstates, _models)
import Run (runBaseAff)
import Lib.Util ((/))

main ∷ Effect Unit
main = launchAff_ do
  rm' "generated"
    { recursive: true, force: true, maxRetries: 0, retryDelay: 100 }
  Blocks.newBlocks
    # writeDatum _blocks ("generated" / "content") "kdlycontent"
    # writeData _blockstates (assets / "kdlycontent" / "blockstates")
    # writeData _models (assets / "kdlycontent" / "models")
    # runBaseAff
  where
  assets = "generated" / "resources" / "core_assets_generated" / "assets"
