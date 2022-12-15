module Main where

import Prelude hiding ((/))

import Data.Map (SemigroupMap)
import Data.Map as Map
import Data.Semigroup.First (First)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Lib.Datagen.PackMcMeta (PackMcMeta, resourceMcmeta)
import Lib.Serializer (writeData, writeDatum)
import Lib.Util (toSMap, (/))
import Node.FS.Aff (rm')
import QuietLife.Blocks as Blocks
import QuietLife.Templates (_blocks, _blockstates, _models)
import Run (Run, runBaseAff)
import Run.Writer (Writer, tellAt)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

_resource_mcmeta ∷ Proxy "resource_mcmeta"
_resource_mcmeta = Proxy

type ResourceMcMeta r =
  (resource_mcmeta ∷ Writer (SemigroupMap String (First PackMcMeta)) | r)

main ∷ Effect Unit
main = launchAff_ do
  rm' "generated"
    { recursive: true, force: true, maxRetries: 0, retryDelay: 100 }
  Blocks.newBlocks *> mcmeta
    # writeData _resource_mcmeta packLocation
    # writeDatum _blocks ("generated" / "content") "kdlycontent"
    # writeData _blockstates (assets / "kdlycontent" / "blockstates")
    # writeData _models (assets / "kdlycontent" / "models")
    # runBaseAff
  where
  packLocation = "generated" / "resources" / "core_assets_generated"
  assets = packLocation / "assets"

  mcmeta ∷ ∀ r. Run (ResourceMcMeta + r) Unit
  mcmeta = tellAt _resource_mcmeta $ toSMap
    (Map.singleton "pack" $ resourceMcmeta "Quiet Life generated assets")
