module Main where

import Prelude hiding ((/))

import Data.Map (SemigroupMap)
import Data.Semigroup.First (First)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Lib.Datagen.PackMcMeta (PackMcMeta, dataMcmeta, resourceMcmeta)
import Lib.Serializer (writeData, writeDatum)
import Lib.Util ((/))
import Node.FS.Aff (rm')
import QuietLife.Blocks as Blocks
import QuietLife.Templates (_blocks, _blockstates, _lang, _models, _recipes)
import Run (Run, runBaseAff)
import Run.Writer (Writer, tellAt)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

_resource_mcmeta ∷ Proxy "resource_mcmeta"
_resource_mcmeta = Proxy

_data_mcmeta ∷ Proxy "data_mcmeta"
_data_mcmeta = Proxy

type ResourceMcMeta r =
  (resource_mcmeta ∷ Writer (SemigroupMap String (First PackMcMeta)) | r)

type DataMcMeta r =
  (data_mcmeta ∷ Writer (SemigroupMap String (First PackMcMeta)) | r)

main ∷ Effect Unit
main = launchAff_ do
  rm' "generated"
    { recursive: true, force: true, maxRetries: 0, retryDelay: 100 }
  Blocks.newBlocks *> mcmeta
    # writeData _resource_mcmeta resourcePackLocation
    # writeData _data_mcmeta dataPackLocation
    # writeDatum _blocks ("generated" / "content") "kdlycontent"
    # writeData _blockstates (assets / "kdlycontent" / "blockstates")
    # writeData _models (assets / "kdlycontent" / "models")
    # writeDatum _lang (assets / "kdlycontent" / "lang") "en_us"
    # writeData _recipes (data_ / "kdlycontent" / "recipes")
    # runBaseAff
  where
  resourcePackLocation = "generated" / "resources" / "core_assets_generated"
  dataPackLocation = "generated" / "data" / "core_data_generated"
  assets = resourcePackLocation / "assets"
  data_ = dataPackLocation / "data"

  mcmeta ∷ ∀ r. Run (ResourceMcMeta + DataMcMeta + r) Unit
  mcmeta = do
    tellAt _resource_mcmeta $ resourceMcmeta "Quiet Life generated assets"
    tellAt _data_mcmeta $ dataMcmeta "Quiet Life generated data"
