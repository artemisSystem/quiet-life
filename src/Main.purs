module Main where

import Prelude

import Control.Monad.Except (lift, runExceptT)
import Data.Either (Either(..))
import Data.Traversable (traverse, traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log, logShow)
import Foreign (FT, renderForeignError)
import Lib.Datagen.Worldgen.DensityFunction (DensityFunction(..), DirectDensityFunction(..))
import Lib.Deserializer (readData)
import Lib.Serializer (DataPack, Serializer, serializer, writeData, writeDatum)
import Node.FS.Aff (rm')
import QuietLife.Blocks as Blocks
import QuietLife.Models as Models

app ∷ FT Aff Unit
app = do
  df ←
    readData "vanilla_json/overworld_final_density.json"
      ∷ _ DensityFunction
  let
    newDf = RangeChoice
      { input: Direct $ Interpolated $ Direct $ FlatCache $ Reference
          "minecraft:overworld/ridges"
      , min_inclusive: 0.0
      , max_exclusive: 999.0
      , when_in_range: Direct $ EndIslands
      -- , when_in_range: Direct $ YClampedGradient
      --     { from_y: 69
      --     , to_y: 71
      --     , from_value: 1.0
      --     , to_value: -1.0
      --     }
      , when_out_of_range: df
      }

    ser
      ∷ Array
          ( Serializer (DataPack "quiet_life" "worldgen/density_function")
              DirectDensityFunction
          )
    ser = [ serializer "test" newDf ]
  lift $ writeData "generated/test" ser

main ∷ Effect Unit
main = launchAff_ do
  rm' "generated"
    { recursive: true, force: true, maxRetries: 0, retryDelay: 100 }
  writeData "generated/resourcepacks/block_assets" Models.models
  writeData "generated/resourcepacks/block_assets" Models.blockstates
  writeDatum "generated" Blocks.blocks
  runExceptT app >>= case _ of
    Right _ → pure unit
    Left e → traverse_ logShow e
