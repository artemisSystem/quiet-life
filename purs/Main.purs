module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Lib.Serializer (writeData, writeDatum)
import QuietLife.Blocks as Blocks
import QuietLife.Models as Models

main ∷ Effect Unit
main = launchAff_ do
  writeData "generated/resourcepacks/block_assets" Models.models
  writeData "generated/resourcepacks/block_assets" Models.blockstates
  writeDatum "generated" Blocks.blocks