module Lib.Datagen.Texture
  ( Pixel
  , Pixels
  , Palette

  , getTexture
  , getPalette

  , applyPalette
  , writeTexture
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Foldable (foldMap)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Vector.Polymorphic (Vector2, (><))
import Effect (Effect)
import Effect.Aff (error, throwError)
import Node.Buffer (Octet)
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Buffer.Immutable as IB
import Node.Path (FilePath)
import Run (AFF, Run, liftAff)
import Type.Row (type (+))

type Pixel = { r ∷ Octet, g ∷ Octet, b ∷ Octet, a ∷ Octet }

type Pixels = List Pixel

type Palette = Map Pixel Pixel

type Texture = { pixels ∷ Pixels, dimensions ∷ Vector2 Int }

pixelsToBuffer ∷ Pixels → ImmutableBuffer
pixelsToBuffer = foldMap (\{ r, g, b, a } → [ r, g, b, a ]) >>> IB.fromArray

pixelsFromBuffer ∷ ImmutableBuffer → Pixels
pixelsFromBuffer =
  IB.toArray >>> List.fromFoldable >>> go Nil >>> List.reverse
  where
  go result (r : g : b : a : rest) = go ({ r, g, b, a } : result) rest
  go result _ = result

paletteFromTexture ∷ Texture → Maybe Palette
paletteFromTexture { pixels, dimensions: (x >< 2) } = Just do
  let
    keys = List.take x pixels
    values = List.drop x pixels
  Map.fromFoldable (List.zip keys values)
paletteFromTexture _ = Nothing

applyPalette ∷ Palette → Texture → Texture
applyPalette palette texture@{ pixels } = texture
  { pixels = pixels <#> \pixel →
      case Map.lookup pixel palette of
        Just newPixel → newPixel
        Nothing → pixel
  }

foreign import readRawTextureData
  ∷ FilePath
  → Effect
      ( Promise
          { width ∷ Int
          , height ∷ Int
          , channels ∷ Int
          , buffer ∷ ImmutableBuffer
          }
      )

getTexture ∷ ∀ r. FilePath → Run (AFF + r) Texture
getTexture file = liftAff do
  { width, height, channels, buffer } ← toAffE (readRawTextureData file)
  when (channels /= 4) do
    throwError (error $ "Image " <> file <> " does not have 4 channels.")
  pure { dimensions: width >< height, pixels: pixelsFromBuffer buffer }

getPalette ∷ ∀ r. FilePath → Run (AFF + r) Palette
getPalette file = getTexture file <#> paletteFromTexture >>= case _ of
  Just palette → pure palette
  Nothing → liftAff do
    throwError (error $ "Palette " <> file <> " is not 2 pixels tall")

foreign import writeRawTextureData
  ∷ FilePath
  → { width ∷ Int, height ∷ Int, buffer ∷ ImmutableBuffer }
  → Effect (Promise Unit)

writeTexture ∷ ∀ r. FilePath → Texture → Run (AFF + r) Unit
writeTexture file { pixels, dimensions: (width >< height) } = liftAff $ toAffE $
  (writeRawTextureData file { width, height, buffer: pixelsToBuffer pixels })

