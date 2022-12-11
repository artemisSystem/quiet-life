module Lib.Datagen.Recipe.ShapedCrafting where

import Prelude

import Foreign.Object (Object)
import Foreign.ReadWrite (class WriteForeign, writeForeign)
import Lib.Datagen.Recipe (CraftingResult(..), Ingredient)
import Lib.Serializer (class IsDataType)

newtype ShapedCraftingRecipe = ShapedCraftingRecipe
  { pattern ∷ Array String
  , key ∷ Object Ingredient
  , result ∷ CraftingResult
  }

instance WriteForeign ShapedCraftingRecipe where
  writeForeign (ShapedCraftingRecipe recipe) = writeForeign recipe