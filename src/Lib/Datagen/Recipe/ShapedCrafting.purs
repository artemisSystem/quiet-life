module Lib.Datagen.Recipe.ShapedCrafting where

import Foreign.Object (Object)
import Foreign.ReadWrite (class WriteForeign, writeForeign)
import Lib.Datagen.Recipe (CraftingResult, Ingredient, Recipe)
import Lib.Json (unsafeFormatJson)
import Lib.Serializer (class Serializable)

newtype ShapedCraftingRecipeData = ShapedCraftingRecipeData
  { pattern ∷ Array String
  , key ∷ Object Ingredient
  , result ∷ CraftingResult
  }

type ShapedCraftingRecipe = Recipe "crafting_shaped" ShapedCraftingRecipeData

instance WriteForeign ShapedCraftingRecipeData where
  writeForeign (ShapedCraftingRecipeData recipe) = writeForeign recipe

instance Serializable ShapedCraftingRecipeData where
  serialize recipe = unsafeFormatJson (writeForeign recipe)
