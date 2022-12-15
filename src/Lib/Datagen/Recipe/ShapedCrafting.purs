module Lib.Datagen.Recipe.ShapedCrafting where

import Foreign.Object (Object)
import Lib.Datagen.Recipe (CraftingResult, Ingredient, Recipe)

type ShapedCraftingRecipeFields =
  ( pattern ∷ Array String
  , key ∷ Object Ingredient
  , result ∷ CraftingResult
  )

type ShapedCraftingRecipe = Recipe "crafting_shaped" ShapedCraftingRecipeFields
