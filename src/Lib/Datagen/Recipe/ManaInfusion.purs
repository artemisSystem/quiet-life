module Lib.Datagen.Recipe.ManaInfusion where

import Lib.Datagen.Recipe (Ingredient)

newtype ManaInfusionRecipeData = ManaInfusionRecipeData
  { input ∷ Ingredient
  , mana ∷ Int
  -- , 
  }