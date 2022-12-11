module Lib.Datagen.Recipe.ManaInfusion where

import Prelude

import Lib.Datagen.Recipe (Ingredient)

newtype ManaInfusionRecipe = ManaInfusionRecipe
  { input ∷ Ingredient
  , mana ∷ Int
  -- , 
  }