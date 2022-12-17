module Lib.Datagen.Recipe where

import Prelude

import Data.Reflectable (class Reflectable, reflectType)
import Foreign.ReadWrite (class WriteForeign, writeForeign)
import Lib.Json (unsafeFormatJson)
import Lib.Serializer (class IsDataType, class Serializable)
import Record (union)
import Type.Proxy (Proxy(..))

newtype Recipe (id ∷ Symbol) (recipe ∷ Row Type) = Recipe (Record recipe)

instance IsDataType (Recipe id fields) where
  getFileExtension _ = "json"

instance
  ( Reflectable id String
  , WriteForeign { type ∷ String | fields }
  ) ⇒
  WriteForeign (Recipe id fields) where
  writeForeign (Recipe recipe) = recipe
    # union { type: id }
    # writeForeign
    where
    id = reflectType (Proxy ∷ _ id)

instance
  WriteForeign (Recipe id fields) ⇒
  Serializable (Recipe id fields) where
  serialize recipe = unsafeFormatJson (writeForeign recipe)

data SingleIngredient
  = TagIngredient String
  | ItemIngredient String

data Ingredient
  = Single SingleIngredient
  | Multiple (Array Ingredient)

instance WriteForeign Ingredient where
  writeForeign (Single (TagIngredient tag)) = writeForeign { tag }
  writeForeign (Single (ItemIngredient item)) = writeForeign { item }
  writeForeign (Multiple array) = writeForeign array

newtype CraftingResult = CraftingResult
  { count ∷ Int
  , item ∷ String
  }

instance WriteForeign CraftingResult where
  writeForeign (CraftingResult result) = writeForeign result
