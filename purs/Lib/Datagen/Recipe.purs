module Lib.Datagen.Recipe where

import Prelude

import Data.Reflectable (class Reflectable, reflectType)
import Foreign.ReadWrite (class WriteForeign, writeForeign)
import Lib.Serializer (class IsDataType)
import Record (union)
import Type.Proxy (Proxy(..))

newtype Recipe (id ∷ Symbol) (recipe ∷ Type) = Recipe recipe

instance IsDataType (Recipe id fields) where
  getFileExtension _ = "json"

instance
  ( Reflectable id String
  , WriteForeign { id ∷ String | fields }
  ) ⇒
  WriteForeign (Recipe id (Record fields)) where
  writeForeign (Recipe record) = record
    # union { id }
    # writeForeign
    where
    id = reflectType (Proxy ∷ _ id)

data SingleIngredient
  = Tag String
  | Item String

data Ingredient
  = Single SingleIngredient
  | Multiple (Array Ingredient)

instance WriteForeign Ingredient where
  writeForeign (Single (Tag tag)) = writeForeign { tag }
  writeForeign (Single (Item item)) = writeForeign { item }
  writeForeign (Multiple array) = writeForeign array

newtype CraftingResult = CraftingResult
  { count ∷ Int
  , item ∷ String
  }

instance WriteForeign CraftingResult where
  writeForeign (CraftingResult result) = writeForeign result
