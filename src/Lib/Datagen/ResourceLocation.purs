module Lib.Datagen.ResourceLocation where

import Prelude

import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Foreign.ReadWrite (class WriteForeign, writeForeign)
import Node.Path (sep)

newtype ResourceLocation = ResourceLocation { namespace ∷ String, id ∷ String }

derive newtype instance Eq ResourceLocation

instance Ord ResourceLocation where
  compare (ResourceLocation rl1) (ResourceLocation rl2) = compare
    [ rl1.namespace, rl1.id ]
    [ rl2.namespace, rl2.id ]

instance WriteForeign ResourceLocation where
  writeForeign rl = writeForeign (toStr rl)

toStr ∷ ResourceLocation → String
toStr (ResourceLocation { namespace, id }) = namespace <> ":" <> id

resourceLocation ∷ String → String → ResourceLocation
resourceLocation namespace id = ResourceLocation { namespace, id }

getNamespace ∷ ResourceLocation → String
getNamespace (ResourceLocation { namespace }) = namespace

getId ∷ ResourceLocation → String
getId (ResourceLocation { id }) = id

getIdAsPath ∷ ResourceLocation → String
getIdAsPath = getId >>> String.replace (Pattern "/") (Replacement sep)

infix 2 resourceLocation as :
