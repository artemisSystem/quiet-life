module Lib.Datagen.ResourceLocation where

import Prelude

import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Foreign.ReadWrite (class WriteForeign, writeForeign)
import Node.Path (sep)

data ResourceLocation = ResourceLocation String String

infix 1 ResourceLocation as :

instance Eq ResourceLocation where
  eq (ns1 : id1) (ns2 : id2) = ns1 == ns2 && id1 == id2

instance Ord ResourceLocation where
  compare (ResourceLocation ns1 id1) (ResourceLocation ns2 id2) = compare
    [ ns1, id1 ]
    [ ns2, id2 ]

instance WriteForeign ResourceLocation where
  writeForeign rl = writeForeign (toStr rl)

toStr ∷ ResourceLocation → String
toStr (namespace : id) = namespace <> ":" <> id

getNamespace ∷ ResourceLocation → String
getNamespace (namespace : _) = namespace

getId ∷ ResourceLocation → String
getId (_ : id) = id

getIdAsPath ∷ ResourceLocation → String
getIdAsPath = getId >>> String.replace (Pattern "/") (Replacement sep)
