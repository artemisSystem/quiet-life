module Lib.Kdl
  ( KdlValue(..)
  , KdlNode
  , Kdl(Kdl)

  , formatKdl

  , singleton
  , node
  , appendChild
  , appendChildren

  , class IsKdlValue
  , toKdlValue

  , appendProp
  , appendValue
  ) where

import Prelude

import Data.Newtype (class Newtype)
import Foreign (Foreign)
import Foreign.Object (Object, insert)
import Foreign.Object as Object
import Foreign.ReadWrite (class WriteForeign, writeForeign)
import Lib.Serializer (class IsDataType, class Serializable)
import Record (disjointUnion)

foreign import null_ ∷ Foreign

data KdlValue
  = KdlString String
  | KdlNumber Number
  | KdlBoolean Boolean
  | KdlNull

type KdlNode =
  { name ∷ String
  , properties ∷ Object KdlValue
  , values ∷ Array KdlValue
  , children ∷ Kdl
  }

newtype Kdl = Kdl (Array KdlNode)

instance WriteForeign KdlValue where
  writeForeign (KdlString str) = writeForeign str
  writeForeign (KdlNumber num) = writeForeign num
  writeForeign (KdlBoolean bool) = writeForeign bool
  writeForeign (KdlNull) = null_

derive instance Newtype Kdl _

derive newtype instance Semigroup Kdl
derive newtype instance Monoid Kdl

instance WriteForeign Kdl where
  writeForeign (Kdl kdl) =
    writeForeign
      ( kdl <#> disjointUnion
          -- Add empty type annotations, we don't need to generate them, but
          -- kdljs requires them (annoyingly)
          { tags:
              { properties: {}
              , values: [] ∷ Array String
              }
          }
      )

instance IsDataType Kdl where
  getFileExtension _ = "kdl"

foreign import unsafeFormatKdl ∷ Foreign → String

instance Serializable Kdl where
  serialize = formatKdl

formatKdl ∷ Kdl → String
formatKdl = writeForeign >>> unsafeFormatKdl

node ∷ String → KdlNode
node name =
  { name, properties: Object.empty, values: [], children: mempty }

singleton ∷ KdlNode → Kdl
singleton kdlNode = Kdl [ kdlNode ]

appendChildren ∷ Kdl → KdlNode → KdlNode
appendChildren childrenToAdd parent@{ children } = parent
  { children = children <> childrenToAdd }

appendChild ∷ KdlNode → KdlNode → KdlNode
appendChild child = appendChildren (Kdl [ child ])

class IsKdlValue a where
  toKdlValue ∷ a → KdlValue

instance IsKdlValue String where
  toKdlValue = KdlString

instance IsKdlValue Number where
  toKdlValue = KdlNumber

instance IsKdlValue Boolean where
  toKdlValue = KdlBoolean

instance IsKdlValue Unit where
  toKdlValue _ = KdlNull

appendProp ∷ ∀ value. IsKdlValue value ⇒ String → value → KdlNode → KdlNode
appendProp key value kdlNode@{ properties } = kdlNode
  { properties = properties # insert key (toKdlValue value) }

appendValue ∷ ∀ value. IsKdlValue value ⇒ value → KdlNode → KdlNode
appendValue value kdlNode@{ values } = kdlNode
  { values = values <> [ toKdlValue value ] }
