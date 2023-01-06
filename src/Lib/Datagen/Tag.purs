module Lib.Datagen.Tag where

import Prelude hiding ((/))

import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (SemigroupMap)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Foreign.ReadWrite (class WriteForeign, writeForeign)
import Lib.Json (unsafeFormatJson)
import Lib.ResourceLocation (ResourceLocation, getRLAsPath, toStr)
import Lib.Serializer (class IsDataType, class Serializable, writeFile)
import Lib.Util (sMapSingleton, (/))
import Node.Path (FilePath)
import Node.Path as Path
import Prim.Row as Row
import Run (AFF, Run, liftAff)
import Run.Writer (Writer, runWriterAt)
import Type.Proxy (Proxy)
import Type.Row (type (+))

data TagEntry = SingleEntry ResourceLocation | TagEntry ResourceLocation

instance WriteForeign TagEntry where
  writeForeign (SingleEntry rl) = writeForeign (toStr rl)
  writeForeign (TagEntry rl) = writeForeign ("#" <> toStr rl)

newtype Tag = Tag (Array TagEntry)

derive newtype instance Semigroup Tag
derive newtype instance Monoid Tag

instance IsDataType Tag where
  getFileExtension _ = "json"

instance WriteForeign Tag where
  writeForeign (Tag values) = writeForeign { replace: false, values }

instance Serializable Tag where
  serialize tag = unsafeFormatJson (writeForeign tag)

singletonTagEntry ∷ TagEntry → Tag
singletonTagEntry entry = Tag [ entry ]

newtype TagCollection = TagCollection (SemigroupMap ResourceLocation Tag)

derive newtype instance Semigroup TagCollection
derive newtype instance Monoid TagCollection

singletonTagCollection ∷ ResourceLocation → Tag → TagCollection
singletonTagCollection rl tag = TagCollection (sMapSingleton rl tag)

singleEntry ∷ ResourceLocation → ResourceLocation → TagCollection
singleEntry rl item = singletonTagCollection rl
  (singletonTagEntry $ SingleEntry item)

tagEntry ∷ ResourceLocation → ResourceLocation → TagCollection
tagEntry rl tag = singletonTagCollection rl
  (singletonTagEntry $ TagEntry tag)

writeTags
  ∷ ∀ wr r name a
  . IsSymbol name
  ⇒ Row.Cons name (Writer TagCollection) (AFF + r) (AFF + wr)
  ⇒ Proxy name
  → FilePath
  → String
  → Run (AFF + wr) a
  → Run (AFF + r) a
writeTags proxy destinationFolder tagType m = do
  Tuple (TagCollection files) a ← runWriterAt proxy m
  a <$ forWithIndex_ files \rl content → liftAff do
    writeFile
      (Path.concat [ destinationFolder, getRLAsPath ("tags" / tagType) rl ])
      content
