module Lib.Serializer where

import Prelude

import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (SemigroupMap)
import Data.Semigroup.First (First(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir', writeTextFile)
import Node.FS.Perms (all, mkPerms, read)
import Node.Path (FilePath, dirname)
import Node.Path as Path
import Prim.Row as Row
import Run (Run, AFF, liftAff)
import Run.Writer (Writer, runWriterAt)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

class IsDataType (dataType ∷ Type) where
  -- | Gets the file extension the data type should be written to, without the
  -- | `.` (For example `"json"`)
  getFileExtension ∷ Proxy dataType → String

instance IsDataType a ⇒ IsDataType (First a) where
  getFileExtension _ = getFileExtension (Proxy ∷ _ a)

class Serializable a where
  serialize ∷ a → String

instance Serializable a ⇒ Serializable (First a) where
  serialize (First a) = serialize a

writeData
  ∷ ∀ wr r name a dataType
  . IsDataType dataType
  ⇒ Serializable dataType
  ⇒ IsSymbol name
  ⇒ Row.Cons name (Writer (SemigroupMap String dataType)) (AFF + r) (AFF + wr)
  ⇒ Monoid (SemigroupMap String dataType)
  ⇒ Proxy name
  → FilePath
  → Run (AFF + wr) a
  → Run (AFF + r) a
writeData proxy destinationFolder m = do
  let extension = "." <> getFileExtension (Proxy ∷ _ dataType)
  Tuple files a ← runWriterAt proxy m
  a <$ forWithIndex_ files \path content → liftAff do
    let stringContent = serialize content
    mkdir $ dirname (Path.concat [ destinationFolder, path <> extension ])
    writeTextFile UTF8
      (Path.concat [ destinationFolder, path <> extension ])
      stringContent
  where
  mkdir folder = mkdir' folder { recursive: true, mode: mkPerms all read read }

writeDatum
  ∷ ∀ wr r name a dataType
  . IsDataType dataType
  ⇒ Serializable dataType
  ⇒ IsSymbol name
  ⇒ Row.Cons name (Writer dataType) (AFF + r) (AFF + wr)
  ⇒ Monoid dataType
  ⇒ Proxy name
  → FilePath
  → String
  → Run (AFF + wr) a
  → Run (AFF + r) a
writeDatum proxy destinationFolder path m = do
  Tuple content a ← runWriterAt proxy m
  let
    extension = "." <> getFileExtension (Proxy ∷ _ dataType)
    stringContent = serialize content
  liftAff do
    mkdir $ dirname (Path.concat [ destinationFolder, path <> extension ])
    writeTextFile UTF8
      (Path.concat [ destinationFolder, path <> extension ])
      stringContent
  pure a
  where
  mkdir folder = mkdir' folder { recursive: true, mode: mkPerms all read read }
