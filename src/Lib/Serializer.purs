module Lib.Serializer where

import Prelude

import Data.FoldableWithIndex (forWithIndex_)
import Data.Semigroup.First (First(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Lib.OnlyOne (UniqueRLMap, getOneOrFileError)
import Lib.ResourceLocation (getRLAsPath)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir', writeTextFile)
import Node.FS.Perms (all, mkPerms, read)
import Node.Path (FilePath, dirname)
import Node.Path as Path
import Prim.Row as Row
import Run (AFF, Run, liftAff)
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
  ⇒ Row.Cons name (Writer (UniqueRLMap dataType)) (AFF + r) (AFF + wr)
  ⇒ Proxy name
  → FilePath
  → FilePath
  → Run (AFF + wr) a
  → Run (AFF + r) a
writeData proxy dataFolder subfolder m = do
  Tuple files a ← runWriterAt proxy m
  a <$ forWithIndex_ files \rl content → liftAff do
    let filePath = (Path.concat [ dataFolder, getRLAsPath subfolder rl ])
    getOneOrFileError filePath content >>= writeFile filePath

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
  a <$ liftAff do writeFile (Path.concat [ destinationFolder, path ]) content

writeFile
  ∷ ∀ dataType
  . IsDataType dataType
  ⇒ Serializable dataType
  ⇒ FilePath
  → dataType
  → Aff Unit
writeFile path content = do
  mkdir (dirname file)
  writeTextFile UTF8 file (serialize content)
  where
  extension = "." <> getFileExtension (Proxy ∷ _ dataType)
  file = path <> extension
  mkdir folder = mkdir' folder { recursive: true, mode: mkPerms all read read }
