module Lib.Serializer where

import Prelude

import Data.Foldable (for_)
import Data.Newtype (class Newtype)
import Data.Reflectable (class Reflectable, reflectType)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir', writeTextFile)
import Node.FS.Perms (all, mkPerms, read)
import Node.Path (FilePath)
import Node.Path as Path
import Type.Proxy (Proxy(..))

data PackType

foreign import data DataPack ∷ Symbol → Symbol → PackType
foreign import data ResourcePack ∷ Symbol → Symbol → PackType
foreign import data KdlFile ∷ PackType

class IsPackType (packType ∷ PackType) where
  getDataLocation ∷ Proxy packType → String

instance
  ( Reflectable namespace String
  , Reflectable subdir String
  ) ⇒
  IsPackType (DataPack namespace subdir) where
  getDataLocation _ = Path.concat [ "data", namespace, subdir ]
    where
    namespace = reflectType (Proxy ∷ _ namespace)
    subdir = reflectType (Proxy ∷ _ subdir)

instance
  ( Reflectable namespace String
  , Reflectable subdir String
  ) ⇒
  IsPackType (ResourcePack namespace subdir) where
  getDataLocation _ = Path.concat [ "assets", namespace, subdir ]
    where
    namespace = reflectType (Proxy ∷ _ namespace)
    subdir = reflectType (Proxy ∷ _ subdir)

instance IsPackType KdlFile where
  getDataLocation _ = "content"

class IsDataType (dataType ∷ Type) where
  -- | Gets the file extension the data type should be written to, without the
  -- | `.` (For example `"json"`)
  getFileExtension ∷ Proxy dataType → String

newtype Serializer
  (packType ∷ PackType)
  (dataType ∷ Type) =
  Serializer { path ∷ FilePath, content ∷ dataType }

derive instance Newtype (Serializer dataType packType) _

-- | Construct a `Serializer` value
serializer
  ∷ ∀ packType dataType
  . FilePath
  → dataType
  → Serializer packType dataType
serializer path content = Serializer { path, content }

class Serializable a where
  serialize ∷ a → String

writeData
  ∷ ∀ packType dataType
  . IsPackType packType
  ⇒ IsDataType dataType
  ⇒ Serializable dataType
  ⇒ FilePath
  → Array (Serializer packType dataType)
  → Aff Unit
writeData packRoot serializers = do
  let
    dataLocation = getDataLocation (Proxy ∷ _ packType)
    extension = "." <> getFileExtension (Proxy ∷ _ dataType)
    destinationFolder = Path.concat [ packRoot, dataLocation ]
  mkdir' destinationFolder { recursive: true, mode: mkPerms all read read }
  for_ serializers \(Serializer { path, content }) → do
    let stringContent = serialize content
    writeTextFile UTF8 (Path.concat [ destinationFolder, path <> extension ])
      stringContent

writeDatum
  ∷ ∀ packType dataType
  . IsPackType packType
  ⇒ IsDataType dataType
  ⇒ Serializable dataType
  ⇒ FilePath
  → Serializer packType dataType
  → Aff Unit
writeDatum path theSerializer = writeData path [ theSerializer ]
