module Lib.Deserializer where

import Prelude

import Control.Monad.Trans.Class (lift)
import Debug (traceM)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Foreign (FT)
import Foreign.ReadWrite (class ReadForeign, readForeign)
import Lib.Json (parseJson)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

readData ∷ ∀ a m. MonadAff m ⇒ ReadForeign a ⇒ String → FT m a
readData path = do
  content ← lift $ liftAff $ readTextFile UTF8 path
  -- traceM content
  json ← lift $ liftEffect $ parseJson content
  readForeign json
