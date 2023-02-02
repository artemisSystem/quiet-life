module QuietLife.Blocks where

import Prelude

import Data.Foldable (for_)
import QuietLife.Constants as Constants
import QuietLife.Templates (DefaultBlockRows {-, hollowLog-} )
import Run (Run)

newBlocks ∷ ∀ r. Run (DefaultBlockRows r) Unit
newBlocks = do
  -- for_ Constants.allLogsWithStripped hollowLog
  pure unit
