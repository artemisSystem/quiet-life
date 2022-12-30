module QuietLife.Tags where

import Prelude

import Data.Foldable (foldMap)
import Lib.Datagen.ResourceLocation ((:))
import Lib.Datagen.Tag (TagCollection, singleEntry)
import QuietLife.Templates (_item_tags)
import Run (Run)
import Run.Writer (Writer, tellAt)

newTags ∷ ∀ r. Run (item_tags ∷ Writer TagCollection | r) Unit
newTags = do
  tellAt _item_tags $
    foldMap (singleEntry ("kdlyextras" : "innate_silk_touch")) do
      tier ← [ "manasteel", "elementium" ]
      tool ← [ "axe", pickaxe tier, "hoe", "shears", "shovel", "sword" ]
      [ "botania" : (tier <> "_" <> tool) ]
  where
  pickaxe tier = if tier == "manasteel" then "pick" else "pickaxe"
