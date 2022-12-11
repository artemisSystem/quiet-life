module QuietLife.Models where

import Prelude

import Data.Foldable (fold)
import Foreign.Object (insert)
import Foreign.Object as Object
import Lib.Datagen.Blockstate (Blockstate(..), Rotation(..), VariantModels(..), aMultiVariant, rotatedMultiVariant, rotatedVariant, singleVariant)
import Lib.Datagen.Model (Model, hollowPillar)
import Lib.Serializer (ResourcePack, Serializer, serializer)
import QualifiedDo.Semigroup as S
import QualifiedDo.Unfoldable as U
import QuietLife.Constants as Constants

models ∷ Array (Serializer (ResourcePack "kdlycontent" "models") Model)
models = S.do
  []
  Constants.allLogs >>= \{ name, namespace, logSuffix } → U.unfold U.do
    serializer ("hollow_" <> name <> "_" <> logSuffix) $ hollowPillar
      "quiet_life:hollow_log"
      { end: fold [ namespace, ":", name, "_", logSuffix, "_top" ]
      , side: fold [ namespace, ":", name, "_", logSuffix ]
      , inside: fold [ namespace, ":", "stripped_", name, "_", logSuffix ]
      }

blockstates
  ∷ Array (Serializer (ResourcePack "minecraft" "blockstates") Blockstate)
blockstates = U.unfold U.do
  serializer "wall_torch" $ VariantBlockstate
    ( Object.empty
        # insert "facing=east" (singleVariant "block/wall_torch")
        # insert "facing=south" (rotatedVariant "block/wall_torch" R0 R90)
        # insert "facing=west" (rotatedVariant "block/wall_torch" R0 R180)
        # insert "facing=north" (rotatedVariant "block/wall_torch" R0 R270)
    )
  serializer "grass_block" $ VariantBlockstate
    ( Object.empty
        # insert "snowy=false"
            ( MultiVariant
                [ aMultiVariant "block/grass_block"
                , rotatedMultiVariant "block/grass_block" R0 R90
                , rotatedMultiVariant "block/grass_block" R0 R180
                , rotatedMultiVariant "block/grass_block" R0 R270
                ]
            )
        # insert "snowy=true" (singleVariant "block/grass_block_snow")
    )