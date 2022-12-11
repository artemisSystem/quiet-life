module QuietLife.Blocks where

import Prelude

import Lib.Datagen.KdlyContent.Block (block)
import Lib.Kdl (Kdl(..), KdlNode, appendChild, appendChildren, appendProp, appendValue, node, singleton)
import Lib.Serializer (KdlFile, Serializer, serializer)
import QualifiedDo.Unfoldable as U

blocks ∷ Serializer KdlFile Kdl
blocks = serializer "kdlycontent" $ singleton myBlock
  where
  myBlock ∷ KdlNode
  myBlock = block "test_block" # (appendChildren <<< Kdl)
    ( U.unfold U.do
        node "type"
          # appendValue "custom"
          # appendChild (node "pistonBehavior" # appendValue "destroy")
        node "settings"
          # appendProp "material" "wood"
          # appendChild (node "hardness" # appendValue 0.2)
        node "item"
    )