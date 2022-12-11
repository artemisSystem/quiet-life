module Lib.Datagen.KdlyContent.Block where

import Prelude

import Lib.Kdl (KdlNode, appendValue, node)

block ∷ String → KdlNode
block blockName = node "block"
  # appendValue blockName
