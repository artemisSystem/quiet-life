module Lib.Datagen.KdlyContent.Block where

import Prelude

import Lib.Kdl (KdlNode, appendProp, appendValue, node)

block ∷ String → KdlNode
block blockName = node "block"
  # appendValue blockName

-- | For use with a `shape` node in a custom block
box ∷ Int → Int → Int → Int → Int → Int → KdlNode
box x0 y0 z0 x1 y1 z1 = node "box"
  # appendProp "minX" x0
  # appendProp "minY" y0
  # appendProp "minZ" z0
  # appendProp "maxX" x1
  # appendProp "maxY" y1
  # appendProp "maxZ" z1
