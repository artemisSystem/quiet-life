module Lib.MapRun where

import Prelude

import Data.Functor.Variant (VariantF, expand, inj)
import Data.Functor.Variant as VariantF
import Data.Symbol (class IsSymbol)
import Prim.Row as R
import Prim.RowList (RowList)
import Prim.RowList as RL
import Run (Run)
import Run as Run
import Type.Proxy (Proxy(..))

class MapVariantF
  ∷ RowList (Type → Type)
  → Row (Type → Type)
  → RowList (Type → Type)
  → Row (Type → Type)
  → (Type → Type)
  → (Type → Type)
  → Constraint
class
  MapVariantF rlFrom rowFrom rlTo rowTo from to
  | rlFrom → rowFrom
  , rlTo → rowTo
  where
  mapVariantF
    ∷ Proxy rlFrom
    → Proxy rlTo
    → (from ~> to)
    → VariantF rowFrom ~> VariantF rowTo

instance mapVariantNil ∷ MapVariantF RL.Nil () RL.Nil () from to where
  mapVariantF _ _ _f variant = variant

instance mapVariantMatchCons ∷
  ( IsSymbol key
  , R.Cons key from tailFrom rowFrom
  , R.Cons key to tailTo rowTo
  , R.Union tailTo x rowTo
  , R.Lacks key tailFrom
  , R.Lacks key tailTo
  , Functor to
  , MapVariantF rlTailFrom tailFrom rlTailTo tailTo from to
  ) ⇒
  MapVariantF
    (RL.Cons key from rlTailFrom)
    rowFrom
    (RL.Cons key to rlTailTo)
    rowTo
    from
    to
  where
  mapVariantF _ _ f = VariantF.on (Proxy ∷ _ key) (f >>> inj (Proxy ∷ _ key))
    (mapVariantF (Proxy ∷ _ rlTailFrom) (Proxy ∷ _ rlTailTo) f >>> expand)

else instance mapVariantMissCons ∷
  ( IsSymbol key
  , R.Cons key f tailFrom rowFrom
  , R.Cons key f tailTo rowTo
  , R.Union tailTo x rowTo
  , R.Lacks key tailFrom
  , R.Lacks key tailTo
  , Functor f
  , MapVariantF rlTailFrom tailFrom rlTailTo tailTo from to
  ) ⇒
  MapVariantF
    (RL.Cons key from rlTailFrom)
    rowFrom
    (RL.Cons key to rlTailTo)
    rowTo
    from
    to
  where
  mapVariantF _ _ f = VariantF.on (Proxy ∷ _ key) (inj (Proxy ∷ _ key))
    (mapVariantF (Proxy ∷ _ rlTailFrom) (Proxy ∷ _ rlTailTo) f >>> expand)

-- | Transform every field of a certain functor in a `Run` computation.
-- | For example, `mapRun (f :: Either e ~> Maybe)` applied to
-- | `Run (a :: Either e, b :: Either e, c :: Array) a` would be
-- | `Run (a :: Maybe, b :: Maybe, c :: Array) a`.
mapRun
  ∷ ∀ rowFrom rlFrom rowTo rlTo from to
  . RL.RowToList rowFrom rlFrom
  ⇒ RL.RowToList rowTo rlTo
  ⇒ MapVariantF rlFrom rowFrom rlTo rowTo from to
  ⇒ (from ~> to)
  → Run rowFrom ~> Run rowTo
mapRun f = Run.interpret
  (mapVariantF (Proxy ∷ _ rlFrom) (Proxy ∷ _ rlTo) f >>> Run.send)
