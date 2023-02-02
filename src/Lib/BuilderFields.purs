module Lib.BuilderFields where

import Prelude

import Data.Functor.Variant (VariantF, expand, inj)
import Data.Functor.Variant as VariantF
import Data.Lazy (Lazy)
import Data.Symbol (class IsSymbol)
import Lib.Overridable (OverridablesRL, default, foldOverridables, override)
import Lib.ResourceLocation (ResourceLocation)
import Prim.Row as R
import Prim.RowList (RowList)
import Prim.RowList as RL
import Run.Writer (Writer(..))
import Type.Proxy (Proxy(..))

class FromBuilderFields
  ∷ RowList (Type → Type)
  → Row (Type → Type)
  → RowList (Type → Type)
  → Row (Type → Type)
  → Constraint
class
  FromBuilderFields rlFrom rowFrom rlTo rowTo
  | rlFrom → rowFrom
  , rlTo → rowTo
  where
  fromBuilderFields
    ∷ Proxy rlFrom → Proxy rlTo → VariantF rowFrom ~> VariantF rowTo

instance fromBuilderFieldsNil ∷ FromBuilderFields RL.Nil () RL.Nil () where
  fromBuilderFields _ _ empty = empty

instance fromBuilderFieldsMatchCons ∷
  ( IsSymbol key
  , R.Cons key (Writer (OverridablesRL m)) tailFrom rowFrom
  , R.Cons key (Writer m) tailTo rowTo
  , R.Union tailTo x rowTo
  , Monoid m
  , R.Lacks key tailFrom
  , R.Lacks key tailTo
  , FromBuilderFields rlTailFrom tailFrom rlTailTo tailTo
  ) ⇒
  FromBuilderFields
    (RL.Cons key (Writer (OverridablesRL m)) rlTailFrom)
    rowFrom
    (RL.Cons key (Writer m) rlTailTo)
    rowTo
  where
  fromBuilderFields _ _ =
    VariantF.on (Proxy ∷ _ key) (f >>> inj (Proxy ∷ _ key))
      (fromBuilderFields (Proxy ∷ _ rlTailFrom) (Proxy ∷ _ rlTailTo) >>> expand)
    where
    f ∷ Writer (OverridablesRL m) ~> Writer m
    f (Writer w a) = Writer (foldOverridables w) a

else instance fromBuilderFieldsMissCons ∷
  ( IsSymbol key
  , R.Cons key f tailFrom rowFrom
  , R.Cons key f tailTo rowTo
  , R.Union tailTo x rowTo
  , R.Lacks key tailFrom
  , R.Lacks key tailTo
  , Functor f
  , FromBuilderFields rlTailFrom tailFrom rlTailTo tailTo
  ) ⇒
  FromBuilderFields
    (RL.Cons key f rlTailFrom)
    rowFrom
    (RL.Cons key f rlTailTo)
    rowTo
  where
  fromBuilderFields _ _ = VariantF.on (Proxy ∷ _ key) (inj (Proxy ∷ _ key))
    (fromBuilderFields (Proxy ∷ _ rlTailFrom) (Proxy ∷ _ rlTailTo) >>> expand)

class ToBuilderFields
  ∷ RowList (Type → Type)
  → Row (Type → Type)
  → RowList (Type → Type)
  → Row (Type → Type)
  → Constraint
class
  ToBuilderFields rlFrom rowFrom rlTo rowTo
  | rlFrom → rowFrom
  , rlTo → rowTo
  where
  toBuilderFields
    ∷ Proxy rlFrom
    → Proxy rlTo
    → ResourceLocation
    → VariantF rowFrom ~> VariantF rowTo

instance toBuilderFieldsNil ∷ ToBuilderFields RL.Nil () RL.Nil () where
  toBuilderFields _ _ _ empty = empty

instance toBuilderFieldsMatchConsDefault ∷
  ( IsSymbol key
  , R.Cons key (Writer (Lazy m)) tailFrom rowFrom
  , R.Cons key (Writer (OverridablesRL m)) tailTo rowTo
  , R.Union tailTo x rowTo
  , Monoid m
  , R.Lacks key tailFrom
  , R.Lacks key tailTo
  , ToBuilderFields rlTailFrom tailFrom rlTailTo tailTo
  ) ⇒
  ToBuilderFields
    (RL.Cons key (Writer (Lazy m)) rlTailFrom)
    rowFrom
    (RL.Cons key (Writer (OverridablesRL m)) rlTailTo)
    rowTo
  where
  toBuilderFields _ _ rl =
    VariantF.on (Proxy ∷ _ key) (f >>> inj (Proxy ∷ _ key))
      ( toBuilderFields (Proxy ∷ _ rlTailFrom) (Proxy ∷ _ rlTailTo) rl >>>
          expand
      )
    where
    f ∷ Writer (Lazy m) ~> Writer (OverridablesRL m)
    f (Writer w a) = Writer (default rl w) a

else instance toBuilderFieldsMatchConsOverride ∷
  ( IsSymbol key
  , R.Cons key (Writer m) tailFrom rowFrom
  , R.Cons key (Writer (OverridablesRL m)) tailTo rowTo
  , R.Union tailTo x rowTo
  , Monoid m
  , R.Lacks key tailFrom
  , R.Lacks key tailTo
  , ToBuilderFields rlTailFrom tailFrom rlTailTo tailTo
  ) ⇒
  ToBuilderFields
    (RL.Cons key (Writer m) rlTailFrom)
    rowFrom
    (RL.Cons key (Writer (OverridablesRL m)) rlTailTo)
    rowTo
  where
  toBuilderFields _ _ rl =
    VariantF.on (Proxy ∷ _ key) (f >>> inj (Proxy ∷ _ key)) $
      toBuilderFields (Proxy ∷ _ rlTailFrom) (Proxy ∷ _ rlTailTo) rl >>> expand
    where
    f ∷ Writer m ~> Writer (OverridablesRL m)
    f (Writer w a) = Writer (override rl w) a

else instance toBuilderFieldsMissCons ∷
  ( IsSymbol key
  , R.Cons key f tailFrom rowFrom
  , R.Cons key f tailTo rowTo
  , R.Union tailTo x rowTo
  , R.Lacks key tailFrom
  , R.Lacks key tailTo
  , Functor f
  , ToBuilderFields rlTailFrom tailFrom rlTailTo tailTo
  ) ⇒
  ToBuilderFields
    (RL.Cons key f rlTailFrom)
    rowFrom
    (RL.Cons key f rlTailTo)
    rowTo
  where
  toBuilderFields _ _ rl = VariantF.on (Proxy ∷ _ key) (inj (Proxy ∷ _ key))
    (toBuilderFields (Proxy ∷ _ rlTailFrom) (Proxy ∷ _ rlTailTo) rl >>> expand)
