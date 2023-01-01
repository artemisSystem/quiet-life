module Lib.Datagen.Worldgen.DensityFunction where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), Replacement(..), contains, replace)
import Foreign (FT, Foreign, ForeignError(..), fail)
import Foreign.Object (Object, delete, lookup)
import Foreign.ReadWrite (class ReadForeign, class WriteForeign, readForeign, writeForeign)
import Lib.Json (unsafeFormatJson)
import Lib.Serializer (class IsDataType, class Serializable)
import Record (disjointUnion)

alt ∷ ∀ m a. Monad m ⇒ FT m a → FT m a → FT m a
alt (ExceptT m) (ExceptT n) = ExceptT do
  rm ← m
  case rm of
    Right x → pure (Right x)
    Left _ → do
      rn ← n
      case rn of
        Right x → pure (Right x)
        Left err → pure (Left (err))

infixr 3 alt as <|>

-- Temporary until we have a proper noise codec
type Noise = String

type ObjectSplineDefinition =
  { coordinate ∷ DensityFunction
  , points ∷
      NonEmptyArray
        { location ∷ Number
        , value ∷ Spline
        , derivative ∷ Number
        }
  }

data Spline
  = ConstantSpline Number
  | ObjectSpline ObjectSplineDefinition

derive instance Generic Spline _

instance Show Spline where
  show s = genericShow s

instance WriteForeign Spline where
  writeForeign (ConstantSpline num) = writeForeign num
  writeForeign (ObjectSpline object) = writeForeign object

instance ReadForeign Spline where
  readForeign json = ConstantSpline <$> (readForeign json ∷ _ Number)
    <|> ObjectSpline <$> (readForeign json ∷ _ ObjectSplineDefinition)

data DensityFunction = Direct DirectDensityFunction | Reference String

derive instance Generic DensityFunction _

instance Show DensityFunction where
  show = genericShow

instance WriteForeign DensityFunction where
  writeForeign (Direct direct) = writeForeign direct
  writeForeign (Reference location) = writeForeign location

instance ReadForeign DensityFunction where
  readForeign json = Reference <$> (readForeign json ∷ _ String)
    <|> Direct <$> (readForeign json ∷ _ DirectDensityFunction)

type OldBlendedNoiseDefinition =
  { xz_scale ∷ Number
  , y_scale ∷ Number
  , xz_factor ∷ Number
  , y_factor ∷ Number
  , smear_scale_multiplier ∷ Number
  }

type NoiseDefinition = { noise ∷ Noise, xz_scale ∷ Number, y_scale ∷ Number }

type WeirdScaledSamplerDefinition =
  { rarity_value_mapper ∷ String, noise ∷ Noise, input ∷ DensityFunction }

type ShiftedNoiseDefinition =
  { noise ∷ Noise
  , xz_scale ∷ Number
  , y_scale ∷ Number
  , shift_x ∷ DensityFunction
  , shift_y ∷ DensityFunction
  , shift_z ∷ DensityFunction
  }

type RangeChoiceDefinition =
  { input ∷ DensityFunction
  , min_inclusive ∷ Number
  , max_exclusive ∷ Number
  , when_in_range ∷ DensityFunction
  , when_out_of_range ∷ DensityFunction
  }

type ClampDefinition =
  { input ∷ DirectDensityFunction, min ∷ Number, max ∷ Number }

type YClampedGradientDefinition =
  { from_y ∷ Int, to_y ∷ Int, from_value ∷ Number, to_value ∷ Number }

data DirectDensityFunction
  = Constant Number

  | Interpolated DensityFunction
  | FlatCache DensityFunction
  | Cache2D DensityFunction
  | CacheOnce DensityFunction
  -- Not including "cache_all_in_cell". Wiki says not for datapack use

  | Abs DensityFunction
  | Square DensityFunction
  | Cube DensityFunction
  | HalfNegative DensityFunction
  | QuarterNegative DensityFunction
  | Squeeze DensityFunction

  | Add DensityFunction DensityFunction
  | Mul DensityFunction DensityFunction
  | Min DensityFunction DensityFunction
  | Max DensityFunction DensityFunction

  | BlendAlpha
  | BlendOffset
  | BlendDensity DensityFunction
  | OldBlendedNoise OldBlendedNoiseDefinition

  -- Not including "beardifier". Wiki says not for datapack use
  | Noise NoiseDefinition
  | EndIslands
  | WeirdScaledSampler WeirdScaledSamplerDefinition
  | ShiftedNoise ShiftedNoiseDefinition
  | RangeChoice RangeChoiceDefinition
  | ShiftA Noise
  | ShiftB Noise
  | Shift Noise
  | Clamp ClampDefinition
  | Spline Spline
  | YClampedGradient YClampedGradientDefinition

instance IsDataType DirectDensityFunction where
  getFileExtension _ = "json"

instance Serializable DirectDensityFunction where
  serialize densityFunction = unsafeFormatJson (writeForeign densityFunction)

derive instance Generic DirectDensityFunction _

instance Show DirectDensityFunction where
  show d = genericShow d

instance WriteForeign DirectDensityFunction where
  writeForeign = case _ of
    Constant argument → write { type: "constant", argument }

    Interpolated argument → write { type: "interpolated", argument }
    FlatCache argument → write { type: "flat_cache", argument }
    Cache2D argument → write { type: "cache_2d", argument }
    CacheOnce argument → write { type: "cache_once", argument }

    Abs argument → write { type: "abs", argument }
    Square argument → write { type: "square", argument }
    Cube argument → write { type: "cube", argument }
    HalfNegative argument → write { type: "half_negative", argument }
    QuarterNegative argument → write
      { type: "quarter_negative", argument }
    Squeeze argument → write { type: "squeeze", argument }

    Add argument1 argument2 → write { type: "add", argument1, argument2 }
    Mul argument1 argument2 → write { type: "mul", argument1, argument2 }
    Min argument1 argument2 → write { type: "min", argument1, argument2 }
    Max argument1 argument2 → write { type: "max", argument1, argument2 }

    BlendAlpha → write { type: "blend_alpha" }
    BlendOffset → write { type: "blend_offset" }
    BlendDensity argument → write { type: "blend_density", argument }
    OldBlendedNoise object →
      write (disjointUnion { type: "old_blended_noise" } object)

    Noise object → write (disjointUnion { type: "noise" } object)
    EndIslands → write { type: "end_islands" }
    WeirdScaledSampler object → write
      (disjointUnion { type: "weird_scaled_sampler" } object)
    ShiftedNoise object → write
      (disjointUnion { type: "shifted_noise" } object)
    RangeChoice object → write
      (disjointUnion { type: "range_choice" } object)
    ShiftA argument → write { type: "shift_a", argument }
    ShiftB argument → write { type: "shift_b", argument }
    Shift argument → write { type: "shift", argument }
    Clamp object → write (disjointUnion { type: "clamp" } object)
    Spline spline → write { type: "spline", spline }
    YClampedGradient object → write
      (disjointUnion { type: "y_clamped_gradient" } object)
    where
    write
      ∷ ∀ r
      . WriteForeign { type ∷ String | r }
      ⇒ { type ∷ String | r }
      → Foreign
    write obj = writeForeign obj
      { type =
          if contains (Pattern ":") obj.type then obj.type
          else "minecraft:" <> obj.type
      }

type Arg a = { argument ∷ a }

arg ∷ ∀ a b. (a → b) → Arg a → b
arg f object = f object.argument

type Arg2 a = { argument1 ∷ a, argument2 ∷ a }

arg2 ∷ ∀ a b. (a → a → b) → Arg2 a → b
arg2 f object = f object.argument1 object.argument2

instance ReadForeign DirectDensityFunction where
  readForeign ∷ ∀ m. Monad m ⇒ Foreign → FT m DirectDensityFunction
  readForeign json = Constant <$> (readForeign json ∷ _ Number) <|> do
    objectWithType ← readForeign json ∷ _ (Object Foreign)
    dfType ← case lookup "type" objectWithType of
      Just str → (readForeign str ∷ _ String)
        <#> replace (Pattern "minecraft:") (Replacement "")
      Nothing → fail (TypeMismatch "String" "undefined")
    let
      object = writeForeign $ delete "type" objectWithType

      -- Short name to fit more things on one line
      o ∷ ∀ r. ReadForeign r ⇒ FT m r
      o = readForeign object
    case dfType of
      "constant" → (arg Constant) <$> (o ∷ _ (Arg Number))
      "interpolated" → (arg Interpolated) <$> (o ∷ _ (Arg DensityFunction))
      "flat_cache" → (arg FlatCache) <$> (o ∷ _ (Arg DensityFunction))
      "cache_2d" → (arg Cache2D) <$> (o ∷ _ (Arg DensityFunction))
      "cache_once" → (arg CacheOnce) <$> (o ∷ _ (Arg DensityFunction))

      "abs" → (arg Abs) <$> (o ∷ _ (Arg DensityFunction))
      "square" → (arg Square) <$> (o ∷ _ (Arg DensityFunction))
      "cube" → (arg Cube) <$> (o ∷ _ (Arg DensityFunction))
      "half_negative" → (arg HalfNegative) <$> (o ∷ _ (Arg DensityFunction))
      "quarter_negative" →
        (arg QuarterNegative) <$> (o ∷ _ (Arg DensityFunction))
      "squeeze" → (arg Squeeze) <$> (o ∷ _ (Arg DensityFunction))

      "add" → (arg2 Add) <$> (o ∷ _ (Arg2 DensityFunction))
      "mul" → (arg2 Mul) <$> (o ∷ _ (Arg2 DensityFunction))
      "min" → (arg2 Min) <$> (o ∷ _ (Arg2 DensityFunction))
      "max" → (arg2 Max) <$> (o ∷ _ (Arg2 DensityFunction))

      "blend_alpha" → BlendAlpha <$ (o ∷ _ {})
      "blend_offset" → BlendOffset <$ (o ∷ _ {})
      "blend_density" → (arg BlendDensity) <$> (o ∷ _ (Arg DensityFunction))
      "old_blended_noise" →
        OldBlendedNoise <$> (o ∷ _ OldBlendedNoiseDefinition)

      "noise" → Noise <$> (o ∷ _ NoiseDefinition)
      "endIslands" → EndIslands <$ (o ∷ _ {})
      "weird_scaled_sampler" →
        WeirdScaledSampler <$> (o ∷ _ WeirdScaledSamplerDefinition)
      "shifted_noise" → ShiftedNoise <$> (o ∷ _ ShiftedNoiseDefinition)
      "range_choice" → RangeChoice <$> (o ∷ _ RangeChoiceDefinition)
      "shift_a" → (arg ShiftA) <$> (o ∷ _ (Arg Noise))
      "shift_b" → (arg ShiftB) <$> (o ∷ _ (Arg Noise))
      "shift" → (arg Shift) <$> (o ∷ _ (Arg Noise))
      "clamp" → Clamp <$> (o ∷ _ ClampDefinition)
      "spline" → Spline <$> (o ∷ _ Spline)
      "y_clamped_gradient" →
        YClampedGradient <$> (o ∷ _ YClampedGradientDefinition)

      _ → fail (ForeignError $ "Unknown density function type: " <> dfType)
