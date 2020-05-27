module Polyform.Json.Validators
  ( JNull
  , Errors
  , Segment(..)
  , Validator
  , _arrayExpected
  , _booleanExpected
  , _fieldMissing
  , _intExpected
  , _nullExpected
  , _numberExpected
  , _objectExpected
  , array
  , arrayOf
  , boolean
  , consErrorsPath
  , hoistErrors
  , index
  , int
  , field
  , number
  , object
  , optionalField
  , string
  )
  where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (Json)
import Data.Argonaut (isNull, jsonNull, toArray, toBoolean, toNumber, toObject, toString) as Argonaut
import Data.Array (index, singleton) as Array
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromNumber) as Int
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Choice ((|||))
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Validation.Semigroup (V)
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Foreign.Object (Object)
import Foreign.Object (lookup) as Object
import Polyform.Validator (Validator, hoistFn, hoistFnMaybe) as Validator
import Polyform.Validator (hoistFnMV, hoistFnMaybe, lmapValidator, runValidator)
import Polyform.Validators (Errors, Validator) as Validators
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol)
import Unsafe.Coerce (unsafeCoerce)

-- | This error representation is for sure "object field / array element" biased.
-- | But it works fine in the most real live scenarios.
-- | For the top level values we just use `Nil` as a path (by using `hoistErrors`).
-- |
-- | To simplify extensibility the error tree is flattened into
-- | list of error paths. Representig recursive types like tree with the
-- | `Variant` would possibly force usage of `VariantF`. I think
-- | we want to avoid this API complication.
data Segment = Key String | Index Int
derive instance eqSegment ∷ Eq Segment
derive instance genericSegment ∷ Generic Segment _
instance showSegment ∷ Show Segment where
  show s = genericShow s

type Errors errs = Array { path ∷ List Segment, errors ∷ Array (Variant errs) }

type Validator m errs i o = Validator.Validator m (Errors errs) i o

-- | Lifts validators which represents error as `Array (Variant errs)`
-- | into json validator by wrapping failures in empty path information.
-- |
-- | For example if you want to use some general purpose `String` validators
-- | in the context of json validation you can easily lift their error
-- | representation:
-- |
-- | ```
-- | nonBlankString ∷ ∀ e m. Monad m ⇒ Validator m (stringExpected ∷ Json, nonBlankExpected ∷ Unit | e) Json String
-- | nonBlankString = fromValidator String.nonBlank <<< string
-- | ```
fromValidator ∷ ∀ errs m i o. Monad m ⇒ Validators.Validator m errs i o → Validator m errs i o
fromValidator = lmapValidator hoistErrors

error ∷ ∀ errs e l r.  Row.Cons l e r errs ⇒ IsSymbol l ⇒ SProxy l → e → Errors errs
error label
  = hoistErrors
  <<< Array.singleton
  <<< Variant.inj label

hoistErrors ∷ ∀ errs. Validators.Errors errs → Errors errs
hoistErrors = Array.singleton <<< { path: Nil, errors: _ }

consErrorsPath ∷ ∀ e. Segment → Errors e → Errors e
consErrorsPath segment = map step
  where
    step { path, errors } = { path: segment : path, errors }

field_
  ∷ ∀ a errs m
  . Monad m
  ⇒ String
  → Validator m errs (Maybe Json) a
  → Validator m errs (Object Json) a
field_ name fv = Validator.hoistFn (Object.lookup name) >>> lmapValidator (consErrorsPath (Key name)) fv

_fieldMissing = SProxy ∷ SProxy "fieldMissing"

-- | These two validators starts from `Object Json` and not just `Json` so
-- | you should compose them with `object` validator like:
-- |
-- | ```purescript
-- | object >>> ({ x: _, y: _ } <$> field "x" int <*> field "y" int)
-- | ```
-- |
-- | We don't provide this composition by default because these could
-- | lead easily to inefficient usage which would validate object multiple
-- | times like:
-- |
-- | { x: _, y: _ } <$> (object >>> field "x" int) <*> (object >>> field "y" int)
field
  ∷ ∀ a errs m
  . Monad m
  ⇒ String
  → Validator m (fieldMissing ∷ Unit | errs)  Json a
  → Validator m (fieldMissing ∷ Unit | errs)  (Object Json) a
field name fv = field_ name (hoistFnMaybe (const $ error _fieldMissing unit) identity >>> fv)

optionalField
  ∷ ∀ a errs m
  . Monad m
  ⇒ String
  → Validator m errs Json a
  → Validator m errs (Object Json) (Maybe a)
optionalField name fv = field_ name v
  where
    v = lcmap (note unit) (pure Nothing ||| Just <$> fv)

_arrayExpected = SProxy ∷ SProxy "arrayExpected"

array ∷ ∀ e m. Monad m ⇒ Validator m (arrayExpected ∷ Json | e) Json (Array Json)
array = Validator.hoistFnMaybe (error _arrayExpected) Argonaut.toArray

_indexMissing = SProxy ∷ SProxy "indexMissing"

arrayOf
  ∷ ∀ m e a
  . Monad m
  ⇒ Validator m (arrayExpected ∷ Json | e) Json a
  → Validator m (arrayExpected ∷ Json | e) Json (Array a)
arrayOf v = array >>> hoistFnMV av
  where
    ep idx = consErrorsPath (Index idx)

    -- | Run every validator by prefixing its error path with index.
    -- | Validator results in `m (V e a)` so we need traverse here.
    f ∷ Array Json → m (Array (V (Errors (arrayExpected ∷ Json | e )) a ))
    f = traverseWithIndex \idx → runValidator (lmapValidator (ep idx) v)

    av ∷ Array Json → m (V (Errors (arrayExpected ∷ Json | e )) (Array a))
    av = f >>> map sequence

index
  ∷ ∀ errs m
  . Monad m
  ⇒ Int
  → Validator.Validator m (Errors (indexMissing ∷ Unit | errs)) (Array Json) Json
index idx = hoistFnMaybe err (flip Array.index idx)
  where
    err _ = consErrorsPath (Index idx) (error _indexMissing unit)

nullable
  ∷ ∀ a errs m
  . Monad m
  ⇒ Validator m (nullExpected ∷ Json | errs) Json a
  → Validator m (nullExpected ∷ Json | errs) Json (Maybe a)
nullable fv = (null *> pure Nothing) <|> (Just <$> fv)

_intExpected = SProxy ∷ SProxy "intExpected"

int ∷ ∀ e m. Monad m ⇒ Validator m (intExpected ∷ Json | e) Json Int
int = Validator.hoistFnMaybe (error _intExpected) (Argonaut.toNumber >=> Int.fromNumber)

_booleanExpected = SProxy ∷ SProxy "booleanExpected"

boolean ∷ ∀ e m. Monad m ⇒ Validator m (booleanExpected ∷ Json | e) Json Boolean
boolean = Validator.hoistFnMaybe (error _booleanExpected) Argonaut.toBoolean

_stringExpected = SProxy ∷ SProxy "stringExpected"

string ∷ ∀ e m. Monad m ⇒ Validator m (stringExpected ∷ Json | e) Json String
string = Validator.hoistFnMaybe (error _stringExpected) Argonaut.toString

_numberExpected = SProxy ∷ SProxy "numberExpected"

number ∷ ∀ e m. Monad m ⇒ Validator m (numberExpected ∷ Json | e) Json Number
number = Validator.hoistFnMaybe (error _numberExpected) Argonaut.toNumber

_objectExpected = SProxy ∷ SProxy "objectExpected"

object ∷ ∀ e m. Monad m ⇒ Validator m (objectExpected ∷ Json | e) Json (Object Json)
object = Validator.hoistFnMaybe (error _objectExpected) Argonaut.toObject

-- | Because argonaut is not providing this type for us any more we define
-- | it here so we can provide a result from `null` validator.
-- | value as `jsonNull`.
foreign import data JNull ∷ Type

instance eqJNull ∷ Eq JNull where
  eq _ _ = true

instance ordJNull ∷ Ord JNull where
  compare _ _ = EQ

jnull ∷ JNull
jnull = unsafeCoerce Argonaut.jsonNull

_nullExpected = SProxy ∷ SProxy "nullExpected"

null ∷ ∀ e m. Monad m ⇒ Validator m (nullExpected ∷ Json | e) Json JNull
null = Validator.hoistFnMaybe (error _nullExpected) (\v → if Argonaut.isNull v then Just jnull else Nothing)

