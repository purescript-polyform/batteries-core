module Polyform.Batteries.Validators.Affjax where

import Prelude
import Affjax (Error(..), Response, request, Request) as Affjax
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Functor.Variant (SProxy(..))
import Data.Semigroup.First (First) as Semigroup
import Data.Validation.Semigroup (V, invalid)
import Data.Variant (Variant, inj)
import Effect.Aff (Aff)
import Global.Unsafe (unsafeStringify)
import Polyform.Validator (Validator) as Polyform
import Polyform.Validator (liftFn, liftFnMV, liftFnV)
import Type.Row (type (+))

type HttpError (err :: # Type)
  = ( wrongHttpStatus :: StatusCode | err )

type AffjaxError (err :: # Type)
  = ( remoteError :: String | err )

type ResponseFormatError (err :: # Type)
  = ( responseFormatError :: String | err )

valid :: forall e a. Semigroup e => a -> V e a
valid = pure

type Error err
  = Semigroup.First (Variant (AffjaxError + ResponseFormatError + err))

affjax ::
  forall a err.
  Polyform.Validator
    Aff
    (Error err)
    (Affjax.Request a)
    (Affjax.Response a)
affjax =
  liftFnMV
    $ \req -> do
        (handleResponse <$> Affjax.request req)
  where
  handleResponse = case _ of
    Left (Affjax.RequestContentError s) → invalid $ pure $ (inj (SProxy :: SProxy "responseFormatError") s)
    Left (Affjax.ResponseBodyError err response) → invalid $ pure $ (inj (SProxy :: SProxy "responseFormatError") (unsafeStringify err <> unsafeStringify response))
    Left (Affjax.XHRError e) → invalid $ pure $ (inj (SProxy :: SProxy "remoteError") $ show e)
    Right response -> valid $ response

status ::
  forall m err res.
  Monad m =>
  (StatusCode -> Boolean) ->
  Polyform.Validator m
    (Error (HttpError + err))
    (Affjax.Response res)
    (Affjax.Response res)
status isCorrect = liftFnV checkStatus
  where
  checkStatus response =
    if isCorrect response.status then
      pure response
    else
      invalid $ pure $ (inj (SProxy :: SProxy "wrongHttpStatus") response.status)

body ::
  forall m err res.
  Monad m =>
  Polyform.Validator m
    (Error (HttpError + err))
    (Affjax.Response res)
    res
body = liftFn _.body

isStatusOK :: StatusCode -> Boolean
isStatusOK (StatusCode n) = (n == 200)

affjaxJson ::
  forall errs.
  Polyform.Validator
    Aff
    (Error (HttpError + AffjaxError + ResponseFormatError + errs))
    (Affjax.Request Json)
    Json
affjaxJson = body <<< status isStatusOK <<< affjax
