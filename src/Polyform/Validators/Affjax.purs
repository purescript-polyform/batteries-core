module Polyform.Validators.Affjax where

import Prelude

import Control.Monad.Error.Class (catchError)
import Data.Argonaut (Json, jsonParser)
import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Functor.Variant (SProxy(..))
import Data.Validation.Semigroup (invalid)
import Data.Variant (Variant, inj)
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Network.HTTP.Affjax (AffjaxRequest, AffjaxResponse)
import Network.HTTP.Affjax (affjax) as Affjax
import Network.HTTP.Affjax.Response (Response, json, string)
import Network.HTTP.StatusCode (StatusCode(..))
import Polyform.Validator (Validator, hoistFnMV, hoistFnV)
import Polyform.Validators.Json (JsError, object)

type HttpErrorRow (err :: # Type) = (wrongHttpStatus :: StatusCode | err)
type AffjaxErrorRow (err :: # Type) = (remoteError :: String | err)
type JsonErrorRow (err :: # Type) = (parsingError :: String | err)

affjax
  :: forall a err
   . (Response a)
  -> Validator
      Aff
      (Array (Variant (AffjaxErrorRow err)))
      AffjaxRequest
      (AffjaxResponse a)
affjax res = hoistFnMV $ \req → do
  (pure <$> Affjax.affjax res req) `catchError` handler where
    handler e = pure (invalid $ singleton $ (inj (SProxy :: SProxy "remoteError") $ show e))

status
  :: forall m err res
   . Monad m
  => (StatusCode -> Boolean)
  -> Validator m
      (Array (Variant (HttpErrorRow err)))
      (AffjaxResponse res)
      res
status isCorrect = hoistFnV checkStatus where
  checkStatus response =
    if isCorrect response.status then
      pure response.response
    else
      invalid $ singleton $ (inj (SProxy :: SProxy "wrongHttpStatus") response.status)

isStatusOK :: StatusCode -> Boolean
isStatusOK (StatusCode n) = (n == 200)

jsonFromRequest
  :: forall err
   . Validator
      Aff
      (Array (Variant(HttpErrorRow (AffjaxErrorRow (JsError err)))))
      AffjaxRequest
      (Object Json)
jsonFromRequest = object <<< status isStatusOK <<< affjax json

valJson
  :: forall m err
   . Monad m
  => Validator m
      (Array (Variant (JsonErrorRow err)))
      String
      Json
valJson = hoistFnV \response -> case jsonParser response of
  Right js -> pure js
  Left error -> invalid  $ singleton (inj (SProxy :: SProxy "parsingError") error)

affjaxJson
  :: forall errs
   . Validator
      Aff
      (Array (Variant (HttpErrorRow(AffjaxErrorRow (JsonErrorRow errs)))))
      AffjaxRequest
      Json
affjaxJson = valJson <<< status isStatusOK <<< affjax string
