module Polyform.Validators.Affjax where

import Prelude

import Affjax (Response, ResponseFormatError, request, Request) as Affjax
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (catchError)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Functor.Variant (SProxy(..))
import Data.Semigroup.First (First) as Semigroup
import Data.Validation.Semigroup (V, invalid)
import Data.Variant (Variant, inj)
import Effect.Aff (Aff)
import Polyform.Validator (Validator) as Polyform
import Polyform.Validator (hoistFn, hoistFnMV, hoistFnV)
import Record (set)
import Type.Row (type (+))

type HttpError (err :: # Type) = (wrongHttpStatus :: StatusCode | err)
type AffjaxError (err :: # Type) = (remoteError :: String | err)
type ResponseFormatError (err :: # Type) =
  (responseFormatError :: Affjax.Response Affjax.ResponseFormatError | err)

valid :: forall e a. Semigroup e => a -> V e a
valid = pure

type Error err = Semigroup.First (Variant (AffjaxError + ResponseFormatError + err))

affjax
  :: forall a err
   . Polyform.Validator
      Aff
      (Error err)
      (Affjax.Request a)
      (Affjax.Response a)
affjax = hoistFnMV $ \req -> do
  (handleResponse <$> Affjax.request req) `catchError` handleError
  where
    handleResponse response = case response.body of
      Left err ->
        let
          response' = set (SProxy :: SProxy "body") err response
        in
          invalid $ pure $ (inj (SProxy :: SProxy "responseFormatError") response')
      Right a ->
        valid $ set (SProxy :: SProxy "body") a response
    handleError e = pure (invalid $ pure $ (inj (SProxy :: SProxy "remoteError") $ show e))

status
  :: forall m err res
   . Monad m
  => (StatusCode -> Boolean)
  -> Polyform.Validator m
      (Error (HttpError + err))
      (Affjax.Response res)
      (Affjax.Response res)
status isCorrect = hoistFnV checkStatus
  where
    checkStatus response =
      if isCorrect response.status then
        pure response
      else
        invalid $ pure $ (inj (SProxy :: SProxy "wrongHttpStatus") response.status)

body
  :: forall m err res
   . Monad m
  => Polyform.Validator m
      (Error (HttpError + err))
      (Affjax.Response res)
      res
body = hoistFn _.body

isStatusOK :: StatusCode -> Boolean
isStatusOK (StatusCode n) = (n == 200)

affjaxJson
  :: forall errs
   . Polyform.Validator
      Aff
      (Error (HttpError + AffjaxError + ResponseFormatError + errs))
      (Affjax.Request Json)
      Json
affjaxJson = body <<< status isStatusOK <<< affjax
