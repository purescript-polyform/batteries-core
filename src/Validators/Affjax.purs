module Validators.Affjax where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Error.Class (catchError)
import Data.Argonaut (Json)
import Data.Array (singleton)
import Data.Functor.Variant (SProxy(..))
import Data.StrMap (StrMap)
import Data.Variant (Variant, inj)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse)
import Network.HTTP.Affjax (affjax) as Affjax
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.StatusCode (StatusCode(..))
import Polyform.Validation (V(Invalid, Valid), Validation, hoistFnMV, hoistFnV)
import Validators.Json (JsError, object)


type HttpErrorRow (err :: # Type) = (wrongHttpStatus :: StatusCode | err)
type AffjaxErrorRow (err :: # Type) = (remoteError :: String | err)




affjax :: forall req res ext err.
  Requestable req => Respondable res => Validation
                                          (Aff ( ajax :: AJAX | ext))
                                          (Array (Variant (AffjaxErrorRow err)))
                                          (AffjaxRequest req)
                                          (AffjaxResponse res)
affjax = hoistFnMV $ \req → do
    (Valid [] <$> Affjax.affjax req) `catchError` (\e -> pure (Invalid $ singleton $ (inj (SProxy :: SProxy "remoteError") $ show e)))


status :: forall m err res.
  Monad m => (StatusCode -> Boolean) ->Validation m
                (Array (Variant (HttpErrorRow err)))
                (AffjaxResponse res)
                res
                
status isCorrect = hoistFnV checkStatus where
  checkStatus response =
      if isCorrect response.status then
        Valid [] response.response
      else
        Invalid $ singleton $ (inj (SProxy :: SProxy "wrongHttpStatus") response.status)
        

isStatusOK :: StatusCode -> Boolean
isStatusOK (StatusCode n) = (n==200)


jsonFromRequest :: forall ext req err.
  Requestable req => Validation
                       (Aff ( ajax :: AJAX | ext))
                       (Array (Variant(HttpErrorRow (AffjaxErrorRow (JsError err)))))
                       (AffjaxRequest req)
                       (StrMap Json)

jsonFromRequest = object <<< status isStatusOK <<< affjax