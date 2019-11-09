{-
   Bot Server API

   This is a bot API to let bots battle

   OpenAPI Version: 3.0.0
   Bot Server API API version: 1.0.0
   Contact: szederjesiarnold@gmail.com
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : BotServer.API.Connect
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module BotServer.API.Connect where

import BotServer.Core
import BotServer.MimeTypes
import BotServer.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** Connect

-- *** helloPost

-- | @POST \/hello@
-- 
-- Initiate connection to the game
-- 
helloPost 
  :: (Consumes HelloPost MimeJSON, MimeRender MimeJSON HelloRequest)
  => HelloRequest -- ^ "helloRequest"
  -> BotServerRequest HelloPost MimeJSON HelloResponse MimeJSON
helloPost helloRequest =
  _mkRequest "POST" ["/hello"]
    `setBodyParam` helloRequest

data HelloPost 
instance HasBodyParam HelloPost HelloRequest 

-- | @application/json@
instance Consumes HelloPost MimeJSON

-- | @application/json@
instance Produces HelloPost MimeJSON
