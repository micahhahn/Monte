{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Monte.MonteAPI where

import GHC.Generics

import Lucid
import Servant
import Servant.HTML.Lucid
import Web.FormUrlEncoded (FromForm)

type TsuroIndex = "tsuro" :> Get '[HTML] (Html ())
type TsuroNew = "tsuro" :> "new" :> Get '[HTML] (Html ())
type TsuroNewPost = "tsuro" :> "new" :> ReqBody '[FormUrlEncoded] GameParameters :> Post '[HTML] (Html ())

type TsuroApi = TsuroIndex
           :<|> TsuroNew
           :<|> TsuroNewPost

type MonteApi = TsuroApi
           :<|> "static" :> Raw