{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module MonteAPI where

import GHC.Generics

import Lucid
import Servant
import Servant.HTML.Lucid
import Web.FormUrlEncoded (FromForm)

{- Eventually we should probably split out game specific code and just join it into an API here -}
data GameParameters = GameParameters
    { players :: Int
    } deriving (Eq, Generic)

instance FromForm GameParameters

type TsuroIndex = "tsuro" :> Get '[HTML] (Html ())
type TsuroNew = "tsuro" :> "new" :> Get '[HTML] (Html ())
type TsuroNewPost = "tsuro" :> "new" :> ReqBody '[FormUrlEncoded] GameParameters :> Post '[HTML] (Html ())

type MonteApi = TsuroIndex
           :<|> TsuroNew
           :<|> TsuroNewPost
           :<|> "static" :> Raw