{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Monte.Tsuro.TsuroApi where

import GHC.Generics

import Servant

import Monte.Tsuro.Types

data GameParameters = GameParameters
    { players :: Int
    } deriving (Eq, Generic)

data GameMove = PlayInitial BoardPos
              | PlayTile Tile
              deriving (Eq, Generic)

type TsuroNew = "tsuro" :> "new" :> ReqBody '[JSON] GameParameters :> Post '[JSON] GameView
type TsuroPlay = "tsuro" :> "play" :> ReqBody '[JSON] GameMove :> Post '[JSON] GameView

type TsuroApi = TsuroNew
           :<|> TsuroPlay