{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Lucid
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid

import Monte.Tsuro.Svg
import Monte.Tsuro.Tile
import Monte.LucidExtensions

type MonteApi = "tsuro" :> Get '[HTML] (Html ())
           :<|> "static" :> Raw
 
makeSvg :: Vector SvgPrim -> Html ()
makeSvg ps = svg_ [height_ "100", width_ "100"] $ do
                path_ [d_ d, fill_ "transparent", stroke_ "black"]
    where d = Text.intercalate " " . Vector.toList $ getPath <$> ps

tsuro :: Html ()
tsuro = do
    doctypehtml_ $ do
        html_ $ do
            head_ $ do
                title_ "Tsuro"
                link_ [type_ "text/css", rel_ "stylesheet", href_ "static/tsuro.css"]
            body_ $ do
                p_ "This. Is. Tsuro."
                sequence_ $ (makeSvg . renderTile) <$> allTiles 

server :: Server MonteApi
server = return tsuro
    :<|> serveDirectoryFileServer "C:\\Users\\micah\\Source\\Monte\\src\\Static"

main :: IO ()
main = run 8080 (serve (Proxy :: Proxy MonteApi) server)
