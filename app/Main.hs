{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad
import Data.Monoid ((<>))
import Data.Proxy
import Data.Array
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
import Monte.Tsuro.Board
import Monte.LucidExtensions

type MonteApi = "tsuro" :> Get '[HTML] (Html ())
           :<|> "static" :> Raw
 
makeSvg :: Vector SvgPrim -> Html ()
makeSvg ps = svg_ [viewbox_ "0 0 100 100", width_ "100px", height_ "100px"] $ do
                path_ [d_ d, fill_ "transparent", stroke_ "black"]
    where d = Text.intercalate " " . Vector.toList $ getPath <$> ps

data Page a = Page a

playerColor :: Player -> Text
playerColor White = "#FFFFFF"
playerColor Black = "#000000"
playerColor Green = "#7B6638"
playerColor Red = "#7F0208"
playerColor Blue = "#354767"
playerColor Gray = "#6D4A4E"
playerColor Yellow = "#EDC863"
playerColor Brown = "#9C4D05"

renderGame :: Game -> Html ()
renderGame (Game board players _ _) = table_ [class_ "board"] $ do
    sequence_ [renderRow y | y <- [top..bottom]]
    sequence_ $ (\p -> div_ [style_ ("color: " <> playerColor (player p))] (toHtml . show $ p)) <$> players

    where (BoardIx (left, top), BoardIx (right, bottom)) = bounds board

          renderRow :: Int -> Html ()
          renderRow y = tr_ [class_ "row"] $ do
              sequence_ [renderTd (x, y) | x <- [left..right]]

          renderTd :: (Int, Int) -> Html ()
          renderTd i = case board ! (BoardIx i) of
              Just t -> td_ [class_ "tile"] (makeSvg $ renderTile t)
              Nothing -> td_ [class_ "blank"] ""

tsuro :: Html ()
tsuro = do
    doctypehtml_ $ do
        html_ $ do
            head_ $ do
                title_ "Tsuro"
                link_ [type_ "text/css", rel_ "stylesheet", href_ "static/tsuro.css"]
            body_ $ do
                p_ "This. Is. Tsuro."
                renderGame testGame

server :: Server MonteApi
server = return tsuro
    :<|> serveDirectoryFileServer "C:\\Users\\micah\\Source\\Monte\\src\\Static"

main :: IO ()
main = run 8080 (serve (Proxy :: Proxy MonteApi) server)
