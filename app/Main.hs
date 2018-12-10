{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

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
import Servant.Links
import Web.FormUrlEncoded (FromForm)
import GHC.Generics

import Monte.MonteAPI
import Monte.Tsuro.Svg
import Monte.Tsuro.Tile
import Monte.Tsuro.Board
import Monte.Tsuro.Types
import Monte.LucidExtensions

import Paths_Monte

scale :: Double -> SvgPrim -> SvgPrim
scale s p = (\(SvgPoint x y) -> SvgPoint (x * s) (y * s)) <$> p

-- | Render tile to a series of svg paths 
-- Only draw where starting index < ending index
renderTile :: Tile -> Vector SvgPrim
renderTile (Tile paths) = scale 100 . renderPath <$> liftedPaths
    where liftedPaths :: Vector (Int, Int)
          liftedPaths = Vector.filter (\(p1, p2) -> p1 < p2) $ Vector.imap (\i v -> (i, v)) paths

          renderPath :: (Int, Int) -> SvgPrim
          renderPath (p1, p2)
              | p1 > 1 = rotateR $ renderPath ((p1 + 6) `mod` 8, (p2 + 6) `mod` 8)
              | p1 == 1 = flipH $ renderPath (flipH' p1, flipH' p2)
              | otherwise = render0To p2

          flipH' :: Int -> Int
          flipH' i = Vector.fromList [1, 0, 7, 6, 5, 4, 3, 2] Vector.! i

          render0To :: Int -> SvgPrim
          render0To x = case x of
              1 -> SvgBeizer4 (SvgPoint _1 0) (SvgPoint _1 0.22) (SvgPoint _2 0.22) (SvgPoint _2 0)
              2 -> SvgBeizer4 (SvgPoint _1 0) (SvgPoint _1 _1) (SvgPoint _2 _1) (SvgPoint 1 _1)
              3 -> SvgBeizer4 (SvgPoint _1 0) (SvgPoint _1 _1) (SvgPoint _2 _2) (SvgPoint 1 _2)
              4 -> SvgBeizer4 (SvgPoint _1 0) (SvgPoint _1 _2) (SvgPoint _2 _2) (SvgPoint _2 1)
              5 -> SvgLine (SvgPoint _1 0) (SvgPoint _1 1)
              6 -> SvgBeizer4 (SvgPoint _1 0) (SvgPoint _1 _1) (SvgPoint _1 _2) (SvgPoint 0 _2)
              7 -> SvgBeizer3 (SvgPoint _1 0) (SvgPoint _1 _1) (SvgPoint 0 _1)
              where _1 = 1/3
                    _2 = 2/3    

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

renderGameView :: GameView -> Html ()
renderGameView (GameView board hand) = renderBoard board

renderBoard :: BoardState -> Html ()
renderBoard (BoardState ts ps) = table_ [class_ "board"] $ do
    sequence_ [renderRow y | y <- [top..bottom]]
    -- sequence_ $ (\p -> div_ [style_ ("color: " <> playerColor (player p))] (toHtml . show $ p)) <$> players

    where (BoardIx (left, top), BoardIx (right, bottom)) = bounds ts

          renderRow :: Int -> Html ()
          renderRow y = tr_ [class_ "row"] $ do
              sequence_ [renderTd (x, y) | x <- [left..right]]

          renderTd :: (Int, Int) -> Html ()
          renderTd i = case ts ! (BoardIx i) of
              Just t -> td_ [class_ "tile"] (makeSvg $ renderTile t)
              Nothing -> td_ [class_ "blank"] ""

data TsuroHeader a = TsuroHeader

tsuroHeader :: Html () -> Html ()
tsuroHeader content = do
    doctypehtml_ $ do
        html_ $ do
            head_ $ do
                title_ "Tsuro"
                link_ [type_ "text/css", rel_ "stylesheet", href_ "static/tsuro.css"]
            body_ $ do
                content

tsuro :: Html ()
tsuro = do
    doctypehtml_ $ do
        html_ $ do
            head_ $ do
                title_ "Tsuro"
                link_ [type_ "text/css", rel_ "stylesheet", href_ "static/tsuro.css"]
            body_ $ do
                p_ "This. Is. Tsuro."
                -- renderGameView 

tsuroNew :: Html ()
tsuroNew = tsuroHeader $ do
    h2_ "New Game"
    form_ [method_ "POST", action_ ("/" <> (toUrlPiece $ safeLink (Proxy :: Proxy MonteApi) (Proxy :: Proxy TsuroNewPost)))] $ do
        label_ [] "Players: "
        input_ [type_ "text", name_ "players", value_ "2"]
        input_ [type_ "submit"]
    
tsuroNewPost :: GameParameters -> Handler (Html ())
tsuroNewPost (GameParameters g) = return . tsuroHeader $ do
    p_ "New Game Created"
    p_ (toHtml (show g))

server :: FilePath -> Server MonteApi
server staticPath = return tsuro
               :<|> return tsuroNew
               :<|> tsuroNewPost
               :<|> serveDirectoryFileServer staticPath

main :: IO ()
main = do
    resource <- getDataFileName "src/static/"
    run 8080 (serve (Proxy :: Proxy MonteApi) (server resource))