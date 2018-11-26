 {-# LANGUAGE OverloadedStrings #-}
 
 module Monte.Tsuro.Board (
     Player(..),
     Game(..),
     playTile,
     testBoard
 ) where

import Data.Array
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Text (Text)
import System.Random

import Monte.Tsuro.Tile

newtype BoardIx = BoardIx (Int, Int)
    deriving (Show, Eq, Ord, Ix)

newtype BoardPos = BoardPos { unBoardPos :: (BoardIx, Int) }
    deriving (Show)

data Player = Player
    { position :: BoardPos
    , tiles :: Vector Tile
    } deriving (Show)

data Game = Game
    { board :: Array BoardIx (Maybe Tile)
    , players :: Vector Player
    , drawTiles :: Vector Tile
    , seed :: StdGen
    } deriving (Show)

mapPath :: BoardIx -> Int -> Maybe BoardPos
mapPath (BoardIx (x, y)) p
    | p >= 0 && p <= 1 = mkPos (y == 0) (x, y - 1)
    | p >= 2 && p <= 3 = mkPos (x == 7) (x + 1, y)
    | p >= 4 && p <= 5 = mkPos (y == 7) (x, y + 1)
    | p >= 6 && p <= 7 = mkPos (x == 0) (x - 1, y)
    where mkPos b ix = if b then Nothing else Just $ BoardPos ((BoardIx ix), (mapping Vector.! p))
          mapping = Vector.fromList [5, 4, 7, 6, 1, 0, 3, 2]
    
{- Returns a the new board position or nothing if the player is forced off the board -}
followPaths :: Array BoardIx (Maybe Tile) -> BoardPos -> Maybe BoardPos
followPaths b p = let (BoardPos (ix, pos)) = p
                  in case b ! ix of
                        Nothing -> Just p    
                        Just (Tile paths) -> case mapPath ix (paths Vector.! pos) of
                            Nothing -> Nothing
                            Just newPos -> followPaths b newPos

{- playTile :: Game 
         -> Int          {- Player number -}
         -> Tile         {- Tile to be played -}
         -> Either Text a -}
playTile game playerIx tile = do
    let player = players game Vector.! playerIx
    if any (isRotationOf tile) (tiles player) then pure () else Left ("This tile is not available to be played" :: Text)
    let newBoard = (board game) // [(fst . unBoardPos . position $ player, Just tile)]
    let newPos = followPaths newBoard (position player)
    Right newPos

blankBoard = array (BoardIx (0, 0), BoardIx (7, 7)) [(BoardIx (x, y), Nothing) | x <- [0..7], y <- [0..7]]

testBoard = blankBoard // (Vector.toList $ Vector.imap (\i t -> (BoardIx (i `quot` 8, i `mod` 8), Just t)) (Vector.fromList allTiles))

testGame = Game { board = blankBoard 
                , players = Vector.fromList [ Player (BoardPos (BoardIx (0, 0), 0)) (Vector.fromList (take 3 allTiles))
                                            , Player (BoardPos (BoardIx (7, 7), 4)) (Vector.fromList (take 3 . drop 3 $ allTiles)) 
                                            ]
                , drawTiles = Vector.fromList . drop 6 $ allTiles
                , seed = mkStdGen 0
                }