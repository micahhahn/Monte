{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Monte.Tsuro.Types
Description : Contains all core types used throughout the game engine.
Maintainer  : micah.s.hahn@gmail.com
-}
module Monte.Tsuro.Types where

import Data.Array
import Data.Vector (Vector)
import System.Random

-- | The 'Player' type represents a player's identity in the game.
data Player = White
            | Black
            | Green
            | Red
            | Blue
            | Gray
            | Yellow
            | Brown
    deriving (Eq, Ord, Enum, Show)

{-| The 'Tile' newtype represents a tile in a specific orientation.

    The positions on the tile wrap around clockwise starting from the top left.

@
 - 0 - 1 - 
 7       2
 6       3
 - 5 - 4 -
@

    The vector holds the starting position as the index and the ending position as the value.  While
    this is not the most space efficient representation (every path is specified twice), it allows
    for quick traversal via 'Vector' lookup.
-}
newtype Tile = Tile (Vector Int)
    deriving (Eq, Show)

-- | The 'BoardIx' newtype represents an index into slots on the game board.
newtype BoardIx = BoardIx (Int, Int)
    deriving (Show, Eq, Ord, Ix)

-- | The 'BoardPos' newtype represents a position in a slot on the game board. 
newtype BoardPos = BoardPos (BoardIx, Int)
    deriving (Show, Eq, Ord)

-- | The 'BoardState' type represents a board containing slots, played tiles, and players.
--   This is public information - known to all players
data BoardState = BoardState
    { tiles :: Array BoardIx (Maybe Tile)
    , players :: Vector (Player, BoardPos)
    } deriving (Show)

-- | The 'GameState' type represents the entire state of the game.  This is __not__ public
--   information.
data GameState = GameState
    { board :: BoardState                         -- ^ The board containing tiles and player positions.
    , drawTiles :: Vector Tile                    -- ^ The unordered collection of tiles in the draw pile.
    , playerHands :: Vector (Player, Vector Tile) -- ^ The contents of all player hands.
    , activePlayer :: Player                      -- ^ The player whose turn it is.
    , randomSeed :: StdGen                        -- ^ The seed used for random events in the game.
    } deriving (Show)

-- | The 'GameView' type represents the view of the game the active player would have.
--   TODO: The view should include a way to know the turn order
data GameView = GameView
    { board :: BoardState
    , hand :: Vector Tile
    } deriving (Show)