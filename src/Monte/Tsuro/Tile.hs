module Monte.Tsuro.Tile (
    allTiles,
    rotate,
    isRotationOf
) where

import Data.List
import Data.Tuple
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Monte.Tsuro.Types

-- | The 'rotate' function rotates the given tile clockwise.
rotate :: Tile -> Tile
rotate (Tile t) = Tile $ Vector.imap (\i v -> ((t Vector.! (i `addMod8` 6))) `addMod8` 2) t
    where addMod8 x y = (x + y) `mod` 8

-- | The 'isRotationOf' function returns true if the tiles are equal under rotation.
isRotationOf :: Tile -> Tile -> Bool
isRotationOf l r = l == r 
                || rotate l == r 
                || (rotate . rotate) l == r 
                || (rotate . rotate . rotate) l == r

{-| Contains all unique tiles under rotation.
    
    The tiles could be listed manually somewhat tediously, but it is easy enough to compute all 
    possible tile permutations and then combine ones rotationally unique to each other.
-}
allTiles :: Vector Tile
allTiles = Vector.fromList . nubBy isRotationOf $ makeTile <$> getPairings [0..7]

    where makeTile :: [(Int, Int)] -> Tile
          makeTile xs = let x1 = Vector.update (Vector.replicate 8 0) (Vector.fromList xs)
                            x2 = Vector.update x1 (swap <$> Vector.fromList xs)
                        in Tile x2
         
          -- Gets all pairings of elements
          -- e.g. getPairings [1..4] == [[(1,2), (3,4)], [(1,3), (2,4)], [(1,4), (2,3)]]
          getPairings :: [Int] -> [[(Int, Int)]]
          getPairings [] = [[]]
          getPairings (x:xs) = concat $ [((x,y):) <$> (getPairings $ delete y xs) | y <- xs]       

