{-# LANGUAGE OverloadedStrings #-}

module Monte.Tsuro.Tile (
    allTiles,
    rotate,
    isRotationOf,
    renderTile
) where

import Data.List
import Data.Tuple
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))

import Monte.Tsuro.Svg
import Monte.Tsuro.Types

{- rotates the tile clockwise -}
rotate :: Tile -> Tile
rotate (Tile t) = Tile $ Vector.imap (\i v -> ((t Vector.! (i `addMod8` 6))) `addMod8` 2) t
    where addMod8 x y = (x + y) `mod` 8

{- Returns true if the tiles are equal under rotation -}
isRotationOf :: Tile -> Tile -> Bool
isRotationOf l r = l == r 
                || rotate l == r 
                || (rotate . rotate) l == r 
                || (rotate . rotate . rotate) l == r

gen :: [Int] -> [[(Int, Int)]]
gen [] = [[]]
gen (x:xs) = concat $ [((x,y):) <$> (gen $ delete y xs) | y <- xs]

makePaths :: [(Int, Int)] -> Tile
makePaths xs = let x1 = Vector.update (Vector.replicate 8 0) (Vector.fromList xs)
                   x2 = Vector.update x1 (swap <$> Vector.fromList xs)
               in Tile x2

unorderedGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
unorderedGroupBy _ [] = []
unorderedGroupBy eq (x:xs) = let (l, r) = partition (eq x) xs
                             in (x : l) : unorderedGroupBy eq r

allTiles :: [Tile]
allTiles = let groups = unorderedGroupBy isRotationOf $ makePaths <$> gen [0..7]
           in head <$> groups

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

