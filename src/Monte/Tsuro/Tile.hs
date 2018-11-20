{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Monte.Tsuro.Tile (
    Tile(..),
    allTiles,
    rotateTile,
    tileEq
) where

import Data.List
import Data.Tuple
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))

data Tile = Tile 
    { paths :: Vector Int
    , rotations :: Int 
    } deriving (Show, Eq)

tileEq :: Tile -> Tile -> Bool
tileEq (Tile l _) (Tile r _) = isEquivalent l r

{- rotates the tile clockwise -}
rotate :: Vector Int -> Vector Int
rotate t = Vector.imap (\i v -> ((t Vector.! (i `addMod8` 6))) `addMod8` 2) t
    where addMod8 x y = (x + y) `mod` 8

rotateTile :: Tile -> Tile
rotateTile (Tile ps r) = Tile (rotate ps) r

gen :: [Int] -> [[(Int, Int)]]
gen [] = [[]]
gen (x:xs) = concat $ [((x,y):) <$> (gen $ delete y xs) | y <- xs]

makePaths :: [(Int, Int)] -> Vector Int
makePaths xs = let x1 = Vector.update (Vector.replicate 8 0) (Vector.fromList xs)
                   x2 = Vector.update x1 (swap <$> Vector.fromList xs)
               in x2

isEquivalent :: Vector Int -> Vector Int -> Bool
isEquivalent l r = l == r 
                || rotate l == r 
                || (rotate . rotate) l == r 
                || (rotate . rotate . rotate) l == r

unorderedGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
unorderedGroupBy _ [] = []
unorderedGroupBy eq (x:xs) = let (l, r) = partition (eq x) xs
                             in (x : l) : unorderedGroupBy eq r

allTiles :: [Tile]
allTiles = let groups = unorderedGroupBy isEquivalent $ makePaths <$> gen [0..7]
           in (\g -> Tile (head g) (length g)) <$> groups

data SvgPrim' a = SvgLine { start :: a, end :: a }
                | SvgBeizer3 { start :: a, control :: a, end :: a }
                | SvgBeizer4 { start :: a, control1 :: a, control2 :: a, end :: a }
                deriving (Functor)

newtype SvgPoint = SvgPoint { unSvgPoing :: (Double, Double) }

type SvgPrim = SvgPrim' SvgPoint

primToSvgString :: SvgPrim -> Text
primToSvgString x = case x of 
    (SvgLine s e) -> "M " <> getPoint s <> " L " <> getPoint e
    (SvgBeizer3 s c e) -> "M " <> getPoint s <> " Q " <> getPoint c <> ", " <> getPoint e
    (SvgBeizer4 s c1 c2 e) -> "M " <> getPoint s <> " C " <> getPoint c1 <> ", " <> getPoint c2 <> ", " <> getPoint e
    
    where getPoint (SvgPoint (x, y)) = (Text.pack . show $ x) <> " " <> (Text.pack . show $ y)

makeHtml :: [SvgPrim] -> Text
makeHtml x = "<svg height=\"100\" width=\"100\"><path d=\"" <> path <> "\" fill=\"transparent\" stroke=\"black\" /></svg>"
    where path = Text.concat $ intersperse " " $ primToSvgString <$> x

scale :: Double -> SvgPrim -> SvgPrim
scale s p = (\(SvgPoint (x, y)) -> SvgPoint (x * s, y * s)) <$> p

{- Only draw where starting index < ending index -}
{- renderTile :: Tile -> [SvgPrim] -}
renderTile (Tile paths _) = scale 100 . renderPath <$> liftedPaths
    where liftedPaths :: Vector (Int, Int)
          liftedPaths = Vector.filter (\(p1, p2) -> p1 < p2) $ Vector.imap (\i v -> (i, v)) paths

          renderPath :: (Int, Int) -> SvgPrim
          renderPath (p1, p2)
              | p1 > 1 = fmap rotateRight $ renderPath ((p1 + 6) `mod` 8, (p2 + 6) `mod` 8)
              | p1 == 1 = fmap flipHorizontal $ renderPath (flipH p1, flipH p2)
              | otherwise = render0To p2

          flipH :: Int -> Int
          flipH i = [1, 0, 7, 6, 5, 4, 3, 2] !! i

          render0To :: Int -> SvgPrim
          render0To 1 = SvgBeizer4 (SvgPoint (1/3, 0)) (SvgPoint (1/3, 0.22)) (SvgPoint (2/3, 0.22)) (SvgPoint (2/3, 0))
          render0To 2 = SvgBeizer4 (SvgPoint (1/3, 0)) (SvgPoint (1/3, 1/3)) (SvgPoint (2/3, 1/3)) (SvgPoint (1, 1/3))
          render0To 3 = SvgBeizer3 (SvgPoint (1/3, 0)) (SvgPoint (1/3, 2/3)) (SvgPoint (1, 2/3))
          render0To 4 = SvgBeizer4 (SvgPoint (1/3, 0)) (SvgPoint (1/3, 2/3)) (SvgPoint (2/3, 1/3)) (SvgPoint (2/3, 1))
          render0To 5 = SvgLine (SvgPoint (1/3, 0)) (SvgPoint (1/3, 1))
          render0To 6 = SvgBeizer4 (SvgPoint (1/3, 0)) (SvgPoint (1/3, 1/3)) (SvgPoint (1/3, 2/3)) (SvgPoint (0, 2/3))
          render0To 7 = SvgBeizer3 (SvgPoint (1/3, 0)) (SvgPoint (1/3, 1/3)) (SvgPoint (0, 1/3))

          rotateRight :: SvgPoint -> SvgPoint
          rotateRight (SvgPoint (x, y)) = SvgPoint (1 - y, x)

          flipHorizontal :: SvgPoint -> SvgPoint
          flipHorizontal (SvgPoint (x, y)) = SvgPoint (1 - x, y)

