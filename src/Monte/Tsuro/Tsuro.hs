module Monte.Tsuro.Tsuro (
    gameTiles
) where

import Data.List
import Data.Tuple
import Data.Vector (Vector)
import qualified Data.Vector as Vector

newtype Tile = Tile { unTile :: Vector Int }
    deriving (Show, Eq)

{- rotates the tile clockwise -}
rotate :: Tile -> Tile
rotate (Tile t) = Tile $ Vector.imap (\i v -> ((t Vector.! (i `addMod8` 6))) `addMod8` 2) t
    where addMod8 x y = (x + y) `mod` 8

gen :: [Int] -> [[(Int, Int)]]
gen [] = [[]]
gen (x:xs) = concat $ [((x,y):) <$> (gen $ delete y xs) | y <- xs]

makeTiles :: [(Int, Int)] -> Tile
makeTiles xs = let x1 = Vector.update (Vector.replicate 8 0) (Vector.fromList xs)
                   x2 = Vector.update x1 (swap <$> Vector.fromList xs)
               in Tile x2

isEquivalent :: Tile -> Tile -> Bool
isEquivalent l r = l == r 
                || rotate l == r 
                || (rotate . rotate) l == r 
                || (rotate . rotate . rotate) l == r

unorderedGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
unorderedGroupBy _ [] = []
unorderedGroupBy eq (x:xs) = let (l, r) = partition (eq x) xs
                             in (x : l) : unorderedGroupBy eq r

gameTiles :: [[Tile]]
gameTiles = unorderedGroupBy isEquivalent $ makeTiles <$> gen [0..7]