{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Monte.Tsuro.Svg (
    SvgPoint(..),
    SvgPrim(..),
    SvgPrim'(..),

    rotateR,
    flipH,
    getPath
) where 

import Data.Text (Text)
import qualified Data.Text as Text

data SvgPrim' a = SvgLine { start :: a, end :: a }
                | SvgBeizer3 { start :: a, control :: a, end :: a }
                | SvgBeizer4 { start :: a, control1 :: a, control2 :: a, end :: a }
                deriving (Functor)

data SvgPoint = SvgPoint Double Double
    deriving (Eq, Ord, Show)

type SvgPrim = SvgPrim' SvgPoint

rotateR :: SvgPrim -> SvgPrim
rotateR = fmap (\(SvgPoint x y) -> SvgPoint (1 - y) x)

flipH :: SvgPrim -> SvgPrim
flipH = fmap (\(SvgPoint x y) -> SvgPoint (1 - x) y)

getPath :: SvgPrim -> Text
getPath x = case x of 
    (SvgLine s e) -> "M " <> getPoint s <> " L " <> getPoint e
    (SvgBeizer3 s c e) -> "M " <> getPoint s <> " Q " <> getPoint c <> ", " <> getPoint e
    (SvgBeizer4 s c1 c2 e) -> "M " <> getPoint s <> " C " <> getPoint c1 <> ", " <> getPoint c2 <> ", " <> getPoint e
    where getPoint (SvgPoint x y) = (Text.pack . show $ x) <> " " <> (Text.pack . show $ y)