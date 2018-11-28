{-# LANGUAGE OverloadedStrings #-}

module Monte.LucidExtensions where

import Lucid.Base
import Data.Text

path_ :: Applicative m => [Attribute] -> HtmlT m ()
path_ = with (makeElementNoEnd "path")

viewbox_ :: Text -> Attribute
viewbox_ = makeAttribute "viewbox"

d_ :: Text -> Attribute
d_ = makeAttribute "d"

fill_ :: Text -> Attribute
fill_ = makeAttribute "fill"

stroke_ :: Text -> Attribute
stroke_ = makeAttribute "stroke"