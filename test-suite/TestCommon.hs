{-# LANGUAGE UnicodeSyntax #-}

module TestCommon (
  xml
, json
, def
, parseURI
) where

import           Language.Haskell.TH.Quote
import           Text.RawString.QQ
import           Network.URI (parseURI)
import           Data.Aeson.QQ
import           Data.Default

-- renames for vim
xml, json ∷ QuasiQuoter
xml = r
json = aesonQQ
