{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UnicodeSyntax #-}

module TestCommon (
  xml
, json
, def
, parseURI
, parseURIReference
) where

import           Language.Haskell.TH.Quote
import           Text.RawString.QQ (r)
import           Network.URI (parseURI, parseURIReference, URI)
import           Data.Aeson.QQ (aesonQQ)
import           Data.Default (def)
import           Data.String (IsString, fromString)
import           Data.Maybe (fromJust)

instance IsString URI where
    fromString = fromJust . parseURIReference

-- renames for vim
xml, json ∷ QuasiQuoter
xml = r
json = aesonQQ
