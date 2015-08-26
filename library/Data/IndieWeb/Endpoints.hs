{-# LANGUAGE OverloadedStrings, UnicodeSyntax, CPP #-}

module Data.IndieWeb.Endpoints where

#if __GLASGOW_HASKELL__ < 709
import           Control.Applicative
#endif
import           Control.Lens
import           Data.Maybe (catMaybes)
import           Data.List (nub)
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.Text as T
import qualified Data.Vector as V
import           Network.URI
import           Network.HTTP.Link

discoverEndpoints ∷ [T.Text]-- ^ the rels of the links you want to find (alternatives, like "webmention" and "http://webmention.org/")
                  → Value -- ^ the full Microformats 2 parse of the page as extracted by 'Data.Microformats2.Parser.parseMf2'
                  → [Link] -- ^ the Link header as parsed by 'Network.HTTP.Link.parseLinkHeader'
                  → [URI]
discoverEndpoints rels mfRoot linkH = nub $ headerLinks ++ mfLinks
  where headerLinks = map href $ concat $ map (\r → filter (\(Link _ as) → (Rel, r) `elem` as) linkH) rels
        mfLinks = catMaybes $ map parseAesonLink $ concat $ catMaybes $ map (\r → V.toList <$> mfRoot ^? key "rels" . key r . _Array) rels
        parseAesonLink v = (T.unpack <$> v ^? _String) >>= parseURI
