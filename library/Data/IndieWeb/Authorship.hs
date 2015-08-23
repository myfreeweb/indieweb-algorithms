{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Data.IndieWeb.Authorship where

import           Control.Monad
import           Control.Monad.Trans.Maybe
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as LB
import           Data.Aeson
import           Data.Microformats2.Parser
import           Network.URI

-- | Parses the representative h-entry on a URI, discovering its authorship <http://indiewebcamp.com/authorship>, using the HTTP fetcher function from arguments.
reprEntryWithAuthor ∷ Monad μ ⇒ (URI → μ (Maybe LB.ByteString)) → Value → μ (Maybe Value)
reprEntryWithAuthor fetch mf = return (Just Null)
