{-# LANGUAGE OverloadedStrings, UnicodeSyntax, CPP, FlexibleContexts #-}

module Data.IndieWeb.Authorship where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Lens
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Foldable (asum, find)
import           Data.Maybe
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Microformats2.Parser
import           Data.IndieWeb.MicroformatsUtil
import           Text.XML (Document)
import           Network.URI
import           Safe (headMay)

-- | Finds the authors of an h-entry, discovering its authorship <http://indiewebcamp.com/authorship> using the HTTP fetcher function from arguments.
entryAuthors ∷ Monad μ ⇒ Mf2ParserSettings
                       → (URI → μ (Maybe Document)) -- ^ the URI fetcher function
                       → URI -- ^ the URI of the page the entry was extracted from
                       → Value -- ^ the full Microformats 2 parse of the page as extracted by 'Data.Microformats2.Parser.parseMf2'
                       → (Value, [Value]) -- ^ (the h-entry, [parent microformats]) as extracted by 'Data.IndieWeb.MicroformatsUtil.allMicroformatsOfType'
                       → μ (Maybe [Value])
entryAuthors mfSettings fetch entryUri mfRoot (entry, parents) = runMaybeT $ asum $ map MaybeT [ embeddedCards, relCards ]
  where fetchIfLink v
          | isMf "h-card" v = return $ Just v
          | otherwise = case (T.unpack <$> v ^? _String) >>= parseURIReference of
                          Nothing → return $ Just v
                          Just uri → cardFromUri uri
        embeddedCards = case asum [ entryAuthor, feedAuthor ] of
                          Nothing → return Nothing
                          Just authors → liftM (Just . catMaybes) $ mapM fetchIfLink authors
        relCards = case V.toList <$> mfRoot ^? key "rels" . key "author" . _Array of
                     Nothing → return Nothing
                     Just rels → liftM (Just . catMaybes) $ mapM fetchIfLink rels
        entryAuthor = getAuthorProp entry
        feedAuthor = getAuthorProp =<< (headMay $ filter (isMf "h-feed") parents)
        getAuthorProp = (V.toList <$>) . (^? key "properties" . key "author" . _Array)
        cardFromUri uri = do
          -- TODO: only allow http(s)
          let uri' = uri `relativeTo` entryUri
          html ← fetch uri'
          let mfSettings' = mfSettings { baseUri = Just uri' }
          return $ representativeCard uri' =<< parseMf2 mfSettings' <$> documentRoot <$> html

representativeCard ∷ URI → Value → Maybe Value
representativeCard uri mfRoot = asum [ matchingUidUrl, matchingRelMeUrl, matchingOnlyUrl ]
  where matchingUidUrl = find (\c → any (== uri) (uris "url" c) && any (== uri) (uris "uid" c)) cards
        matchingRelMeUrl = find (any (`elem` relMeUris) . uris "url") cards
        matchingOnlyUrl = if length cards /= 1 then Nothing else find (any (== uri) . uris "url") cards
        relMeUris = mapMaybe (parseURI . T.unpack) $ mfRoot ^.. key "rels" . key "me" . values . _String
        uris k c  = mapMaybe (parseURI . T.unpack) $ c ^.. key "properties" . key k . values . _String
        cards = fmap fst $ fromMaybe [] $ allMicroformatsOfType "h-card" mfRoot
