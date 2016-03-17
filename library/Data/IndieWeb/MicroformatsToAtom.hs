{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Data.IndieWeb.MicroformatsToAtom (
  module Data.IndieWeb.MicroformatsToAtom
, empty
) where

import           Control.Lens hiding (element)
import           Control.Monad
import           Control.Applicative ((<|>))
import           Data.Monoid
import           Data.Maybe
import           Data.Text as T (Text, isPrefixOf, drop)
import           Data.List (sortOn)
import           Data.Aeson
import           Data.Aeson.Lens
import           Text.XML.Writer
import           Text.XML
import           Data.IndieWeb.MicroformatsUtil

-- | Converts a givent Microformats 2 parse to an Atom feed.
feedToAtom ∷ XML → Value → Document
feedToAtom metaXml mfRoot =
  let atom x = Name x (Just "http://www.w3.org/2005/Atom") Nothing
      el x = element (atom x)
      elA x = elementA (atom x)
      firstStr k = (^? key "properties" . key k . nth 0 . _String)
      prop f (Just x) = f x
      prop _ _ = empty

      cardToAtom card = el "author" $ do
        prop (el "name" . content) $ firstStr "name" card
        prop (el "uri" . content) $ firstStr "url" card
        prop (el "email" . content) $ (\x → if "mailto:" `T.isPrefixOf` x then T.drop 7 x else x) <$> firstStr "email" card
        prop (\u → elA "link" [("rel", "alternate"), ("type", "text/html"), ("href", u)] empty) $ firstStr "url" card
        prop (\u → elA "link" [("rel", "avatar"), ("href", u)] empty) $ firstStr "photo" card
        asType "person"

   in document (atom "feed") $ do
        metaXml

        let isAuthorOfFeed (_, p:_) = isMf "h-feed" p
            isAuthorOfFeed _ = False
            author = take 1 $ reverse $ sortOn isAuthorOfFeed $ fromMaybe [] $ allMicroformatsOfType "h-card" mfRoot
        forM_ author $ \(card, _) → do
          cardToAtom card
          prop (el "logo" . content) $ firstStr "photo" card

        let entries = fromMaybe [] $ allMicroformatsOfType "h-entry" mfRoot
        forM_ entries $ \(entry, _) →
          el "entry" $ do
            prop (el "id" . content) $ firstStr "uid" entry <|> firstStr "url" entry
            el "title" $ content $ if detectEntryType entry == Note then "" else (fromMaybe "" $ firstStr "name" entry)
            prop (\u → elA "link" [("rel", "alternate"), ("type", "text/html"), ("href", u)] empty) $ firstStr "url" entry
            prop (el "published" . content) $ firstStr "published" entry
            prop (el "updated" . content) $ firstStr "updated" entry
            prop (elA "content" [("type", "html")] . content) $ entry ^? key "properties" . key "content" . nth 0 . key "html" . _String
            mapM_ cardToAtom (entry ^.. key "properties" . key "author")
            asForEntry entry

asType, asVerb ∷ Text → XML
asType x = element (Name "object-type" (Just "http://activitystrea.ms/spec/1.0/") Nothing) $ content $ "http://activitystrea.ms/schema/1.0/" <> x
asVerb x = element (Name "verb" (Just "http://activitystrea.ms/spec/1.0/") Nothing) $ content x

asForEntry ∷ Value → XML
asForEntry entry = asForEntry' (detectEntryType entry) entry
  where asForEntry' Like e = asVerb "like" >> fromMaybe empty (typeForEntry <$> detectEntryType <$> e ^? key "properties" . key "like-of" . nth 0)
        asForEntry' RSVP e = fromMaybe empty (asVerb . ("rsvp-" <>) <$> e ^? key "properties" . key "rsvp" . nth 0 . _String) >> asType "event"
        asForEntry' Checkin _ = asVerb "checkin" >> asType "place"
        asForEntry' Repost _ = asVerb "share" >> typeForEntry Repost
        asForEntry' x _ = asVerb "post" >> typeForEntry x
        typeForEntry Reply = asType "comment"
        typeForEntry Bookmark = asType "bookmark"
        typeForEntry Article = asType "article"
        typeForEntry _ = asType "note"
