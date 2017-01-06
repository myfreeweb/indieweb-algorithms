{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Data.IndieWeb.MicroformatsToAtom (
  feedToAtom
, atomForEntry
, asForEntry
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

atom ∷ Text → Name
atom x = Name x (Just "http://www.w3.org/2005/Atom") Nothing

el ∷ ToXML a ⇒ Text → a → XML
el x = element (atom x)

elA ∷ ToXML a ⇒ Text → [(Name, Text)] → a → XML
elA x = elementA (atom x)

firstStr ∷ AsValue s ⇒ Text → s → Maybe Text
firstStr k = (^? key "properties" . key k . nth 0 . _String)

prop ∷ (t → XML) → Maybe t → XML
prop f (Just x) = f x
prop _ _ = empty


-- | Converts a givent Microformats 2 parse to an Atom feed.
feedToAtom ∷ XML → Value → Document
feedToAtom metaXml mfRoot =
  document (atom "feed") $ do
    metaXml

    let isAuthorOfFeed (_, p:_) = isMf "h-feed" p
        isAuthorOfFeed _ = False
        author = take 1 $ reverse $ sortOn isAuthorOfFeed $ fromMaybe [] $ allMicroformatsOfType "h-card" mfRoot
    forM_ author $ \(card, _) → do
      atomForCard card
      prop (el "logo" . content) $ firstStr "photo" card

    let entries = fromMaybe [] $ allMicroformatsOfType "h-entry" mfRoot
    forM_ entries $ \(entry, path) →
      when (null path) $ el "entry" $ atomForEntry entry


atomForCard ∷ Value → XML
atomForCard card = el "author" $ do
  prop (el "name" . content) $ firstStr "name" card
  prop (el "uri" . content) $ firstStr "url" card
  prop (el "email" . content) $ (\x → if "mailto:" `T.isPrefixOf` x then T.drop 7 x else x) <$> firstStr "email" card
  prop (\u → elA "link" [("rel", "alternate"), ("type", "text/html"), ("href", u)] empty) $ firstStr "url" card
  prop (\u → elA "link" [("rel", "avatar"), ("href", u)] empty) $ firstStr "photo" card
  asType "person"

atomForEntry ∷ Value → XML
atomForEntry entry = do
  prop (el "id" . content) $ firstStr "uid" entry <|> firstStr "url" entry
  el "title" $ content $ if detectEntryType entry == Note then "" else (fromMaybe "" $ firstStr "name" entry)
  prop (\u → elA "link" [("rel", "alternate"), ("type", "text/html"), ("href", u)] empty) $ firstStr "url" entry
  prop (el "published" . content) $ firstStr "published" entry
  prop (el "updated" . content) $ firstStr "updated" entry
  prop (elA "content" [("type", "html")] . content) $ entry ^? key "properties" . key "content" . nth 0 . key "html" . _String
  mapM_ atomForCard (entry ^.. key "properties" . key "author")
  asForEntry entry

asType, asVerb ∷ Text → XML
asType x = element (Name "object-type" (Just "http://activitystrea.ms/spec/1.0/") Nothing) $ content $ "http://activitystrea.ms/schema/1.0/" <> x
asVerb x = element (Name "verb" (Just "http://activitystrea.ms/spec/1.0/") Nothing) $ content x


thrInReplyTo ∷ Text → XML
thrInReplyTo x = elementA (Name "in-reply-to" (Just "http://purl.org/syndication/thread/1.0") Nothing) [("ref", x), ("href", x)] empty

asForEntry ∷ Value → XML
asForEntry entry = asForEntry' (detectEntryType entry) entry
  where asForEntry' Like e = asVerb "like" >> nest e "like-of"
        asForEntry' RSVP e = fromMaybe empty (asVerb . ("rsvp-" <>) <$> e ^? key "properties" . key "rsvp" . nth 0 . _String) >> asType "event"
        asForEntry' Checkin _ = asVerb "checkin" >> asType "place"
        asForEntry' Repost e = asVerb "share" >> nest e "repost-of"
        asForEntry' Reply e = asVerb "post" >> asType "comment" >> mapM_ thrInReplyTo (refUrls e "in-reply-to")
        asForEntry' Bookmark _ = asVerb "post" >> asType "bookmark"
        asForEntry' Article _ = asVerb "post" >> asType "article"
        asForEntry' _ _ = asVerb "post" >> asType "note"
        refUrls e k = e ^.. key "properties" . key k . values . key "properties" . key "url" . values . _String
        nest e k = forM_ (e ^.. key "properties" . key k . values) $ \ee → do
          asType "activity"
          element (Name "object" (Just "http://activitystrea.ms/spec/1.0/") Nothing) $ atomForEntry ee
