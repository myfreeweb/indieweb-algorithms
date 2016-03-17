{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Data.IndieWeb.MicroformatsUtil where

import           Control.Lens
import           Data.Maybe
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T

-- | Recursively finds microformats of a given type, also keeping the path (in reverse order) that led to them.
-- Returns Nothing when the given Value isn't an object with an 'items' key, ie. is not a result of Microformats 2 parsing.
allMicroformatsOfType ∷ T.Text → Value → Maybe [(Value, [Value])]
allMicroformatsOfType typeName mf = do
  items ← mf ^? key "items" . _Array
  let findOfType acc path v =
        let valToList (Array vec) = V.toList vec
            valToList _ = []
            childMfs = V.toList $ fromMaybe V.empty $ v ^? key "children" . _Array
            propMfs = concatMap valToList $ HMS.elems $ fromMaybe HMS.empty $ v ^? key "properties" . _Object
            acc' = acc ++ concatMap (findOfType acc $ v : path) (childMfs ++ propMfs) in
        if String typeName `V.elem` fromMaybe V.empty (v ^? key "type" . _Array)
           then (v, path) : acc'
           else acc'
  Just $ concatMap (findOfType [] []) items

-- | Tests whether a given Value is of a given microformat type (e.g. h-entry, h-feed).
isMf ∷ T.Text → Value → Bool
isMf t = (String t `V.elem`) . (fromMaybe V.empty) . (^? key "type" . _Array)

data EntryType = Reply | Repost | Like | Bookmark | RSVP | Checkin | Event | Audio | Photo | Article | Note
  deriving (Show, Eq)

-- | Detects a post type (https://indiewebcamp.com/posts#Kinds_of_Posts)
detectEntryType ∷ Value → EntryType
detectEntryType entry
  | isJust (entry ^? key "properties" . key "in-reply-to" . nth 0) = Reply
  | isJust (entry ^? key "properties" . key "repost-of" . nth 0) = Repost
  | isJust (entry ^? key "properties" . key "like-of" . nth 0) = Like
  | isJust (entry ^? key "properties" . key "bookmark-of" . nth 0) = Bookmark
  | isJust (entry ^? key "properties" . key "rsvp" . nth 0) = RSVP
  | isJust (entry ^? key "properties" . key "location" . nth 0) = Checkin
  | isJust (entry ^? key "properties" . key "event" . nth 0) = Event
  | isJust (entry ^? key "properties" . key "audio" . nth 0) = Audio
  | isJust (entry ^? key "properties" . key "photo" . nth 0) = Photo
  | isJust (entry ^? key "properties" . key "name" . nth 0) && (entry ^? key "properties" . key "name" . nth 0) /= (entry ^? key "properties" . key "content" . nth 0 . key "value") = Article
  | otherwise = Note
