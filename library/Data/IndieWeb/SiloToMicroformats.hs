{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Data.IndieWeb.SiloToMicroformats where

import           Prelude
import           Text.XML.Lens hiding ((.=))
import qualified Data.Text as T
import           Data.Aeson
import           Data.Aeson.Lens (key)
import           Data.Monoid
import           Data.Maybe
import           Control.Applicative
import           Data.Microformats2.Parser.HtmlUtil
import           Data.Microformats2.Parser.Property (getImgSrc)
import           Data.Microformats2.Parser (extractProperty, Mf2ParserSettings)

-- | Parses a twitter.com tweet into an h-entry value.
parseTwitter ∷ Mf2ParserSettings → Element → Maybe Value
parseTwitter s r = if isJust tweet && isJust username && isJust tweetText
                      then Just wrapper
                      else Nothing
  where wrapper = object [ "type" .= [ String "h-entry" ]
                         , "properties" .= props ]
        props = object [ "uid" .= [ permalink ]
                       , "url" .= [ permalink ]
                       , "content" .= [ tweetText ]
                       , "name" .= [ fromMaybe Null $ tweetText >>= (^? key "value") ]
                       , "published" .= [ str timestamp ]
                       , "author" .= [ author ]
                       , "comment" .= comments ]
        author = object [ "type" .= [ String "h-card" ]
                        , "properties" .= object [ "uid" .= [ profileUrl username ]
                                                 , "url" .= [ profileUrl username ]
                                                 , "name" .= [ str fullname ]
                                                 , "nickname" .= [ str username ]
                                                 , "photo" .= [ str avatar ] ] ]
        comments = mapMaybe (parseTwitter s) $ r ^.. hasClass "permalink-replies" ./ hasClass "tweet"
        profileUrl (Just x) = String $ "https://twitter.com/" <> T.dropWhile (== '@') x
        profileUrl _ = Null
        tweetText = tweet >>= (^? hasClass "tweet-text") >>= return . extractProperty s "e"
        fullname = tweet >>= (^? hasClass "fullname") >>= getInnerTextRaw
        timestamp = tweet >>= (^? hasClass "_timestamp" . attr "data-time")
        username = tweet >>= (^? hasClass "username") >>= getInnerTextRaw
        avatar = tweet >>= (^? hasClass "avatar") >>= getImgSrc
        permalink = tweet >>= (^? hasClass "tweet-timestamp") >>= return . extractProperty s "u"
        tweet = r ^? hasClass "permalink-tweet" <|> r ^? hasClass "tweet"
        hasClass c = entire . attributeSatisfies "class" (c `T.isInfixOf`)
        str (Just x) = String x
        str _ = Null
