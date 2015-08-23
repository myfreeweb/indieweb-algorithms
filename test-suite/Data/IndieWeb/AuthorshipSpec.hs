{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings, UnicodeSyntax #-}

module Data.IndieWeb.AuthorshipSpec (spec) where

import           Test.Hspec hiding (shouldBe)
import           Test.Hspec.Expectations.Pretty (shouldBe)
import           TestCommon
import           Network.URI
import qualified Data.ByteString.Lazy as LB
import           Data.Functor.Identity
import           Data.Maybe
import           Data.String
import           Data.Aeson
import           Data.Default
import           Data.Microformats2.Parser
import           Data.IndieWeb.Authorship

instance IsString URI where
    fromString = fromJust . parseURI

spec ∷ Spec
spec = do
  describe "reprEntryWithAuthor" $ do
    let o = object
        mockFetch ∷ URI → Identity (Maybe LB.ByteString)
        mockFetch "http://direct" = return $ Just [xml|<body><div class="h-entry"> <data class=p-name value=Name> <div class="p-author h-card"><h1 class="p-name">Author from Direct!|]
        mockFetch "http://link" = return $ Just [xml|<body><div class="h-entry"> <data class=p-name value=Name> <a class="p-author">http://author/page</a>|]
        mockFetch "http://rel" = return $ Just [xml|<body> <a href="http://author/page" rel="author"></a><div class="h-entry"><data class=p-name value=Name>|]
        mockFetch "http://author/page/link-relative" = return $ Just [xml|<body><div class="h-entry"> <data class=p-name value=Name> <a class="p-author">/page</a>|]
        mockFetch "http://feed" = return $ Just [xml|<body><div class="h-feed"> <div class="p-author h-card"><h1 class="p-name">Author from Feed!</h1></div><div class="h-entry"><data class=p-name value=Name>|]
        mockFetch "http://feed/link" = return $ Just [xml|<body><div class="h-feed"> <a class="p-author">http://author/page</div><div class="h-entry"><data class=p-name value=Name>|]
        mockFetch "http://author/page" = return $ Just [xml|<body><div class="h-card"><h1 class="p-name">Author from Page!|]
        mockFetch _ = return Nothing
        rE = fromJust . runIdentity . reprEntryWithAuthor mockFetch . parseMf2 def . documentRoot . parseLBS . fromMaybe "" . runIdentity . mockFetch

    it "parses author" $ do
      pending
      rE "http://direct" `shouldBe` o [ "type" .= [ String "h-entry" ]
                                      , "properties" .= o [ "name" .= [ String "Name" ]
                                                          , "author" .= [ o [ "type" .= [ String "h-card" ]
                                                                            , "properties" .= o [ "name" .= [ String "Author from Direct!" ] ] ] ] ] ]
    -- it "parses author-link" $ parseWithAuthor "http://link" `shouldBe` (Identity $ Just $ def { entryAuthor = [ CardCard $ def { cardName = pure "Author from Page!" } ] })
    -- it "parses rel" $ parseWithAuthor "http://rel" `shouldBe` (Identity $ Just $ def { entryAuthor = [ CardCard $ def { cardName = pure "Author from Page!" } ] })
    -- it "parses relative author-link" $ parseWithAuthor "http://author/page/link-relative" `shouldBe` (Identity $ Just $ def { entryAuthor = [ CardCard $ def { cardName = pure "Author from Page!" } ] })
    -- it "parses h-feed author" $ parseWithAuthor "http://feed" `shouldBe` (Identity $ Just $ def { entryAuthor = [ CardCard $ def { cardName = pure "Author from Feed!" } ] })
    -- it "parses h-feed author-link" $ parseWithAuthor "http://feed/link" `shouldBe` (Identity $ Just $ def { entryAuthor = [ CardCard $ def { cardName = pure "Author from Page!" } ] })
