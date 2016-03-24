{-# LANGUAGE QuasiQuotes, OverloadedStrings, UnicodeSyntax, CPP #-}

module Data.IndieWeb.AuthorshipSpec (spec) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Test.Hspec hiding (shouldBe)
import           Test.Hspec.Expectations.Pretty (shouldBe)
import           TestCommon
import           Network.URI
import qualified Data.ByteString.Lazy as LB
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Aeson.Lens
import           Control.Lens ((^?), _Just, _head)
import           Data.Microformats2.Parser
import           Data.IndieWeb.Authorship
import           Data.IndieWeb.MicroformatsUtil

spec ∷ Spec
spec = do
  describe "findAuthors" $ do
    let mockFetch ∷ URI → Identity (Maybe LB.ByteString)
        mockFetch "http://direct" = return $ Just [xml|<body><div class="h-entry"> <data class=p-name value=Name> <div class="p-author h-card"><h1 class="p-name">Author from Direct!|]
        mockFetch "http://link" = return $ Just [xml|<body><div class="h-entry"> <data class=p-name value=Name> <a class="p-author">http://author/page</a>|]
        mockFetch "http://link-relme" = return $ Just [xml|<body><div class="h-entry"> <data class=p-name value=Name> <a class="p-author">http://author/page-relme</a>|]
        mockFetch "http://link-only-url" = return $ Just [xml|<body><div class="h-entry"> <data class=p-name value=Name> <a class="p-author">http://author/page-only-url</a>|]
        mockFetch "http://link-not-only-url" = return $ Just [xml|<body><div class="h-entry"> <data class=p-name value=Name> <a class="p-author">http://author/page-not-only-url</a>|]
        mockFetch "http://rel" = return $ Just [xml|<body> <a href="http://author/page" rel="author"></a><div class="h-entry"><data class=p-name value=Name>|]
        mockFetch "http://author/page/link-relative" = return $ Just [xml|<body><div class="h-entry"> <data class=p-name value=Name> <a class="p-author">/page</a>|]
        mockFetch "http://feed" = return $ Just [xml|<body><div class="h-feed"> <div class="p-author h-card"><h1 class="p-name">Author from Feed!</h1></div><div class="h-entry"><data class=p-name value=Name>|]
        mockFetch "http://feed/link" = return $ Just [xml|<body><div class="h-feed"> <a class="p-author">http://author/page</a><div class="h-entry"><data class=p-name value=Name>|]
        mockFetch "http://author/page" = return $ Just [xml|<body><div class="h-card"><a class="u-url u-uid" href="http://author/page"><h1 class="p-name">Author from Page!|]
        mockFetch "http://author/page-relme" = return $ Just [xml|<body><div class="h-card"><a rel="me" classexample="u-url" href="http://example"><h1 class="p-name">Author from Page-relme!|]
        mockFetch "http://author/page-only-url" = return $ Just [xml|<body><div class="h-card"><a class="u-url" href="http://author/page-only-url"><h1 class="p-name">Author from Page-only-url!|]
        mockFetch "http://author/page-not-only-url" = return $ Just [xml|<body><div class="h-card"><a class="u-url" href="http://author/page-not-only-url"><h1 class="p-name">Author from Page-not-only-url!</div><div class="h-card"><a class="u-url" href="http://author/page-not-only-url"><h1 class="p-name">Author from Page-not-only-url!|]
        mockFetch _ = return Nothing
        rE u = let mf = parseMf2 def . documentRoot . parseLBS . fromMaybe "" . runIdentity $ mockFetch u in
                   runIdentity . entryAuthors def (\x → mockFetch x >>= \y → return $ parseLBS <$> y) u mf . head . fromJust $ allMicroformatsOfType "h-entry" mf
        firstName = _Just . _head . key "properties" . key "name" . nth 0 . _String

    it "finds the author h-cards embedded in the h-entry" $ do
      rE "http://direct" ^? firstName `shouldBe` Just "Author from Direct!"

    it "finds the author h-cards embedded in the h-feed parent of h-entry" $ do
      rE "http://feed" ^? firstName `shouldBe` Just "Author from Feed!"

    it "finds the author h-cards linked from the h-entry" $ do
      rE "http://link" ^? firstName `shouldBe` Just "Author from Page!"
      rE "http://link-relme" ^? firstName `shouldBe` Just "Author from Page-relme!"
      rE "http://link-only-url" ^? firstName `shouldBe` Just "Author from Page-only-url!"
      rE "http://link-not-only-url" ^? firstName `shouldBe` Nothing

    it "finds the author h-cards linked from the h-feed parent of h-entry" $ do
      rE "http://feed/link" ^? firstName `shouldBe` Just "Author from Page!"

    it "finds the author h-cards linked as rel=author" $ do
      rE "http://rel" ^? firstName `shouldBe` Just "Author from Page!"
      rE "http://author/page/link-relative" ^? firstName `shouldBe` Just "Author from Page!"
