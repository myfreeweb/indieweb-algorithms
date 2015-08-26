{-# LANGUAGE QuasiQuotes, OverloadedStrings, UnicodeSyntax, CPP #-}

module Data.IndieWeb.AuthorshipSpec (spec) where

#if __GLASGOW_HASKELL__ < 709
import           Control.Applicative
#endif
import           Test.Hspec hiding (shouldBe)
import           Test.Hspec.Expectations.Pretty (shouldBe)
import           TestCommon
import           Network.URI
import qualified Data.ByteString.Lazy as LB
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Aeson
import           Data.Microformats2.Parser
import           Data.IndieWeb.Authorship
import           Data.IndieWeb.MicroformatsUtil

spec ∷ Spec
spec = do
  describe "findAuthors" $ do
    let o = object
        mockFetch ∷ URI → Identity (Maybe LB.ByteString)
        mockFetch "http://direct" = return $ Just [xml|<body><div class="h-entry"> <data class=p-name value=Name> <div class="p-author h-card"><h1 class="p-name">Author from Direct!|]
        mockFetch "http://link" = return $ Just [xml|<body><div class="h-entry"> <data class=p-name value=Name> <a class="p-author">http://author/page</a>|]
        mockFetch "http://rel" = return $ Just [xml|<body> <a href="http://author/page" rel="author"></a><div class="h-entry"><data class=p-name value=Name>|]
        mockFetch "http://author/page/link-relative" = return $ Just [xml|<body><div class="h-entry"> <data class=p-name value=Name> <a class="p-author">/page</a>|]
        mockFetch "http://feed" = return $ Just [xml|<body><div class="h-feed"> <div class="p-author h-card"><h1 class="p-name">Author from Feed!</h1></div><div class="h-entry"><data class=p-name value=Name>|]
        mockFetch "http://feed/link" = return $ Just [xml|<body><div class="h-feed"> <a class="p-author">http://author/page</a><div class="h-entry"><data class=p-name value=Name>|]
        mockFetch "http://author/page" = return $ Just [xml|<body><div class="h-card"><h1 class="p-name">Author from Page!|]
        mockFetch _ = return Nothing
        rE u = let mf = parseMf2 def . documentRoot . parseLBS . fromMaybe "" . runIdentity $ mockFetch u in
                   runIdentity . entryAuthors def (\x → mockFetch x >>= \y → return $ parseLBS <$> y) u mf . head . fromJust $ allMicroformatsOfType "h-entry" mf
        card n = o [ "value" .= String n
                   , "type" .= [ String "h-card" ]
                   , "properties" .= o [ "name" .= [ String n ] ] ]
        card' n = o [ "type" .= [ String "h-card" ]
                    , "properties" .= o [ "name" .= [ String n ] ] ]

    it "finds the author h-cards embedded in the h-entry" $ do
      rE "http://direct" `shouldBe` Just [ card "Author from Direct!" ]

    it "finds the author h-cards embedded in the h-feed parent of h-entry" $ do
      rE "http://feed" `shouldBe` Just [ card "Author from Feed!" ]

    it "finds the author h-cards linked from the h-entry" $ do
      rE "http://link" `shouldBe` Just [ card' "Author from Page!" ]

    it "finds the author h-cards linked from the h-feed parent of h-entry" $ do
      rE "http://feed/link" `shouldBe` Just [ card' "Author from Page!" ]

    it "finds the author h-cards linked as rel=author" $ do
      rE "http://rel" `shouldBe` Just [ card' "Author from Page!" ]
      rE "http://author/page/link-relative" `shouldBe` Just [ card' "Author from Page!" ]
