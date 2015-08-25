{-# LANGUAGE QuasiQuotes, OverloadedStrings, UnicodeSyntax #-}

module Data.IndieWeb.EndpointsSpec (spec) where

import           Test.Hspec hiding (shouldBe)
import           Test.Hspec.Expectations.Pretty (shouldBe)
import           TestCommon()
import           Network.HTTP.Link
import           Data.Aeson
import           Data.IndieWeb.Endpoints

spec ∷ Spec
spec = do
  describe "discoverEndpoint" $ do
    let dW = discoverEndpoints [ "webmention", "http://webmention.org/" ]
        o = object

    it "finds endpoints in mf2" $ do
      dW (o [ "rels" .= o [ "webmention" .= [ String "http://a/wm1", String "http://a/wm2" ]
                          , "http://webmention.org/" .= [ String "http://a/wm3" ]
                          , "nope" .= [ String "http://a/nope" ]] ]) [] `shouldBe` [ "http://a/wm1", "http://a/wm2", "http://a/wm3" ]

    it "finds endpoints in the link header" $ do
      dW (o []) [ Link "http://a/wm1" [(Rel, "webmention")]
                , Link "http://a/wm2" [(Rel, "http://webmention.org/")] ] `shouldBe` [ "http://a/wm1", "http://a/wm2" ]

    it "deduplicates links" $ do
      dW (o [ "rels" .= o [ "webmention" .= [ String "http://a/wm3", String "http://a/wm4" ]
                          , "http://webmention.org/" .= [ String "http://a/wm2" ]
                          , "nope" .= [ String "http://a/nope" ]] ]) [ Link "http://a/wm1" [(Rel, "webmention")]
                                                                     , Link "http://a/wm2" [(Rel, "http://webmention.org/")] ] `shouldBe` [ "http://a/wm1", "http://a/wm2", "http://a/wm3", "http://a/wm4" ]
