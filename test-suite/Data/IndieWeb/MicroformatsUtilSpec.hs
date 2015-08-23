{-# LANGUAGE QuasiQuotes, OverloadedStrings, UnicodeSyntax #-}

module Data.IndieWeb.MicroformatsUtilSpec (spec) where

import           Test.Hspec hiding (shouldBe)
import           Test.Hspec.Expectations.Pretty (shouldBe)
import           TestCommon
import           Data.Maybe
import           Data.Aeson hiding (json)
import           Data.Default
import           Data.Microformats2.Parser
import           Data.IndieWeb.MicroformatsUtil

spec ∷ Spec
spec = do
  describe "allMicroformatsOfType" $ do
    it "recursively finds microformats in items" $ do
      let mf = parseMf2 def $ documentRoot $ parseLBS [xml|<body>
        <div class=h-entry>
          <h1 class=p-name>Entry</h1>
          <div class=h-child>
            <data class=p-name value=Child>
            <div class=h-grand-child>Grandchild</div>
          </div>
          <div class="p-prop h-prop-child">
            <data class=p-name value=PropChild>
            <div class=h-grand-child>PropGrandchild</div>
          </div>
        </div>|]
          entryObj = [json|{"type":["h-entry"],"children":[{"value":"Child","type":["h-child"],"children":[{"value":"Grandchild","type":["h-grand-child"],"properties":{"name":["Grandchild"]}}],"properties":{"name":["Child"]}}],"properties":{"name":["Entry"],"prop":[{"value":"PropChild","type":["h-prop-child"],"children":[{"value":"PropGrandchild","type":["h-grand-child"],"properties":{"name":["PropGrandchild"]}}],"properties":{"name":["PropChild"]}}]}}|]
          childObj = [json|{"value":"Child","type":["h-child"],"children":[{"value":"Grandchild","type":["h-grand-child"],"properties":{"name":["Grandchild"]}}],"properties":{"name":["Child"]}}|]
          grandchildObj = [json|{"value":"Grandchild","type":["h-grand-child"],"properties":{"name":["Grandchild"]}}|]
          propchildObj = [json|{"value":"PropChild","type":["h-prop-child"],"children":[{"value":"PropGrandchild","type":["h-grand-child"],"properties":{"name":["PropGrandchild"]}}],"properties":{"name":["PropChild"]}}|]
          propgrandchildObj = [json|{"value":"PropGrandchild","type":["h-grand-child"],"properties":{"name":["PropGrandchild"]}}|]
      allMicroformatsOfType "h-blah" mf `shouldBe` Just []
      allMicroformatsOfType "h-entry" mf `shouldBe` Just [ (entryObj, []) ]
      allMicroformatsOfType "h-child" mf `shouldBe` Just [ (childObj, [entryObj]) ]
      allMicroformatsOfType "h-prop-child" mf `shouldBe` Just [ (propchildObj, [entryObj]) ]
      allMicroformatsOfType "h-grand-child" mf `shouldBe` Just [ (grandchildObj, [childObj, entryObj]), (propgrandchildObj, [propchildObj, entryObj]) ]
