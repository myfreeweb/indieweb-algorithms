{-# LANGUAGE QuasiQuotes, OverloadedStrings, UnicodeSyntax #-}

module Data.IndieWeb.MicroformatsToAtomSpec  where

import           Test.Hspec
import           TestCommon
import           Text.XML
import           Data.Text.Lazy (Text, unpack)
import           Data.Aeson hiding (json)
import           Data.IndieWeb.MicroformatsToAtom

ex ∷ Value
ex = [json|{
    "items": [
        {
            "type": [ "h-entry" ],
            "properties": {
                "published": [ "2016-03-15T21:03:52.570481000000Z" ],
                "url": [ "https://unrelenting.technology/notes/2016-03-15-21-03-52" ],
                "content": [ { "value": "Updated the build of sweetroll on this website", "html": " \u003cp\u003eUpdated the build of \u003ca href=\"https://github.com/myfreeweb/sweetroll\"\u003esweetroll\u003c/a\u003e on this website \u003c/p\u003e\u003ca href=\"https://brid.gy/publish/twitter\"\u003e\u003c/a\u003e " } ],
                "name": [ "Updated the build of sweetroll on this website" ]
            }
        },
        {
            "type": [ "h-entry" ],
            "properties": {
                "published": [ "2016-03-14T22:23:11.089Z" ],
                "url": [ "https://unrelenting.technology/responses/reply" ],
                "content": [ { "value": "(Reply test)", "html": " \u003cp\u003e(Reply test)\u003c/p\u003e" } ],
                "name": [ "(Reply test)" ],
                "in-reply-to": [
                    {
                        "type": [ "h-entry" ],
                        "properties": {
                            "url": [ "https://unrelenting.technology/responses/parent" ]
                        }
                    }
                ]
            }
        },
        {
            "type": [ "h-entry" ],
            "properties": {
                "published": [ "2016-03-14T22:23:11.089Z" ],
                "url": [ "https://unrelenting.technology/notes/repost" ],
                "content": [ { "value": "(Repost test)", "html": " \u003cp\u003e(Repost test)\u003c/p\u003e" } ],
                "name": [ "(Repost test)" ],
                "repost-of": [
                    {
                        "type": [ "h-entry" ],
                        "properties": {
                            "published": [ "2016-03-14T22:23:11.089Z" ],
                            "url": [ "https://unrelenting.technology/notes/original" ],
                            "content": [ { "value": "(Repost original)", "html": " \u003cp\u003e(Repost original)\u003c/p\u003e" } ],
                            "name": [ "(Repost original)" ]
                        }
                    }
                ]
            }
        },
        {
            "type": [ "h-card" ],
            "properties": {
                "name": [ "Greg" ],
                "email": [ "mailto:greg@unrelenting.technology" ],
                "photo": [ "https://www.gravatar.com/avatar/5e7281cf21d93c6a238749790afe086c" ],
                "note": [ "My name's Greg. I do a lot of software development (mostly web and mobile), some design and photography. my.pronoun.is/they/them." ],
                "url": [ "https://unrelenting.technology/" ],
                "key": [ "https://unrelenting.technology/pub/3B011BAF.asc" ],
                "x-pronoun-oblique": [ "them" ],
                "x-pronoun-nominative": [ "they" ]
            }
        }
    ],
    "rels": {
        "home": [ "https://unrelenting.technology/" ],
        "author": [ "https://unrelenting.technology/" ]
    },
    "rel-urls": {
        "https://unrelenting.technology/": { "text": "Greg", "rels": [ "me", "home", "author" ] }
    }
} |]

exAtom ∷ Text
exAtom = [xml|
<?xml version="1.0" encoding="UTF-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <author>
    <name>Greg</name>
    <uri>https://unrelenting.technology/</uri>
    <email>greg@unrelenting.technology</email>
    <link href="https://unrelenting.technology/" rel="alternate" type="text/html"/>
    <link href="https://www.gravatar.com/avatar/5e7281cf21d93c6a238749790afe086c" rel="avatar"/>
    <object-type xmlns="http://activitystrea.ms/spec/1.0/">http://activitystrea.ms/schema/1.0/person</object-type>
  </author>
  <logo>https://www.gravatar.com/avatar/5e7281cf21d93c6a238749790afe086c</logo>
  <entry>
    <id>https://unrelenting.technology/notes/2016-03-15-21-03-52</id>
    <title>Updated the build of sweetroll on this website</title>
    <link href="https://unrelenting.technology/notes/2016-03-15-21-03-52" rel="alternate" type="text/html"/>
    <published>2016-03-15T21:03:52.570481000000Z</published>
    <content type="html"> \u003cp\u003eUpdated the build of \u003ca href="https://github.com/myfreeweb/sweetroll"\u003esweetroll\u003c/a\u003e on this website \u003c/p\u003e\u003ca href="https://brid.gy/publish/twitter"\u003e\u003c/a\u003e </content>
    <verb xmlns="http://activitystrea.ms/spec/1.0/">post</verb>
    <object-type xmlns="http://activitystrea.ms/spec/1.0/">http://activitystrea.ms/schema/1.0/note</object-type>
  </entry>
  <entry>
    <id>https://unrelenting.technology/responses/reply</id>
    <title>(Reply test)</title>
    <link href="https://unrelenting.technology/responses/reply" rel="alternate" type="text/html"/>
    <published>2016-03-14T22:23:11.089Z</published>
    <content type="html"> \u003cp\u003e(Reply test)\u003c/p\u003e</content>
    <verb xmlns="http://activitystrea.ms/spec/1.0/">post</verb>
    <object-type xmlns="http://activitystrea.ms/spec/1.0/">http://activitystrea.ms/schema/1.0/comment</object-type>
    <in-reply-to xmlns="http://purl.org/syndication/thread/1.0" ref="https://unrelenting.technology/responses/parent" href="https://unrelenting.technology/responses/parent"></in-reply-to>
  </entry>
  <entry>
    <id>https://unrelenting.technology/notes/repost</id>
    <title>(Repost test)</title>
    <link href="https://unrelenting.technology/notes/repost" rel="alternate" type="text/html"/>
    <published>2016-03-14T22:23:11.089Z</published>
    <content type="html"> \u003cp\u003e(Repost test)\u003c/p\u003e</content>
    <verb xmlns="http://activitystrea.ms/spec/1.0/">share</verb>
    <in-reply-to xmlns="http://purl.org/syndication/thread/1.0" ref="https://unrelenting.technology/notes/original" href="https://unrelenting.technology/responses/parent"></in-reply-to>
    <object-type xmlns="http://activitystrea.ms/spec/1.0/">http://activitystrea.ms/schema/1.0/activity</object-type>
    <object xmlns="http://activitystrea.ms/spec/1.0/">
      <id>https://unrelenting.technology/notes/original</id>
      <title>(Repost original)</title>
      <link href="https://unrelenting.technology/notes/original" rel="alternate" type="text/html"/>
      <published>2016-03-14T22:23:11.089Z</published>
      <content type="html"> \u003cp\u003e(Repost original)\u003c/p\u003e</content>
      <verb xmlns="http://activitystrea.ms/spec/1.0/">post</verb>
      <object-type xmlns="http://activitystrea.ms/spec/1.0/">http://activitystrea.ms/schema/1.0/note</object-type>
    </object>
  </entry>
</feed>|]

spec ∷ Spec
spec = do
  describe "feedToAtom" $ do
    it "converts h-entries to an Atom feed" $ do
      --feedToAtom empty ex `shouldBe` parseText_ def exAtom
      --putStrLn $ unpack $ renderText def $ feedToAtom empty ex
      pendingWith "XML comparison sucks because of whitespace nodes and namespaces"
