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
