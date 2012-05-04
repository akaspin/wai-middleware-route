{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import qualified Data.Text as T 

import Network.Wai (Application, responseLBS)
import Network.Wai.Test
import qualified Network.HTTP.Types as H

main :: IO ()
main = defaultMain [
        testCase "Split Text" caseSplitText
    ]

caseSplitText :: Assertion
caseSplitText = do
    [""] @=? sp ""
    ["", ""] @=? sp "/"
    ["", "a"] @=? sp "/a"
    ["", "a", "", "b", ""] @=? sp "/a//b/"
  where
    sp = T.split (=='/')
    
