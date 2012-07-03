{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL 
import qualified Data.Text as T 

import Network.Wai (Application, responseLBS, Request(..))
import Network.Wai.Test
import qualified Network.HTTP.Types as H

import Network.Wai.Middleware.Route

main :: IO ()
main = defaultMain [
      testCase "Split Text" caseSplitText
    , testCase "Route"      caseRoute
    ]

caseSplitText :: Assertion
caseSplitText = do
    [""] @=? sp ""
    ["", ""] @=? sp "/"
    ["", "a"] @=? sp "/a"
    ["", "a", "", "b", ""] @=? sp "/a//b/"
  where
    sp = T.split (=='/')

caseRoute :: Assertion
caseRoute = flip runSession routedApp $ do
    -- unhappy, setRawPathInfo has error. it producing 'pathInfo' as @[\"\"]@
    rRoot <- request defaultRequest
    assertBody "" rRoot

    testOne "foo"                   "foo"
    testOne "foo/bar"               "foo/bar"
    testOne "foo/baz"               "foo/#"
    testOne "foo/bar/baz"           "#/#/#"
    testOne "foo/bar/bast"          "#/#/bast"
    testOne "last/bar/baz"          "#/#/#"
    testOne "will/never/be/routed"  "noroute"
    
    -- Weird \"Last slash\" behaviour
    testOne "foo/bar/"              "foo/bar"
    
    -- test @POST@
    rPost <- request $ setRawPathInfo 
            defaultRequest {requestMethod="POST"} "foo/bar" 
    assertBody "post foo/bar" rPost
  where
    testOne url answer = do
        resp <- mkRequest url
        assertStatus 200 resp
        assertBody answer resp

-- | Routed application
routedApp :: Application
routedApp = 
    dispatch_ mappings $ testApp "noroute"
  where
    mappings = mkRoutes' [
          Get ""            $ testApp ""
        , Get "foo"         $ testApp "foo"
        , Get "foo/bar"     $ testApp "foo/bar"
        , Get "foo/#"       $ testApp "foo/#"
        , Get "#/#/bast"    $ testApp "#/#/bast"
        , Get "#/#/#"       $ testApp "#/#/#"
        , Get "last/*"      $ testApp "#/*"
        , Post "foo/bar"      $ testApp "post foo/bar"
        ]

-- | Simple test application
testApp :: BL.ByteString -> Application
testApp answer _req = 
    return $ responseLBS H.ok200 [] answer

-- | Default request
mkRequest :: B.ByteString -> Session SResponse
mkRequest = request . setRawPathInfo defaultRequest 

