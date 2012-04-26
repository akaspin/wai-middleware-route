{-# LANGUAGE OverloadedStrings #-} 

-- | Tests
module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import Data.ByteString.Char8 ()

import Network.Wai (Application)
import Network.Wai.Middleware.Route


main :: IO ()
main = undefined

