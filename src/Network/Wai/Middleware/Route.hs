{-# LANGUAGE OverloadedStrings #-}

{- | Route middleware for wai.

It's Heavy inspired by @vhost@ middleware from @wai-extra@ but 
gives some sugar to life.
-}

module Network.Wai.Middleware.Route where

import Data.List
import Data.ByteString (ByteString)
import Text.Regex.Posix ((=~))
import Control.Applicative
import Network.HTTP.Types (Method)
import Network.Wai

-- | Routing rule 
type Rule = Request -> Bool

-- | Route for dispatch
type Route = (Rule, Application)

{- | Dispatch. Seek for first route where the 'Rule' gives a positive result.
Uses 'find' instead 'filter' in @vhost@

> dispatch [
>     ((=="/") . rawPathInfo, auth . cache . Issues.get)
>   , ((=="/issues") . rawPathInfo, cache . About.handle)
>   ] defaultApp
-}
dispatch :: 
       [Route]      -- ^ List of routes
    -> Application  -- ^ Default 'Application' 
    -> Application  -- ^ Returns founded 'Application'. If not found - returns
                    -- default.
dispatch routes def req =  
    case find (\(b, _) -> b req) routes of
        Nothing -> def req
        Just (_, app) -> app req

{- | Syntax shugar for most frequently case: HTTP Method and Request path 
regex pattern.

> ("GET" &~~ "^/issues", app)
-}
(&~~) :: 
       Method       -- ^ HTTP Method 
    -> ByteString   -- ^ Request path pattern. 
    -> Rule         -- ^ Routing rule
infixl 1 &~~
method &~~ pattern = onPath method $ (=~ pattern) . rawPathInfo
    where
        onPath :: Method -> Rule -> Rule       
        onPath "*" p = p
        onPath m p = (&&) <$> (==m) . requestMethod <*> p
