{-# LANGUAGE OverloadedStrings #-}

{- | Route middleware for wai.

It's Heavy inspired by @vhost@ middleware from @wai-extra@ but 
gives some sugar to life.
-}

module Network.Wai.Middleware.Route (
    -- * Dispatching
    dispatch, 
    -- * Creating rules
    rule
) where

import Data.List (find)
import Data.ByteString (ByteString)
import Text.Regex.Posix ((=~))
import Control.Applicative ((<$>), (<*>))
import Network.HTTP.Types (Method)
import Network.Wai (Request, Application, rawPathInfo, requestMethod)

{- | Dispatch. Seek for first route where the \"rule\" gives a positive result.
Uses 'find' instead 'filter' in @vhost@

> dispatch [
>     ((=="/") . rawPathInfo, auth . cache . Issues.get)
>   , ((=="/issues") . rawPathInfo, cache . About.handle)
>   ] defaultApp
-}
dispatch :: 
       [(Request -> Bool, Application)]      
                    -- ^ List of routes
    -> Application  -- ^ Default 'Application' 
    -> Application  -- ^ Returns founded 'Application'. If not found - returns
                    --   default.
dispatch routes def req =  
    case find (\(b, _) -> b req) routes of
        Nothing -> def req
        Just (_, app) -> app req

{- | Syntax shugar for most frequently case: HTTP Method and Request path 
regex pattern.

> ("*" `rule` "^/issues/any", app)
> (methodGet `rule` "^/issues", anotherApp)
-}
rule :: 
       Method       -- ^ HTTP Method. Use @\"*\"@ for any method
    -> ByteString   -- ^ Request path pattern. 
    -> Request      -- ^ Request 
    -> Bool         -- ^ Routing rule
rule method pattern = onPath method $ (=~ pattern) . rawPathInfo
    where
        onPath :: Method -> (Request -> Bool) -> Request -> Bool
        onPath "*" p = p
        onPath m p = (&&) <$> (==m) . requestMethod <*> p
