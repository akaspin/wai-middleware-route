{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

{- | Route middleware for wai.

It's Heavy inspired by @vhost@ middleware from @wai-extra@ but 
gives some sugar to life.
-}

module Network.Wai.Middleware.Route (
    -- * Dispatching
    dispatch, 
    -- * Creating rules
    onRegex,
    onPrefix,
    onExact
) where

import Data.List (find)
import Data.ByteString (ByteString, isPrefixOf)
import Text.Regex.Posix ((=~))
import Control.Applicative ((<$>), (<*>))
import Network.HTTP.Types (Method)
import Network.Wai (Request(..), Application)

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
        Just (_, app) -> app req
        Nothing -> def req

-- | Most frequently case: HTTP Method and Request path regex pattern.
--
-- > ("*" `rule` "^/issue/", app)
-- > (methodGet `rule` "^/issues$", anotherApp)
onRegex :: 
       Method       -- ^ HTTP Method. Use @\"*\"@ for any method
    -> ByteString   -- ^ Request path pattern. 
    -> Request      -- ^ Request 
    -> Bool
onRegex method needle = 
    withMethod method $ (=~ needle) . rawPathInfo
    
-- | HTTP Method and Request path prefix.
--
-- > ("*" `rule` "^/issue/", app)
-- > (methodGet `rule` "^/issues$", anotherApp)
onPrefix :: 
       Method       -- ^ HTTP Method. Use @\"*\"@ for any method
    -> ByteString   -- ^ Request path prefix. 
    -> Request      -- ^ Request 
    -> Bool         -- ^ Routing rule
onPrefix method needle = 
    withMethod method $ (needle `isPrefixOf`) . rawPathInfo

-- | HTTP Method and Request path.
--
-- > ("*" `rule` "^/issue/", app)
-- > (methodGet `rule` "^/issues$", anotherApp)
onExact :: 
       Method       -- ^ HTTP Method. Use @\"*\"@ for any method
    -> ByteString   -- ^ Request path. 
    -> Request      -- ^ Request 
    -> Bool         -- ^ Routing rule
onExact method needle = 
    withMethod method $ (needle ==) . rawPathInfo

withMethod :: 
       Method
    -> (Request -> Bool) 
    -> Request 
    -> Bool
withMethod "*" p = p
withMethod m p = (&&) <$> (== m) . requestMethod <*> p
