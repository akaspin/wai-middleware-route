{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | This module contains helpers for use "Yesod.Routes.Dispatch" with 
--   "Network.Wai".
--
--   This 'Middleware' uses first 'D.Piece' in path to route @HTTP@ method.
--   'D.Static' means concrete method. 'D.Dynamic' means any method.

module Network.Wai.Middleware.Route (
    -- * Routing rules
    Rule(..),
    mkRoutes,
    mkRoutes',
    mkRoute,
    
    -- * Middleware
    dispatch,
    dispatch_
) where

import Control.Applicative ((<$>), (<*>))

import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

import Network.Wai (Application, requestMethod, pathInfo)
import qualified Yesod.Routes.Dispatch as D

-- | Rule for route. Rules without single quotes (@'@) means fixed length
--   paths. And vice versa, rules with single quotes (@'@) means paths with
--   variable lengh 
--
--   Paths converts to 'D.Piece's by following rules:
--   
--   * Paths splits by slashes (@/@).
--   
--   * Text between slashes becomes 'D.Static' 'D.Piece'. The same thing 
--     happens with the text at the ends of paths.
--
--   * Hashes (@#@) inside slashes becomes 'D.Dynamic' 'D.Piece's.
--
--   * To make route with variable length just add asterisk (@\*@) after last
--     slash.
--
-- > "foo"
-- > [Static "foo"] Fixed
-- > 
-- > "foo/bar"
-- > [Static "foo", Static "bar"] Fixed
-- > 
-- > "foo/#/bar"
-- > [Static "foo", Dynamic, Static "bar"] Fixed
-- > 
-- > "foo/#/bar/baz/*"
-- > [Dynamic, Static "foo", Dynamic, Static "bar", Static "baz"] Variable

data Rule = 
    Get T.Text Application          -- ^ @GET@ method
  | Post T.Text Application         -- ^ @POST@ method
  | Head T.Text Application         -- ^ @HEAD@ method
  | Put T.Text Application          -- ^ @PUT@ method
  | Delete T.Text Application       -- ^ @DELETE@ method
  | Trace T.Text Application        -- ^ @TRACE@ method
  | Connect T.Text Application      -- ^ @CONNECT@ method
  | Options T.Text Application      -- ^ @OPTIONS@ method
  | Any T.Text Application          -- ^ Any @HTTP@ method
  | Gen T.Text T.Text Application
        -- ^ Generic rule with @HTTP@ method and path

-- | Make 'D.Route's from 'Rules'. 
--   
--   Equivalent @map mkRoute@ 
mkRoutes :: 
       [Rule]
            -- ^ Routing rules 
    -> [D.Route Application]
mkRoutes = map mkRoute
{-# INLINE mkRoutes #-} 

-- | Make 'D.Dispatch's from 'Rules'. 
--   
--   Equivalent @toDispatch . mkRoutes@ 
mkRoutes' :: [Rule] -> D.Dispatch Application
mkRoutes' = D.toDispatch . mkRoutes

-- | Make 'D.Route' from 'Rule'. 'D.rhPieces' of 'D.Route' will be
--   prepended with 'D.Piece' with corresponding @HTTP@ method. 
--   'D.Static' means concrete method. 'D.Dynamic' means any method.
--
-- > mkRoute $ Get "foo/bar" app
-- > Route [Static "foo", Static "bar"] False (const $ Just app) 
mkRoute :: Rule -> D.Route Application
mkRoute (Get p a) = mkGenRoute (D.Static "GET") p a
mkRoute (Post p a) = mkGenRoute (D.Static "POST") p a
mkRoute (Head p a) = mkGenRoute (D.Static "HEAD") p a
mkRoute (Put p a) = mkGenRoute (D.Static "PUT") p a
mkRoute (Delete p a) = mkGenRoute (D.Static "DELETE") p a
mkRoute (Trace p a) = mkGenRoute (D.Static "TRACE") p a
mkRoute (Connect p a) = mkGenRoute (D.Static "CONNECT") p a
mkRoute (Options p a) = mkGenRoute (D.Static "OPTIONS") p a
mkRoute (Any p a) = mkGenRoute D.Dynamic p a
mkRoute (Gen m p a) = mkGenRoute (D.Static m) p a
{-# INLINE mkRoute #-} 

-- | Make generic route
mkGenRoute :: 
       D.Piece      -- ^ Method piece. 'D.Dynamic' means any method. 
    -> T.Text         -- ^ Path pieces
    -> Application  -- ^ Routed application
    -> D.Route Application
mkGenRoute m path =
    let (!pieces, !hasMulti) = mkPieces path in
    D.Route (m:pieces) hasMulti . const . Just 
{-# INLINE mkGenRoute #-}

-- | Parse 'T.Text' and make tuple with 'D.Piece's and 'D.hasMulti'. 
--   
-- > ""         -- ([], False)
-- > "*"        -- ([], True)
-- > "foo/#"    -- ([Static "foo", Dynamic], False)  
-- > "foo/#/*"  -- ([Static "foo", Dynamic], True)  
mkPieces :: 
       T.Text               -- ^ Path to parse 
    -> ([D.Piece], Bool)    -- ^ list of 'D.Piece's and 'D.hasMulti'
mkPieces "" = ([], False) 
mkPieces !t = 
    if T.last t == '*' 
        then (prep . T.init $ t, True)
        else (prep t, False)
  where
    prep = map chunk . filter (/="") . T.split (=='/')
    {-# INLINE prep #-} 
    -- | Convert chunk
    chunk "#" = D.Dynamic
    chunk c = D.Static c
    {-# INLINE chunk #-} 
{-# INLINE mkPieces #-} 

-----------------------------------------------------------------------------
-- Middleware.
-----------------------------------------------------------------------------

-- | Dispatch 'Middleware'. 
-- 
-- > rs :: Dispatch Application
-- > rs = toDispatch . mkRoutes [
-- >      Get  "foo"  fooGetApp
-- >    , Post "foo"  fooPostApp
-- >    , Get "foo//bar" fooDynBarApp
-- > 
-- >    , Any  "any"  anyMethodApp
-- >    ]
-- > 
-- > app :: Application
-- > app = dispatch True rs (error "Not dispatched")
dispatch :: 
       Bool 
            -- ^ Squash empty 'pathInfo' chunks. It often appear in the 
            --   presence of double slashes or \"Ending slash\" in URL.
    -> D.Dispatch Application   
            -- ^ Dispatch function. 
            --   Use 'D.toDispatch' and route helpers.
    -> Application 
            -- ^ Default (@404@) application. 
    -> Application
dispatch squash mappings defApp req = 
    case mappings . needle $ req of
        Nothing -> defApp req
        (Just app) -> app req 
  where
    needle = (:) <$> decodeUtf8 . requestMethod <*> path squash
    path False = pathInfo
    path True = filter (/="") . pathInfo

-- | Dispatch 'Middleware' with auto-squash empty path pieces. Equiwalent to
-- > dispatch True
dispatch_ ::
       D.Dispatch Application   
            -- ^ Dispatch function. 
            --   Use 'D.toDispatch' and route helpers.
    -> Application 
            -- ^ Default (@404@) application. 
    -> Application
dispatch_ = dispatch True



