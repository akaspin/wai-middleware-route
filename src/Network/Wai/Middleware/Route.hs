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
    dispatch
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
--   * Double (triple, etc.) slashes means becomes 'D.Dynamic' 'D.Piece's. 
--     The same thing happens with the slashes at the ends of paths.
--
-- > "foo"
-- > [Static "foo"]
-- > 
-- > "foo/bar"
-- > [Static "foo", Static "bar"]
-- > 
-- > "foo//bar"
-- > [Static "foo", Dynamic, Static "bar"]
-- > 
-- > "/foo//bar/baz/"
-- > [Dynamic, Static "foo", Dynamic, Static "bar", Static "baz", Dynamic]
-- > 

data Rule = 
    Get T.Text Application
        -- ^ @GET@,  fixed length path
  | Post T.Text Application
        -- ^ @POST@, fixed length path
  | Head T.Text Application
        -- ^ @HEAD@, fixed length path
  | Put T.Text Application
        -- ^ @PUT@, fixed length path
  | Delete T.Text Application
        -- ^ @DELETE@, fixed length path
  | Trace T.Text Application
        -- ^ @TRACE@, fixed length path
  | Connect T.Text Application
        -- ^ @CONNECT@, fixed length path
  | Options T.Text Application
        -- ^ @OPTIONS@, fixed length path
  | Any T.Text Application
        -- ^ Any @HTTP@ method, fixed length path
  | Get' T.Text Application
        -- ^ @GET@, variable length path
  | Post' T.Text Application
        -- ^ @POST@, variable length path
  | Head' T.Text Application
        -- ^ @HEAD@, variable length path
  | Put' T.Text Application
        -- ^ @PUT@, variable length path
  | Delete' T.Text Application
        -- ^ @DELETE@, variable length path
  | Trace' T.Text Application
        -- ^ @TRACE@, variable length path
  | Connect' T.Text Application
        -- ^ @CONNECT@, variable length path
  | Options' T.Text Application
        -- ^ @OPTIONS@, variable length path
  | Any' T.Text Application
        -- ^ Any @HTTP@ method, variable length path
  | Gen Bool T.Text T.Text Application
        -- ^ Generic rule with path lenghts flag, @HTTP@ method and path

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
mkRoute (Get p a) = mkGenRoute (D.Static "GET") False p a
mkRoute (Post p a) = mkGenRoute (D.Static "POST") False p a
mkRoute (Head p a) = mkGenRoute (D.Static "HEAD") False p a
mkRoute (Put p a) = mkGenRoute (D.Static "PUT") False p a
mkRoute (Delete p a) = mkGenRoute (D.Static "DELETE") False p a
mkRoute (Trace p a) = mkGenRoute (D.Static "TRACE") False p a
mkRoute (Connect p a) = mkGenRoute (D.Static "CONNECT") False p a
mkRoute (Options p a) = mkGenRoute (D.Static "OPTIONS") False p a
mkRoute (Any p a) = mkGenRoute D.Dynamic False p a
mkRoute (Get' p a) = mkGenRoute (D.Static "GET") True p a
mkRoute (Post' p a) = mkGenRoute (D.Static "POST") True p a
mkRoute (Head' p a) = mkGenRoute (D.Static "HEAD") True p a
mkRoute (Put' p a) = mkGenRoute (D.Static "PUT") True p a
mkRoute (Delete' p a) = mkGenRoute (D.Static "DELETE") True p a
mkRoute (Trace' p a) = mkGenRoute (D.Static "TRACE") True p a
mkRoute (Connect' p a) = mkGenRoute (D.Static "CONNECT") True p a
mkRoute (Options' p a) = mkGenRoute (D.Static "OPTIONS") True p a
mkRoute (Any' p a) = mkGenRoute D.Dynamic True p a
mkRoute (Gen v m p a) = mkGenRoute (D.Static m) v p a
{-# INLINE mkRoute #-} 

-- | Make generic route
mkGenRoute :: 
       D.Piece      -- ^ Method piece. 'D.Dynamic' means any method. 
    -> Bool         -- ^ 'D.rhHasMulti'
    -> T.Text         -- ^ Path pieces
    -> Application  -- ^ Routed application
    -> D.Route Application
mkGenRoute m hasMulti pieces = 
    D.Route (m:mkPieces pieces) hasMulti . const . Just 
{-# INLINE mkGenRoute #-}

-- | Make Pieces from path
mkPieces :: T.Text -> [D.Piece]
mkPieces = 
    map chunk . protPath . T.split (=='/')
  where
    protPath [""] = []
    protPath p = p
    chunk "" = D.Dynamic
    chunk c = D.Static c
{-# INLINE mkPieces #-}

-----------------------------------------------------------------------------
-- Middleware.
-----------------------------------------------------------------------------

-- | Dispatch function.
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
-- > app = dispatch rs (error "Not dispatched")

dispatch :: 
       D.Dispatch Application   
            -- ^ Dispatch function. 
            --   Use 'D.toDispatch' and route helpers below.
    -> Application 
            -- ^ Default (@404@) application. 
    -> Application
dispatch mappings defApp req = 
    case mappings . needle $ req of
        Nothing -> defApp req
        (Just app) -> app req 
  where
    needle = (:) <$> decodeUtf8 . requestMethod <*> pathInfo



