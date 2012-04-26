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

-- | Rule for route. 
data Rule = 
    Get T.Text Application
  | Post T.Text Application
  | Head T.Text Application
  | Put T.Text Application
  | Delete T.Text Application
  | Trace T.Text Application
  | Connect T.Text Application
  | Options T.Text Application
  | Any T.Text Application
  | Get' T.Text Application
  | Post' T.Text Application
  | Head' T.Text Application
  | Put' T.Text Application
  | Delete' T.Text Application
  | Trace' T.Text Application
  | Connect' T.Text Application
  | Options' T.Text Application
  | Any' T.Text Application

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
    map chunk . T.split (=='/')
  where
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
-- >    , Get' "foo/" fooGetDynApp
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



