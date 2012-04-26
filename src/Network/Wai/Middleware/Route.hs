{-# LANGUAGE OverloadedStrings #-}

-- | This module contains helpers for use "Yesod.Routes.Dispatch" with 
--   "Network.Wai".
--
--   This 'Middleware' uses first 'D.Piece' in path to route @HTTP@ method.
--   'D.Static' means concrete method. 'D.Dynamic' means any method.

module Network.Wai.Middleware.Route (

    -- * Middleware
    dispatch,

    -- * Route helpers #route_helpers#
    -- $route_helpers
    
    -- ** Simple paths
    -- $simple_paths
    
    -- *** Fixed length
    sGET, sPOST, sHEAD, sPUT, sDELETE, sTRACE, sCONNECT, sOPTIONS,
    sANY,
    -- *** Variable length
    mGET, mPOST, mHEAD, mPUT, mDELETE, mTRACE, mCONNECT, mOPTIONS,
    mANY, 
    
    -- ** Native paths
    -- *** Fixed length
    sGET', sPOST', sHEAD', sPUT', sDELETE', sTRACE', sCONNECT', sOPTIONS',
    sANY', sRoute,
    -- *** Variable length
    mGET', mPOST', mHEAD', mPUT', mDELETE', mTRACE', mCONNECT', mOPTIONS',
    mANY', mRoute,
    -- * 'D.Piece' helper
    mkP
) where

import Control.Applicative ((<$>), (<*>))

import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

import Network.Wai (Application, requestMethod, pathInfo)
import qualified Yesod.Routes.Dispatch as D

-----------------------------------------------------------------------------
-- Docs chunks
-----------------------------------------------------------------------------

-- $route_helpers
-- Functions below simplify process of creating 'D.Route's. Each helper 
-- prepends corresponding @HTTP@ method to path.

-- $simple_paths
-- All functions below is combinations of native path helpers and 'mkP'.

-----------------------------------------------------------------------------
-- Middleware.
-----------------------------------------------------------------------------

-- | Dispatch function.
-- 
-- > rs :: Dispatch Application
-- > rs = toDispatch [
-- >      -- simple paths 
-- >      sGET  "foo"  fooGetApp
-- >    , sPOST "foo"  fooPostApp
-- >    , sGET  "foo/" fooGetDynApp
-- > 
-- >      -- native paths
-- >    , sGET'  [Static "bar", Dynamic] barGetDynApp
-- >
-- >      -- simple paths, any method 
-- >    , sANY  "any"  anyMethodApp
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

-----------------------------------------------------------------------------
-- 'D.Route' helpers for simple paths.
-----------------------------------------------------------------------------

-- | 'D.Route' helpers for concrete @HTTP@ methods with fixed-length 
--   simple path.
sGET, sPOST, sHEAD, sPUT, sDELETE, sTRACE, sCONNECT, sOPTIONS :: 
       T.Text       -- ^ Path
    -> Application  -- ^ Routable application 
    -> D.Route Application
sGET = sGET' . mkP
sPOST = sPOST' . mkP
sHEAD = sHEAD' . mkP
sPUT = sPUT' . mkP
sDELETE = sDELETE' . mkP
sTRACE = sTRACE' . mkP
sCONNECT = sCONNECT' . mkP
sOPTIONS = sOPTIONS' . mkP

-- | 'D.Route' helper for any @HTTP@ method with fixed-length simple path.
sANY :: 
       T.Text       -- ^ Path
    -> Application  -- ^ Routable application 
    -> D.Route Application
sANY = sRoute D.Dynamic . mkP

-- | 'D.Route' helpers for concrete @HTTP@ methods with vary-length 
--   simple path.
mGET, mPOST, mHEAD, mPUT, mDELETE, mTRACE, mCONNECT, mOPTIONS :: 
       T.Text       -- ^ Path
    -> Application  -- ^ Routable application 
    -> D.Route Application
mGET = mGET' . mkP
mPOST = mPOST' . mkP
mHEAD = mHEAD' . mkP
mPUT = mPUT' . mkP
mDELETE = mDELETE' . mkP
mTRACE = mTRACE' . mkP
mCONNECT = mCONNECT' . mkP
mOPTIONS = mOPTIONS' . mkP

-- | 'D.Route' helper for any @HTTP@ method with vary-length simple path.
mANY :: 
       T.Text       -- ^ Path
    -> Application  -- ^ Routable application 
    -> D.Route Application
mANY = mRoute D.Dynamic . mkP

-----------------------------------------------------------------------------
-- 'D.Route' helpers for native 'D.Piece' paths (Fixed-length)
-----------------------------------------------------------------------------

-- | 'D.Route' helpers for concrete @HTTP@ methods with fixed-length native 
--   @yesod-routes@ path.
sGET', sPOST', sHEAD', sPUT', sDELETE', sTRACE', sCONNECT', sOPTIONS' :: 
       [D.Piece]        -- ^ Path
    -> Application      -- ^ Routable application
    -> D.Route Application
sGET' = sRoute (D.Static "GET")
sPOST' = sRoute (D.Static "POST")
sHEAD' = sRoute (D.Static "HEAD")
sPUT' = sRoute (D.Static "PUT")
sDELETE' = sRoute (D.Static "DELETE")
sTRACE' = sRoute (D.Static "TRACE")
sCONNECT' = sRoute (D.Static "CONNECT")
sOPTIONS' = sRoute (D.Static "OPTIONS")

-- | 'D.Route' helper for any @HTTP@ method with fixed-length native 
--   @yesod-routes@ path.
sANY' :: 
       [D.Piece]        -- ^ Path
    -> Application      -- ^ Routable application
    -> D.Route Application
sANY' = sRoute D.Dynamic

-- | Generalized 'D.Route' helper for fixed-length native 
--   @yesod-routes@ path.
sRoute ::  
       D.Piece      -- ^ Method piece. 'D.Dynamic' means any method. 
    -> [D.Piece]    -- ^ Path pieces
    -> Application  -- ^ Routed application
    -> D.Route Application
sRoute = (`defRoute` False)

-----------------------------------------------------------------------------
-- Generalized 'D.Route' helpers for native 'D.Piece' paths
-----------------------------------------------------------------------------

-- | 'D.Route' helpers for concrete @HTTP@ methods with vary-length native 
--   @yesod-routes@ path.
mGET', mPOST', mHEAD', mPUT', mDELETE', mTRACE', mCONNECT', mOPTIONS' :: 
       [D.Piece]        -- ^ Path
    -> Application      -- ^ Routable application
    -> D.Route Application
mGET' = mRoute (D.Static "GET")
mPOST' = mRoute (D.Static "POST")
mHEAD' = mRoute (D.Static "HEAD")
mPUT' = mRoute (D.Static "PUT")
mDELETE' = mRoute (D.Static "DELETE")
mTRACE' = mRoute (D.Static "TRACE")
mCONNECT' = mRoute (D.Static "CONNECT")
mOPTIONS' = mRoute (D.Static "OPTIONS")

-- | 'D.Route' helper for any @HTTP@ method with fixed-length native 
--   @yesod-routes@ path.
mANY' :: 
       [D.Piece]        -- ^ Path
    -> Application      -- ^ Routable application
    -> D.Route Application
mANY' = sRoute D.Dynamic

-- | Generalized 'D.Route' helper for vary-length native 
--   @yesod-routes@ path.
mRoute ::  
       D.Piece      -- ^ Method piece. 'D.Dynamic' means any method. 
    -> [D.Piece]    -- ^ Path pieces
    -> Application  -- ^ Routed application
    -> D.Route Application
mRoute = (`defRoute` True)

-----------------------------------------------------------------------------
-- 'D.Piece' helpers
-----------------------------------------------------------------------------

-- | Make 'D.Piece's from 'T.Text'. Splits path on slashes. Dual slashes means
--   'D.Dynamic' 'D.Piece's.
--
-- > mkP ""             -- []
-- > mkP "foo/bar"      -- [Static "foo", Static "bar"]
-- > mkP "foo//bar/"    -- [Static "foo", Dynamic, Static "bar", Dynamic]
mkP :: T.Text -> [D.Piece]
mkP = 
    map chunk . T.split (=='/')
  where
    chunk "" = D.Dynamic
    chunk c = D.Static c

-------------------------------------------------------------------------------
---- Internal
-------------------------------------------------------------------------------

defRoute :: 
       D.Piece      -- ^ Method piece. 'D.Dynamic' means any method. 
    -> Bool         -- ^ 'D.rhHasMulti'
    -> [D.Piece]    -- ^ Path pieces
    -> Application  -- ^ Routed application
    -> D.Route Application
defRoute m hasMulti pieces = 
    D.Route (m:pieces) hasMulti . const . Just 

