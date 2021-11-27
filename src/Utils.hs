{-# LANGUAGE CPP #-}
module Utils where

import Data.Coerce
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Control.Monad.IO.Class
import Data.JSString.Text
import Data.Text as T
import GHCJS.Foreign.Callback as GHCJS
import GHCJS.Types
import HtmlT
import JavaScript.Object.Internal
import qualified JavaScript.Object as Object
import qualified JavaScript.Web.Location as JS
import JavaScript.Array

mkRouteRef :: MonadReactive m => m (DynRef Text)
mkRouteRef = do
  initial <- liftIO readUrl
  routeRef <- newRef initial
  win <- liftIO getCurrentWindow
  popStateCb <- liftIO $ asyncCallback do
    newUrl <- readUrl
    existingUrl <- readRef routeRef
    when (newUrl /= existingUrl) $ writeRef routeRef newUrl
  liftIO $ Object.setProp "onpopstate" (jsval popStateCb) (coerce win)
  return routeRef {dynref_modifier = modifyUrl routeRef}
  where
    readUrl = do
      loc <- JS.getWindowLocation
      href <- JS.getHash loc
      return $ T.dropWhile (=='#') $ textFromJSString href
    writeUrl href = do
      loc <- JS.getWindowLocation
      let finalHash = if href == "" then href else "#" <> href
      JS.setHash (textToJSString finalHash) loc
    modifyUrl routeRef f = do
      old <- readRef routeRef
      let new = f old
      liftIO $ writeUrl new
      modifyRef routeRef (const new)

forkHtml :: IO () -> Html ThreadId
forkHtml act = do
  threadId <- liftIO $ forkIO act
  addFinalizer (killThread threadId)
  return threadId

foreign import javascript unsafe
  "performance.now()"
  js_performanceNow :: IO Double

foreign import javascript unsafe
  "$1.children[$2]"
  js_children :: DOMElement -> Int -> IO DOMNode

foreign import javascript unsafe
  "$1.querySelectorAll($2)"
  js_querySelectorAll :: DOMElement -> JSString -> IO JSArray
