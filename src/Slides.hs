module Slides where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Text as T
import Data.List as L
import HtmlT
import Text.Read
import Debug.Trace
import qualified Data.Set as Set
import Data.Maybe
import Data.Bool
import qualified JavaScript.Array as JS
import Data.Coerce

import "this" Life
import "this" Utils

type SlideNum = Int

type SlideStep = Int

slidesWidget :: (SlideNum, SlideStep) -> Html ()
slidesWidget = \case
  (0, s) -> do
    div_ [class_ "Slide-1"] do
      h1_ "HtmlT — Lightweight frontend library for GHCJS"
      h2_ "Introduction"
      ul_ [class_ "Slide-1-contents"] do
        li_ $ a_ [href_ "#1-1"] "Table of Contents"
        li_ $ a_ [href_ "#2-1"] "Motivation for HtmlT"
        li_ $ a_ [href_ "#3-1"] "Advantages"
        li_ $ a_ [href_ "#5-1"] "Main Ideas Behind Implementation"
        li_ $ a_ [href_ "#6-1"] "Events System Implementation"
        li_ $ a_ [href_ "#4-1"] "Comparison with Reflex"
      div_ [class_ "Slide-1-legend"] do
        shortcuts
          [ ("Step forward", ["→", "Space"])
          , ("Step backward", ["←", "Backspace"])
          , ("To the last slide", ["↑", "End"])
          , ("To the first slide", ["↓", "Home"])
          , ("Next slide", ["Ctrl + →", "Ctrl + Space"])
          , ("Previous slide", ["Ctrl + ←", "Ctrl + Backspace"])
          ]
      div_ [class_ "Slide-1-links"] do
        div_ "Vladislav Lagunov"
        div_ "November 2021"
        div_ $ a_ [href_ "https://github.com/lagunoff/htmlt"]
          "https://github.com/lagunoff/htmlt"
  (1, s) -> do
    h1_ "Motivation for HtmlT"
    div_ [class_ "Content"] do
      div_ [class_ "Slide2-Background"] do
        img_ [src_ "assets/javascript-frameworks.jpg"] blank
      ul_ [class_ "BulletedList"] do
        when (s > 0) $ li_ "Same language on both sides"
        when (s > 1) $ li_ "Small and readable codebase"
        when (s > 2) $ li_ "No dependencies"
        when (s > 3) $ li_ "Faster builds, faster development cycles"
  (2, s) -> do
    h1_ do
      span_ [class_ "SlideNumber"] "2"
      text "Main Features"
    div_ [class_ "Content"] do
      ul_ [class_ "BulletedList"] do
        when (s > 0) $ li_ "dskjlsjfknsdlj"
        when (s > 1) $ li_ "sdhflsfblsdhjl"
  (3, s) -> do
    h1_ do
      span_ [class_ "SlideNumber"] "3"
      text "Game Of Life"
    lifeStateRef <- newRef $ Set.fromList gospelGun
    threadStateRef <- newRef Nothing
    fpsRef <- newRef 0
    div_ [class_ "Content"] do
      div_ [class_ "Life"] do
        forM_ [miny..maxy] \y ->
          forM_ [minx..maxx] \x ->
             div_ do
               toggleClass "Alive" $ Set.member (x, y) <$> fromRef lifeStateRef
               style_ $ T.pack $ "top: " <> show (y * 4) <> "px; left: " <> show (x * 4) <> "px"
      button_ do
        "Next Gen"
        onOptions "click" defaultListenerOpts {lo_sync_callback = True} $ const $ modifyRef lifeStateRef $ nextGen bounds
      button_ do
        dynText $ bool "Run" "Stop" . isJust <$> fromRef threadStateRef
        on_ "click" $ handleRun threadStateRef lifeStateRef fpsRef
      p_ do
        "Cells alive:"
        dynText $ T.pack . show . Set.size <$> fromRef lifeStateRef
        ",  FPS:"
        dynText $ T.pack . show <$> fromRef fpsRef
  (4, s) -> do
    h1_ do
      span_ [class_ "SlideNumber"] "4"
      text "Faster Game Of Life"
    lifeStateRef <- newRef $ Set.fromList gospelGun
    threadStateRef <- newRef Nothing
    fpsRef <- newRef 0
    div_ [class_ "Content"] do
      lifeEl <- div_ [class_ "Life"] do
        forM_ [miny..maxy] \y ->
          forM_ [minx..maxx] \x ->
             div_ do
               style_ $ T.pack $ "top: " <> show (y * 4) <> "px; left: " <> show (x * 4) <> "px"
        asks html_current_element
      forDyn_ (fromRef lifeStateRef) $ liftIO . updateDomElements lifeEl
      button_ do
        "Next Gen"
        onOptions "click" defaultListenerOpts {lo_sync_callback = True} $ const $ modifyRef lifeStateRef $ nextGen bounds
      button_ do
        dynText $ bool "Run" "Stop" . isJust <$> fromRef threadStateRef
        on_ "click" $ handleRun threadStateRef lifeStateRef fpsRef
      p_ do
        "Cells alive:"
        dynText $ T.pack . show . Set.size <$> fromRef lifeStateRef
        ",  FPS:"
        dynText $ T.pack . show <$> fromRef fpsRef
  _ -> slidesWidget (0, 0)
  where
    bounds@(minx, maxx, miny, maxy) = (0, 127, 0, 127)
    gospelGun = [(1, 5),(1, 6),(2, 5),(2, 6),(11, 5),(11, 6),(11, 7),(12, 4),(12, 8),(13, 3),(13, 9),(14, 3),(14, 9),(15, 6),(16, 4),(16, 8),(17, 5),(17, 6),(17, 7),(18, 6),(21, 3),(21, 4),(21, 5),(22, 3),(22, 4),(22, 5),(23, 2),(23, 6),(25, 1),(25, 2),(25, 6),(25, 7),(35, 3),(35, 4),(36, 3),(36, 4)]
    shortcuts :: [(Html (), [Html ()])] -> Html ()
    shortcuts = table_ . tbody_ . mapM_ shortcutRow
    shortcutRow (title, keys) = tr_ do
      td_ title
      td_ . sequence_ . L.intersperse (text ", ") . fmap (span_ [class_ "Shortcut"]) $ keys

slideStepLengths :: [Int]
slideStepLengths = [1, 5, 3, 1, 1]

updateDomElements :: DOMElement -> Set.Set (Int, Int) -> IO ()
updateDomElements lifeEl lifeState = do
  childrenAlife <- js_querySelectorAll lifeEl ".Alive"
  forM_ [0.. JS.length childrenAlife - 1] \idx -> do
    let node = childrenAlife JS.! idx
    js_classListRemove (coerce node) "Alive"
  forM_ lifeState \(x, y) -> do
    let pos = y * 128 + x
    node <- js_children lifeEl pos
    js_classListAdd (coerce node) "Alive"

handleRun
  :: DynRef (Maybe ThreadId)
  -> DynRef (Set.Set (Int, Int))
  -> DynRef Double
  -> Html ()
handleRun threadStateRef lifeStateRef fpsRef =
  readRef threadStateRef >>= \case
    Just threadId -> do
      liftIO $ killThread threadId
      writeRef threadStateRef Nothing
    Nothing -> do
      let
        go t0 = do
          modifyRef lifeStateRef $ nextGen bounds
          t1 <- liftIO js_performanceNow
          writeRef fpsRef $ fromIntegral (round ((1000.0 / (t1 - t0)) * 100)) / 100
          threadDelay 0
          go t1
      t <- liftIO js_performanceNow
      threadId <- forkHtml (go t)
      writeRef threadStateRef (Just threadId)
  where
    bounds@(minx, maxx, miny, maxy) = (0, 127, 0, 127)

slidesStyles :: Text
slidesStyles = "\
  \.Slide h1 {\
  \  margin: 0;\
  \  color: rgba(0,0,0,0.87);\
  \  font-weight: 400;\
  \  font-size: 35px;\
  \  text-align: center;\
  \}\
  \.BulletedList {\
  \  list-style: none;\
  \  padding: 0;\
  \  font-size: 24px;\
  \}\
  \.BulletedList li:before {\
  \  content: \"➢\";\
  \  margin: 0 12px;\
  \  font-size: 1.3em;\
  \}\
  \.SlideNumber {\
  \  display: inline-block;\
  \  width: 42px;\
  \  height: 42px;\
  \  border-radius: 50%;\
  \  margin-right: 16px;\
  \  border: solid 2px black;\
  \  text-align: center;\
  \}\
  \.Life {\
  \  width: 510px;\
  \  height: 510px;\
  \  position: relative;\
  \}\
  \.Life > div {\
  \  width: 3px;\
  \  height: 3px;\
  \  position: absolute;\
  \  background: rgba(0,0,0,0.08);\
  \}\
  \.Life > div.Alive {\
  \  background: black;\
  \}\
  \.Slide-1 h1 {\
  \  margin-top: 24px;\
  \  text-align: center;\
  \  font-size: 24px;\
  \  font-weight: 400;\
  \}\
  \.Slide-1 h2 {\
  \  margin: 24px 0 0 0;\
  \  text-align: center;\
  \  font-size: 48px;\
  \  font-weight: 400;\
  \}\
  \.Slide-1 h2 {\
  \  margin: 24px 0 0 0;\
  \  text-align: center;\
  \  font-size: 48px;\
  \  font-weight: 400;\
  \}\
  \.Slide-1-links {\
  \  position: absolute;\
  \  right: 24px;\
  \  bottom: 16px;\
  \  text-align: right;\
  \}\
  \.Slide-1-legend {\
  \  position: absolute;\
  \  left: 24px;\
  \  bottom: 16px;\
  \  line-height: 1.5em;\
  \  font-size: 14px;\
  \}\
  \.Slide-1-legend table td:nth-child(1) {\
  \  text-align: right;\
  \  padding-right: 16px;\
  \}\
  \.Shortcut {\
  \  border: solid 1px rgba(0,0,0,0.2);\
  \  border-radius: 3px;\
  \  padding: 2px 6px;\
  \  min-width: 18px;\
  \  text-align: center;\
  \  font-family: monospace;\
  \}\
  \.Slide-1-contents {\
  \  list-style: deceimal;\
  \}\
  \.Slide2-Background {\
  \  right: 24px;\
  \  top: 110px;\
  \  position: absolute;\
  \}\
  \.Slide2-Background img {\
  \  filter: opacity(0.6);\
  \}"
